module Main where

import Conduit
import Control.Lens
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty qualified as AEP
import Data.Aeson.Lens
import Data.ByteString.Base64 (encodeBase64)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client.Conduit (defaultManagerSettings)
import Network.HTTP.Conduit qualified as HTTP
import Options.Generic qualified as OG
import System.FilePath
import UnliftIO.Directory

data CLI = CLI
  { downloadUrlPrefix :: Text,
    artifactDir :: FilePath OG.<!> "artifacts",
    metadataPath :: FilePath OG.<!> "meta.json"
  }
  deriving stock (Show, Generic)

instance OG.ParseRecord CLI where
  parseRecord = OG.parseRecordWithModifiers OG.lispCaseModifiers

main :: IO ()
main = do
  for_ [stdout, stderr] $ flip hSetBuffering LineBuffering
  CLI
    { artifactDir = OG.DefValue artifactDir,
      metadataPath = OG.DefValue metadataPath,
      ..
    } <-
    OG.getRecord "mirror various GHC WASM bindists"
  mgr <- HTTP.newManager defaultManagerSettings
  bindists <- loadStoredBindists metadataPath
  bindists <- updateStoredBindists mgr downloadUrlPrefix artifactDir bindists
  saveStoredBindists metadataPath bindists

type Url = Text -- such typesafe much wow

data StoredBindist = StoredBindist
  { mirrorUrl :: Url,
    originalUrl :: Url,
    sriHash :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (A.FromJSON, A.ToJSON)

type StoredBindists = Map Text StoredBindist

loadStoredBindists :: FilePath -> IO StoredBindists
loadStoredBindists path =
  doesFileExist path >>= \case
    True -> either fail pure =<< A.eitherDecodeFileStrict' path
    False -> pure Map.empty

saveStoredBindists :: FilePath -> StoredBindists -> IO ()
saveStoredBindists path = BL.writeFile path . AEP.encodePretty

updateStoredBindists ::
  HTTP.Manager -> Text -> FilePath -> StoredBindists -> IO StoredBindists
updateStoredBindists mgr urlPrefix bindistDir =
  Map.mergeA
    do Map.traverseMissing \name src -> updateBindist name src Nothing
    do Map.preserveMissing
    do Map.zipWithAMatched \name src -> updateBindist name src . Just
    bindistSrcs
  where
    updateBindist :: Text -> BindistSrc -> Maybe StoredBindist -> IO StoredBindist
    updateBindist bindistName bindistSrc prevBindist = do
      putTextLn $ "checking " <> bindistName
      originalUrl <- getLatestBindistURL mgr bindistSrc
      case prevBindist of
        Just bindist | bindist.originalUrl == originalUrl -> do
          putTextLn "no updates"
          pure bindist
        _ -> do
          putTextLn "updating"
          createDirectoryIfMissing True bindistDir
          let fileName = fileNameFor originalUrl
          req <- HTTP.parseUrlThrow (toString originalUrl)
          sha256 <- runConduitRes do
            res <- lift $ HTTP.http req mgr
            HTTP.responseBody res
              .| getZipSink do
                ZipSink (sinkFile (bindistDir </> toString fileName))
                  *> ZipSink sinkSha256
          pure
            StoredBindist
              { mirrorUrl = urlPrefix <> fileName,
                originalUrl = originalUrl,
                sriHash = "sha256-" <> encodeBase64 sha256
              }
      where
        sinkSha256 = SHA256.finalize <$> foldlC SHA256.update SHA256.init
        fileNameFor url = bindistName <> urlExt
          where
            urlExt = T.dropWhile (/= '.') . T.takeWhileEnd (/= '/') $ url

-- get the latest version of a bindist

data BindistSrc
  = GitLabArtifact
      { gitlabDomain :: Text,
        projectId :: Int,
        ref :: Text,
        jobName :: Text,
        artifactPath :: Text,
        pipelineFilter :: [(ByteString, Maybe ByteString)]
      }
  | GitHubArtifact
      { ownerRepo :: Text,
        branch :: Text,
        workflowName :: Text,
        artifactName :: Text,
        event :: Text
      }
  deriving stock (Show)

getLatestBindistURL :: HTTP.Manager -> BindistSrc -> IO Url
getLatestBindistURL mgr = \case
  GitLabArtifact {..} -> do
    Just url <- runConduit $ pipelineIds .| concatMapMC getPipelineJobUrl .| headC
    pure url
    where
      projectUrl = "https://" <> gitlabDomain <> "/api/v4/projects/" <> show projectId
      pipelineUrl = toString $ projectUrl <> "/pipelines"

      pipelineIds = do
        apiRes <- liftIO $ fetch pipelineUrl $ ("ref", qv ref) : pipelineFilter
        yieldMany $ apiRes ^.. values . key "id" . _Integer

      getPipelineJobUrl pipelineId = do
        apiRes <-
          fetch
            (pipelineUrl <> "/" <> show pipelineId <> "/jobs")
            [("per_page", Just "100"), ("scope", Just "success")]
        let hasJobName = filteredBy $ key "name" . _String . only jobName
            jobId = apiRes ^? values . hasJobName . key "id" . _Integer
        pure $ downloadUrlForJobId <$> jobId

      downloadUrlForJobId jobId =
        projectUrl <> "/jobs/" <> show jobId <> "/artifacts/" <> artifactPath
  GitHubArtifact {..} -> do
    let runsUrl = toString $ "https://api.github.com/repos/" <> ownerRepo <> "/actions" <> "/runs"

    apiRes <- fetch runsUrl [("branch", qv branch), ("event", qv event)]
    let hasWorkflowName = filteredBy $ key "name" . _String . only workflowName
    Just runId <-
      pure $ apiRes ^? key "workflow_runs" . values . hasWorkflowName . key "id" . _Integer

    apiRes <- fetch (runsUrl <> "/" <> show runId <> "/artifacts") []
    let hasArtifactName = filteredBy $ key "name" . _String . only artifactName
    Just artifactId <-
      pure $ apiRes ^? key "artifacts" . values . hasArtifactName . key "id" . _Integer

    pure $ "https://nightly.link/" <> ownerRepo <> "/actions/artifacts/" <> show artifactId <> ".zip"
  where
    fetch url qs = do
      req <- HTTP.setQueryString qs <$> HTTP.parseUrlThrow url
      let ua = ("User-Agent", "ghc-wasm-bindists")
      req <- pure $ req {HTTP.requestHeaders = ua : HTTP.requestHeaders req}
      HTTP.responseBody <$> HTTP.httpLbs req mgr

    qv = Just . encodeUtf8

bindistSrcs :: Map Text BindistSrc
bindistSrcs =
  Map.fromList
    [ (,)
        "wasm32-wasi-ghc-gmp"
        GitLabArtifact
          { gitlabDomain = "gitlab.haskell.org",
            projectId = 1,
            ref = "master",
            jobName = "nightly-x86_64-linux-alpine3_17-wasm-cross_wasm32-wasi-release+fully_static",
            artifactPath = "ghc-x86_64-linux-alpine3_17-wasm-cross_wasm32-wasi-release+fully_static.tar.xz",
            pipelineFilter = [("source", Just "schedule"), ("scope", Just "finished")]
          },
      (,)
        "wasm32-wasi-ghc-native"
        GitLabArtifact
          { gitlabDomain = "gitlab.haskell.org",
            projectId = 1,
            ref = "master",
            jobName = "nightly-x86_64-linux-alpine3_17-wasm-int_native-cross_wasm32-wasi-release+fully_static",
            artifactPath = "ghc-x86_64-linux-alpine3_17-wasm-int_native-cross_wasm32-wasi-release+fully_static.tar.xz",
            pipelineFilter = [("source", Just "schedule"), ("scope", Just "finished")]
          },
      (,)
        "wasm32-wasi-ghc-unreg"
        GitLabArtifact
          { gitlabDomain = "gitlab.haskell.org",
            projectId = 1,
            ref = "master",
            jobName = "nightly-x86_64-linux-alpine3_17-wasm-unreg-cross_wasm32-wasi-release+fully_static",
            artifactPath = "ghc-x86_64-linux-alpine3_17-wasm-unreg-cross_wasm32-wasi-release+fully_static.tar.xz",
            pipelineFilter = [("source", Just "schedule"), ("scope", Just "finished")]
          },
      (,)
        "wasm32-wasi-ghc-9.6"
        GitLabArtifact
          { gitlabDomain = "gitlab.haskell.org",
            projectId = 1,
            ref = "ghc-9.6",
            jobName = "x86_64-linux-alpine3_12-cross_wasm32-wasi-release+fully_static",
            artifactPath = "ghc-x86_64-linux-alpine3_12-cross_wasm32-wasi-release+fully_static.tar.xz",
            pipelineFilter = [("status", Just "success")]
          },
      (,)
        "wasm32-wasi-ghc-9.8"
        GitLabArtifact
          { gitlabDomain = "gitlab.haskell.org",
            projectId = 1,
            ref = "ghc-9.8",
            jobName = "x86_64-linux-alpine3_17-wasm-cross_wasm32-wasi-release+fully_static",
            artifactPath = "ghc-x86_64-linux-alpine3_17-wasm-cross_wasm32-wasi-release+fully_static.tar.xz",
            pipelineFilter = [("status", Just "success")]
          },
      (,)
        "wasi-sdk"
        GitLabArtifact
          { gitlabDomain = "gitlab.haskell.org",
            projectId = 3212,
            ref = "main",
            jobName = "x86_64-linux",
            artifactPath = "dist/wasi-sdk-18-linux.tar.gz",
            pipelineFilter = [("status", Just "success")]
          },
      (,)
        "wasi-sdk-darwin"
        GitLabArtifact
          { gitlabDomain = "gitlab.haskell.org",
            projectId = 3212,
            ref = "main",
            jobName = "darwin",
            artifactPath = "dist/wasi-sdk-18-macos.tar.gz",
            pipelineFilter = [("status", Just "success")]
          },
      (,)
        "wasi-sdk-aarch64-linux"
        GitLabArtifact
          { gitlabDomain = "gitlab.haskell.org",
            projectId = 3212,
            ref = "main",
            jobName = "aarch64-linux",
            artifactPath = "dist/wasi-sdk-18-linux.tar.gz",
            pipelineFilter = [("status", Just "success")]
          },
      (,)
        "libffi-wasm"
        GitLabArtifact
          { gitlabDomain = "gitlab.haskell.org",
            projectId = 3214,
            ref = "master",
            jobName = "x86_64-linux",
            artifactPath = "",
            pipelineFilter = []
          },
      (,)
        "binaryen"
        GitHubArtifact
          { ownerRepo = "type-dance/binaryen",
            branch = "main",
            workflowName = "release",
            artifactName = "release-ubuntu-latest",
            event = "push"
          },
      (,)
        "wizer"
        GitHubArtifact
          { ownerRepo = "bytecodealliance/wizer",
            branch = "main",
            workflowName = "Release",
            artifactName = "bins-x86_64-linux",
            event = "push"
          },
      (,)
        "proot"
        GitLabArtifact
          { gitlabDomain = "gitlab.com",
            projectId = 9799675,
            ref = "master",
            jobName = "dist",
            artifactPath = "dist/proot",
            pipelineFilter = []
          }
    ]
