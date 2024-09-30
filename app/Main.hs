module Main where

import Codec.Archive.Zip.Conduit.UnZip qualified as UnZip
import Conduit
import Control.Lens
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty qualified as AEP
import Data.Aeson.Lens
import Data.ByteString.Base64 (encodeBase64)
import Data.ByteString.Lazy qualified as BL
import Data.Conduit.Lzma qualified as Lzma
import Data.Conduit.Tar qualified as Tar
import Data.Conduit.Zstd qualified as Zstd
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Deriving.Aeson
import Network.HTTP.Client.Conduit (defaultManagerSettings)
import Network.HTTP.Conduit qualified as HTTP
import Options.Generic qualified as OG
import System.FilePath
import Text.Regex.Pcre2 qualified as Pcre2
import UnliftIO.Directory

data CLI w = CLI
  { downloadUrlPrefix :: w OG.::: Text,
    artifactDir :: w OG.::: FilePath OG.<!> "artifacts",
    metadataPath :: w OG.::: FilePath OG.<!> "meta.json",
    bindistRegex :: w OG.::: Text OG.<!> ""
  }
  deriving stock (Generic)

instance OG.ParseRecord (CLI OG.Wrapped) where
  parseRecord = OG.parseRecordWithModifiers OG.lispCaseModifiers

main :: IO ()
main = do
  for_ [stdout, stderr] $ flip hSetBuffering LineBuffering
  cli@CLI {..} <- OG.unwrapRecord "mirror various GHC WASM bindists"
  mgr <- HTTP.newManager defaultManagerSettings
  bindists <- loadStoredBindists metadataPath
  bindists <- updateStoredBindists mgr cli artifactDir bindists
  saveStoredBindists metadataPath bindists

type Url = Text -- such typesafe much wow

data StoredBindist = StoredBindist
  { mirrorUrl :: Url,
    originalUrl :: Url,
    sriHash :: Text,
    ghcSubdir :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving (A.FromJSON, A.ToJSON) via CustomJSON '[OmitNothingFields] StoredBindist

type StoredBindists = Map Text StoredBindist

loadStoredBindists :: FilePath -> IO StoredBindists
loadStoredBindists path =
  doesFileExist path >>= \case
    True -> either fail pure =<< A.eitherDecodeFileStrict' path
    False -> pure Map.empty

saveStoredBindists :: FilePath -> StoredBindists -> IO ()
saveStoredBindists path = BL.writeFile path . AEP.encodePretty

updateStoredBindists ::
  HTTP.Manager -> CLI OG.Unwrapped -> FilePath -> StoredBindists -> IO StoredBindists
updateStoredBindists mgr cli bindistDir =
  Map.mergeA
    do Map.traverseMaybeMissing \name src -> updateBindist name src Nothing
    do Map.preserveMissing
    do Map.zipWithMaybeAMatched \name src -> updateBindist name src . Just
    bindistInfos
  where
    updateBindist :: Text -> BindistInfo -> Maybe StoredBindist -> IO (Maybe StoredBindist)
    updateBindist bindistName bindistInfo prevBindist
      | Pcre2.matches cli.bindistRegex bindistName = fmap Just do
          putTextLn $ "checking " <> bindistName
          originalUrl <- getLatestBindistURL mgr bindistInfo.src
          case prevBindist of
            Just bindist | bindist.originalUrl == originalUrl -> do
              putTextLn "no updates"
              pure bindist
            _ -> do
              putTextLn "updating"
              createDirectoryIfMissing True bindistDir
              (fileName, sha256, ghcSubdir) <-
                download
                  mgr
                  bindistInfo.dlArgs
                  originalUrl
                  bindistDir
                  (toString bindistName)
              pure
                StoredBindist
                  { mirrorUrl = cli.downloadUrlPrefix <> toText fileName,
                    originalUrl = originalUrl,
                    sriHash = "sha256-" <> encodeBase64 sha256,
                    ghcSubdir
                  }
      | otherwise = pure prevBindist

download ::
  HTTP.Manager ->
  DownloadArgs ->
  Url ->
  -- | Target directory.
  FilePath ->
  -- | Identifier (used as the file base name).
  String ->
  -- | The file name, SHA256 hash, and the GHC bindist subdir name (if
  -- applicable).
  IO (FilePath, ByteString, Maybe Text)
download mgr dlArgs url dir basename = runConduitRes do
  req <- HTTP.parseUrlThrow (toString url)
  res <- lift $ HTTP.http req mgr
  (ext, (sha256, ghcSubdir)) <-
    HTTP.responseBody res .| (fuseBoth preprocess . getZipSink) do
      ZipSink $ sinkFile initialFile
      sha256 <- ZipSink sinkSha256
      ghcSubdir <- case dlArgs.isGhcBindist of
        Just compressionFormat -> ZipSink do
          let decompress = case compressionFormat of
                Lzma -> Lzma.decompress Nothing
                Zstd -> Zstd.decompress
          Just fi <- decompress .| Tar.untar yield .| headC
          pure $ Just $ T.takeWhile (/= '/') $ decodeUtf8 $ Tar.filePath fi
        Nothing -> pure Nothing
      pure (sha256, ghcSubdir)
  let actualFileName = addExtension basename ext
  renameFile initialFile (dir </> actualFileName)
  pure (actualFileName, sha256, ghcSubdir)
  where
    initialFile = dir </> basename

    sinkSha256 = SHA256.finalize <$> foldlC SHA256.update SHA256.init

    -- Preprocess, and return the file extension to use.
    preprocess :: ConduitT ByteString ByteString (ResourceT IO) String
    preprocess
      | dlArgs.isSingleEntryZip =
          void UnZip.unZipStream .| do
            Just (Left UnZip.ZipEntry {zipEntryName}) <- await
            concatC
            let fileName = either toString decodeUtf8 zipEntryName
            pure $ takeAtMostTwoExtensions fileName
      | otherwise = do
          awaitForever yield
          pure $ takeAtMostTwoExtensions $ toString url
      where
        takeAtMostTwoExtensions p = takeExtension p' <> ext1
          where
            (p', ext1) = splitExtension p

data BindistInfo = BindistInfo
  { dlArgs :: DownloadArgs,
    src :: BindistSrc
  }
  deriving stock (Show)

data DownloadArgs = DownloadArgs
  { -- | If 'True: unpack the ZIP file.
    isSingleEntryZip :: Bool,
    -- | Used to extract the GHC bindist subdir for metadata.
    isGhcBindist :: Maybe CompressionFormat
  }
  deriving stock (Show)

rawFileDownloadArgs :: DownloadArgs
rawFileDownloadArgs =
  DownloadArgs
    { isSingleEntryZip = False,
      isGhcBindist = Nothing
    }

data CompressionFormat = Lzma | Zstd
  deriving stock (Show)

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
        artifactName :: Text
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

    apiRes <- fetch runsUrl [("branch", qv branch)]
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

bindistInfos :: Map Text BindistInfo
bindistInfos =
  Map.fromList
    [ (,)
        "wasm32-wasi-ghc-gmp"
        BindistInfo
          { dlArgs = rawFileDownloadArgs {isGhcBindist = Just Lzma},
            src =
              GitLabArtifact
                { gitlabDomain = "gitlab.haskell.org",
                  projectId = 1,
                  ref = "master",
                  jobName = "nightly-x86_64-linux-alpine3_18-wasm-cross_wasm32-wasi-release+fully_static+text_simdutf",
                  artifactPath = "ghc-x86_64-linux-alpine3_18-wasm-cross_wasm32-wasi-release+fully_static+text_simdutf.tar.xz",
                  pipelineFilter = [("source", Just "schedule")]
                }
          },
      (,)
        "wasm32-wasi-ghc-native"
        BindistInfo
          { dlArgs = rawFileDownloadArgs {isGhcBindist = Just Lzma},
            src =
              GitLabArtifact
                { gitlabDomain = "gitlab.haskell.org",
                  projectId = 1,
                  ref = "master",
                  jobName = "nightly-x86_64-linux-alpine3_18-wasm-int_native-cross_wasm32-wasi-release+fully_static+text_simdutf",
                  artifactPath = "ghc-x86_64-linux-alpine3_18-wasm-int_native-cross_wasm32-wasi-release+fully_static+text_simdutf.tar.xz",
                  pipelineFilter = [("source", Just "schedule")]
                }
          },
      (,)
        "wasm32-wasi-ghc-unreg"
        BindistInfo
          { dlArgs = rawFileDownloadArgs {isGhcBindist = Just Lzma},
            src =
              GitLabArtifact
                { gitlabDomain = "gitlab.haskell.org",
                  projectId = 1,
                  ref = "master",
                  jobName = "nightly-x86_64-linux-alpine3_18-wasm-unreg-cross_wasm32-wasi-release+fully_static+text_simdutf",
                  artifactPath = "ghc-x86_64-linux-alpine3_18-wasm-unreg-cross_wasm32-wasi-release+fully_static+text_simdutf.tar.xz",
                  pipelineFilter = [("source", Just "schedule")]
                }
          },
      (,)
        "wasm32-wasi-ghc-9.6"
        BindistInfo
          { dlArgs = rawFileDownloadArgs {isGhcBindist = Just Lzma},
            src =
              GitLabArtifact
                { gitlabDomain = "gitlab.haskell.org",
                  projectId = 3223,
                  ref = "ghc-9.6",
                  jobName = "x86_64-linux-alpine3_18-wasm-cross_wasm32-wasi-release+fully_static",
                  artifactPath = "ghc-x86_64-linux-alpine3_18-wasm-cross_wasm32-wasi-release+fully_static.tar.xz",
                  pipelineFilter = []
                }
          },
      (,)
        "wasm32-wasi-ghc-9.8"
        BindistInfo
          { dlArgs = rawFileDownloadArgs {isGhcBindist = Just Lzma},
            src =
              GitLabArtifact
                { gitlabDomain = "gitlab.haskell.org",
                  projectId = 3223,
                  ref = "ghc-9.8",
                  jobName = "x86_64-linux-alpine3_18-wasm-cross_wasm32-wasi-release+fully_static",
                  artifactPath = "ghc-x86_64-linux-alpine3_18-wasm-cross_wasm32-wasi-release+fully_static.tar.xz",
                  pipelineFilter = []
                }
          },
      (,)
        "wasm32-wasi-ghc-9.10"
        BindistInfo
          { dlArgs = rawFileDownloadArgs {isGhcBindist = Just Lzma},
            src =
              GitLabArtifact
                { gitlabDomain = "gitlab.haskell.org",
                  projectId = 1,
                  ref = "ghc-9.10",
                  jobName = "x86_64-linux-alpine3_18-wasm-cross_wasm32-wasi-release+fully_static",
                  artifactPath = "ghc-x86_64-linux-alpine3_18-wasm-cross_wasm32-wasi-release+fully_static.tar.xz",
                  pipelineFilter = [] -- [("status", Just "success")]
                }
          },
      (,)
        "wasm32-wasi-ghc-gmp-aarch64-darwin"
        BindistInfo
          { dlArgs = DownloadArgs {isSingleEntryZip = True, isGhcBindist = Just Zstd},
            src =
              GitHubArtifact
                { ownerRepo = "tweag/ghc-wasm-bindists",
                  branch = "main",
                  workflowName = "ghc-wasm-darwin-bindist",
                  artifactName = "ghc-wasm-aarch64-darwin-bindist"
                }
          },
      (,)
        "wasm32-wasi-ghc-gmp-x86_64-darwin"
        BindistInfo
          { dlArgs = DownloadArgs {isSingleEntryZip = True, isGhcBindist = Just Zstd},
            src =
              GitHubArtifact
                { ownerRepo = "tweag/ghc-wasm-bindists",
                  branch = "main",
                  workflowName = "ghc-wasm-darwin-bindist",
                  artifactName = "ghc-wasm-x86_64-darwin-bindist"
                }
          },
      (,)
        "wasm32-wasi-ghc-gmp-aarch64-linux"
        BindistInfo
          { dlArgs = DownloadArgs {isSingleEntryZip = True, isGhcBindist = Just Zstd},
            src =
              GitHubArtifact
                { ownerRepo = "tweag/ghc-wasm-bindists",
                  branch = "main",
                  workflowName = "ghc-wasm-aarch64-linux-bindist",
                  artifactName = "ghc-wasm-aarch64-linux-bindist"
                }
          },
      (,)
        "wasi-sdk"
        BindistInfo
          { dlArgs = rawFileDownloadArgs,
            src =
              GitLabArtifact
                { gitlabDomain = "gitlab.haskell.org",
                  projectId = 3212,
                  ref = "master",
                  jobName = "x86_64-linux",
                  artifactPath = "dist/wasi-sdk-25.0-x86_64-linux.tar.gz",
                  pipelineFilter = [("status", Just "success")]
                }
          },
      (,)
        "wasi-sdk-aarch64-darwin"
        BindistInfo
          { dlArgs = rawFileDownloadArgs,
            src =
              GitLabArtifact
                { gitlabDomain = "gitlab.haskell.org",
                  projectId = 3212,
                  ref = "master",
                  jobName = "aarch64-darwin",
                  artifactPath = "dist/wasi-sdk-25.0-x86_64-macos.tar.gz",
                  pipelineFilter = [("status", Just "success")]
                }
          },
      (,)
        "wasi-sdk-x86_64-darwin"
        BindistInfo
          { dlArgs = rawFileDownloadArgs,
            src =
              GitLabArtifact
                { gitlabDomain = "gitlab.haskell.org",
                  projectId = 3212,
                  ref = "master",
                  jobName = "x86_64-darwin",
                  artifactPath = "dist/wasi-sdk-25.0-x86_64-macos.tar.gz",
                  pipelineFilter = [("status", Just "success")]
                }
          },
      (,)
        "wasi-sdk-aarch64-linux"
        BindistInfo
          { dlArgs = rawFileDownloadArgs,
            src =
              GitLabArtifact
                { gitlabDomain = "gitlab.haskell.org",
                  projectId = 3212,
                  ref = "master",
                  jobName = "aarch64-linux",
                  artifactPath = "dist/wasi-sdk-25.0-aarch64-linux.tar.gz",
                  pipelineFilter = [("status", Just "success")]
                }
          },
      (,)
        "libffi-wasm"
        BindistInfo
          { dlArgs = rawFileDownloadArgs,
            src =
              GitLabArtifact
                { gitlabDomain = "gitlab.haskell.org",
                  projectId = 3214,
                  ref = "master",
                  jobName = "x86_64-linux",
                  artifactPath = "",
                  pipelineFilter = []
                }
          },
      (,)
        "proot"
        BindistInfo
          { dlArgs = rawFileDownloadArgs,
            src =
              GitLabArtifact
                { gitlabDomain = "gitlab.com",
                  projectId = 9799675,
                  ref = "master",
                  jobName = "dist",
                  artifactPath = "dist/proot",
                  pipelineFilter = []
                }
          }
    ]
