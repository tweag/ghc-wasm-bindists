#!/bin/sh

set -eu

export PATH=~/.local/bin:"$PATH"

apk upgrade
apk add \
  alpine-sdk \
  autoconf \
  automake \
  bash \
  cabal \
  coreutils \
  ghc \
  jq \
  ncurses-static \
  python3 \
  xz \
  zstd

cd "$(mktemp -d)"
curl -f -L --retry 5 https://github.com/tweag/rust-alpine-mimalloc/archive/refs/heads/master.tar.gz | tar xz --strip-components=1
mv mimalloc.diff /tmp
./build.sh
cd "$OLDPWD"

export LD_PRELOAD=/usr/lib/libmimalloc.so

cd "$(mktemp -d)"
curl -f -L --retry 5 https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/raw/master/bootstrap.sh | PREFIX=$PWD SKIP_GHC=1 sh
. "$PWD/env"
cd "$OLDPWD"

cd "$(mktemp -d)"

git clone --ref-format=reftable --depth=1 --recurse-submodules --shallow-submodules --jobs 32 https://gitlab.haskell.org/ghc/ghc.git .

cabal update

./hadrian/build --version

cabal install \
  alex-3.5.1.0 \
  happy-2.0.2

./boot

./configure --host="$(uname -m)-alpine-linux" --target=wasm32-wasi --with-intree-gmp --with-system-libffi

hadrian/build --no-color --no-progress --no-time --flavour=release+host_fully_static+text_simdutf --docs=none -j binary-dist-dir test:all_deps _build/stage0/bin/wasm32-wasi-haddock _build/stage0/bin/wasm32-wasi-hpc _build/stage0/bin/wasm32-wasi-runghc

BINDIST_NAME=$(basename _build/bindist/*)

tar --sort=name --mtime="@$(git log -1 --pretty=%ct)" --owner=0 --group=0 --numeric-owner --use-compress-program="zstd --ultra -22 --threads=0" -cf /workspace/$BINDIST_NAME.tar.zst -C _build/bindist $BINDIST_NAME

hadrian/build --no-color --no-progress --no-time --flavour=release+host_fully_static+text_simdutf --docs=none -j test

cd "$OLDPWD"
