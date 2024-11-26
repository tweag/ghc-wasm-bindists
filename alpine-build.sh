#!/bin/sh

set -eu

export PATH=~/.local/bin:"$PATH"

apk upgrade
apk add \
  alpine-sdk \
  autoconf \
  automake \
  bash \
  coreutils \
  curl \
  gmp-dev \
  jq \
  musl-locales \
  ncurses-dev \
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
curl -f -L --retry 5 https://downloads.haskell.org/ghc/9.10.1/ghc-9.10.1-aarch64-alpine3_18-linux.tar.xz | tar xJ --strip-components=1
./configure --prefix=$HOME/.local
make install -j16

curl -f -L --retry 5 https://downloads.haskell.org/cabal/cabal-install-3.12.1.0/cabal-install-3.12.1.0-aarch64-linux-alpine3_18.tar.xz | tar xJ -C ~/.local/bin cabal

cd "$(mktemp -d)"

git clone --ref-format=reftable --depth=1 --recurse-submodules --shallow-submodules --jobs 32 https://gitlab.haskell.org/ghc/ghc.git .

cabal update

./hadrian/build --version

cabal install \
  alex-3.5.1.0 \
  happy-1.20.1.1

./boot

cd "$(mktemp -d)"
curl -f -L --retry 5 https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | PREFIX=$PWD SKIP_GHC=1 sh
. "$PWD/env"
cd "$OLDPWD"

./configure --host="$(uname -m)-alpine-linux" --target=wasm32-wasi --with-intree-gmp --with-system-libffi

hadrian/build --no-color --no-progress --no-time --flavour=release+host_fully_static+text_simdutf --docs=none -j binary-dist-dir test:all_deps _build/stage0/bin/wasm32-wasi-haddock _build/stage0/bin/wasm32-wasi-hpc _build/stage0/bin/wasm32-wasi-runghc

BINDIST_NAME=$(basename _build/bindist/*)

tar --sort=name --mtime="@$(git log -1 --pretty=%ct)" --owner=0 --group=0 --numeric-owner --use-compress-program="zstd --ultra -22 --threads=0" -cf /workspace/$BINDIST_NAME.tar.zst -C _build/bindist $BINDIST_NAME

hadrian/build --no-color --no-progress --no-time --flavour=release+host_fully_static+text_simdutf --docs=none -j test

cd "$OLDPWD"
