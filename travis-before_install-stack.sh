#!/bin/sh
set -ex

PATH=/opt/ghc/$GHCVER/bin:$PATH
PATH=$HOME/.ghc-install/$GHCVER/bin:$PATH
PATH=$HOME/bin:$PATH
PATH=$HOME/.cabal/bin:$PATH
PATH=$HOME/.local/bin:$PATH
PATH=/opt/cabal/2.0/bin:$PATH
PATH=/opt/happy/1.19.5/bin:$PATH
PATH=/opt/alex/3.1.7/bin:$PATH
export PATH

. ./travis-common.sh

mkdir -p ~/.local/bin
travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 \
    | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
stack setup --resolver "$STACKAGE_RESOLVER"

git version
