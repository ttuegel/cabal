#!/bin/sh
set -ex

. ./travis-common.sh

case "$GHCVER" in
    7.8.*|7.10.*|8.0.*)
        GHCXZ=YES
        ;;
    *)
        echo "unknown or unsupported GHC version: $GHCVER"
        false
        ;;
esac

case "$GHCVER" in
    7.10.*|8.0.*)
        GHCURL=http://downloads.haskell.org/~ghc/$GHCVER/ghc-$GHCVER-x86_64-apple-darwin.tar.xz
        ;;
    7.4.*|7.6.*|7.8.*)
        GHCURL=https://www.haskell.org/ghc/dist/$GHCVER/ghc-$GHCVER-x86_64-apple-darwin.tar.xz
        ;;
    *)
        echo "unknown or unsupported GHC version: $GHCVER"
        false
        ;;
esac

travis_retry curl -OL $GHCURL
if [ "$GHCXZ" = "YES" ]; then
    tar -xJf ghc-*.tar.*;
else
    tar -xjf ghc-*.tar.*;
fi

cd ghc-*;
./configure --prefix=$HOME/.ghc-install/$GHCVER
make install;
cd ..;

mkdir "${HOME}/bin"
travis_retry curl -L https://www.haskell.org/cabal/release/cabal-install-2.0.0.0/cabal-install-2.0.0.0-x86_64-apple-darwin-sierra.tar.xz | tar xJO > "${HOME}/bin/cabal"
chmod a+x "${HOME}/bin/cabal"
"${HOME}/bin/cabal" --version

git version
