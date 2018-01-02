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

if [ "$USE_GOLD" = "YES" ]
then
    sudo update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.gold" 20
    sudo update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.bfd" 10
fi
ld -v

if [ "$GHCVER" = "none" ]; then
    travis_retry sudo add-apt-repository -y ppa:hvr/ghc
    travis_retry sudo apt-get update
    travis_retry sudo apt-get install --force-yes ghc-$GHCVER
fi

if [ "$TRAVIS_OS_NAME" = "linux" ]; then
    travis_retry sudo add-apt-repository -y ppa:hvr/ghc
    travis_retry sudo apt-get update
    travis_retry \
        sudo apt-get install --force-yes \
        cabal-install-head cabal-install-2.0 \
        happy-1.19.5 alex-3.1.7 \
        ghc-$GHCVER-prof ghc-$GHCVER-dyn

    if [ "x$TEST_OTHER_VERSIONS" = "xYES" ]
    then
        travis_retry \
            sudo apt-get install --force-yes \
            ghc-7.0.4-prof ghc-7.0.4-dyn \
            ghc-7.2.2-prof ghc-7.2.2-dyn \
            ghc-head-prof ghc-head-dyn
    fi

fi

git version
