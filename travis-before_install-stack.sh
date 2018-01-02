#!/bin/sh
set -ex

. ./travis-common.sh

mkdir -p ~/.local/bin
travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 \
    | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
stack setup --resolver "$STACKAGE_RESOLVER"

git version
