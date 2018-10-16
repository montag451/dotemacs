#!/usr/bin/env bash

set -e

DEFAULT_BASE_URL=$(git config --local remote.origin.url)
BASE_URL=${1:-${DEFAULT_BASE_URL}}

git submodule init
git config --local --name-only --null --get-regexp "^submodule\..*\.url$" | while IFS= read -d $'\0' name; do
    git config --local "${name}" "${BASE_URL%/}/lib/$(echo "$name" | cut -d . -f 2)";
done
