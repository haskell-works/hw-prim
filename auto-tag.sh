#!/usr/bin/env bash

if [ "$_system_type" == "Darwin" ]; then
  sed () {
    gsed "$@"
  }
fi

_repo_name=$(basename `git rev-parse --show-toplevel`)

_tag_prefix=$(
  cat $_repo_name.cabal | grep '^version:' | head | cut -d ':' -f 2 | xargs | sed 's|\.[0-9]\+$||g'
)

_last_tag_suffix=$(
  (
    echo 0
    git tag | grep "^$_tag_prefix\." | cut -d . -f 4
  ) | sort -g | tail -n 1
)

_next_tag_suffix=$(($_last_tag_suffix + 1))

_next_tag=$_tag_prefix.$_next_tag_suffix

echo $_tag_prefix.$_next_tag_suffix

sed -e "s|^\(version:\s\+\)\([0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+\)|\1$_next_tag|g" -i "$_repo_name.cabal"

git add $_repo_name.cabal
git commit -m "Auto tag v$_next_tag"
git tag -f -a "v$_next_tag" -m "Auto tag v$_next_tag"
git push -f origin "v$_next_tag"
