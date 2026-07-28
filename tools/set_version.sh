#!/usr/bin/env /bin/bash

# Stamps the nearest tag into src/Version.elm (pre-push hook).
#
# `git describe` picks arbitrarily when several tags sit on the same commit.
# A stray v26.07.13 alongside v26.08.0-dev.5 on the same commit was enough to
# stamp the wrong version and then block every push: this hook rewrites the
# file, and pre-commit fails any hook that modifies files. So resolve the tag
# describe lands on to its commit, and take the highest version tag on it.

nearest=`git describe --tags --abbrev=0 2>/dev/null`

if [ -z "$nearest" ]; then
  echo "No tag found - leaving src/Version.elm unchanged."
  exit 0
fi

commit=`git rev-parse "$nearest^{commit}"`

# versionsort.suffix makes -dev/-rc rank *below* the plain release they lead up
# to; git's default ranks them above it, so tagging v26.08.0 onto a commit that
# already carries v26.08.0-dev.6 would stamp the dev version. Suffixes not
# listed here (one-off branch tags like -removepf1.1) still outrank a release
# tag on the same commit - don't put those two on one commit.
tag=`git -c versionsort.suffix=-dev -c versionsort.suffix=-rc \
      tag --points-at "$commit" --sort=-v:refname | head -1`
tag=${tag:-$nearest}

cat > src/Version.elm <<EOF
module Version exposing (version)


version : String
version =
    "$tag"
EOF

# Only ask for a commit when the file actually changed - otherwise this prints
# "Please commit and try again" on every push, which reads like a failure when
# nothing happened.
if ! git diff --quiet -- src/Version.elm; then
  echo "Set version in src/Version.elm to $tag. Please commit and try again."
fi
