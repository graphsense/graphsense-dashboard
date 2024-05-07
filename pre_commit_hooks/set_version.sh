#!/usr/bin/env /bin/bash

tag=`git describe --tags --abbrev=0`
echo $tag
#commit=`git log -n 1 --pretty=format:"%h"`

cat > src/Version.elm <<EOF
module Version exposing (version)


version : String
version =
    "$tag"
EOF
