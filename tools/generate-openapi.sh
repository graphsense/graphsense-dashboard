#!/bin/bash
temp=`mktemp`
dir=$PWD/`dirname $0`
wget https://raw.githubusercontent.com/graphsense/graphsense-openapi/develop/graphsense.yaml -O $temp
temp2=`mktemp`
python $dir/mangle-openapi.py $temp > $temp2
docker run --rm \
    -v "${dir}/../openapi:/build"  \
    -v "${temp2}:/spec.yaml" \
    -v "${dir}/../templates:/templates" \
    openapitools/openapi-generator-cli:latest generate \
    -i /spec.yaml \
    -g elm \
    -o /build \
    --additional-properties=generateAliasAsModel=false \
    -t /templates 
rm $temp $temp2
