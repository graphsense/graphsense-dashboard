#!/bin/bash
openapi=$1
if [ -z "$openapi" ]; then
  echo "Pass a URL or file path to the openapi spec"
  exit 1
fi
temp=`mktemp`
dir=$PWD/`dirname $0`
if [[ "$openapi" =~ ^http ]]; then
  echo "Fetching from $openapi"
  wget https://raw.githubusercontent.com/graphsense/graphsense-openapi/develop/graphsense.yaml -O $temp
else
  echo "Copying from $openapi"
  cp "$openapi" $temp
fi
temp2=`mktemp`
echo "Generating"
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
