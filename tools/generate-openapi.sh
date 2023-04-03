#!/bin/bash
openapi=$1
if [ -z "$openapi" ]; then
  echo "Pass a URL or file path to the openapi spec"
  exit 1
fi
resturl=$2
temp=`mktemp`
dir=$PWD/`dirname $0`
if [[ "$openapi" =~ ^http ]]; then
  echo "Fetching from $openapi"
  wget $openapi -O $temp
else
  echo "Copying from $openapi"
  cp "$openapi" $temp
fi
temp2=`mktemp`
echo "Generating"
python3 $dir/mangle-openapi.py $temp $resturl > $temp2

docker run --rm \
    -v "${dir}/../openapi:/build:Z"  \
    -v "${temp2}:/spec.yaml:Z" \
    -v "${dir}/../templates:/templates:Z" \
    openapitools/openapi-generator-cli:latest generate \
    -i /spec.yaml \
    -g elm \
    -o /build \
    --additional-properties=generateAliasAsModel=false \
    -t /templates 
rm $temp $temp2

sed -i 's/"txType"/"tx_type"/' "${dir}/../openapi/src/Api/Data.elm"
sed -i 's/tx_hash/txHash/' "${dir}/../openapi/src/Api/Request/Txs.elm"
printf "\n\nvaluesDecodervaluesDecoder = valuesDecoder" >> "${dir}/../openapi/src/Api/Data.elm"
