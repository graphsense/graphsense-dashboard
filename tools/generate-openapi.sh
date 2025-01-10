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
    openapitools/openapi-generator-cli:v7.10.0 generate \
    -i /spec.yaml \
    -g elm \
    -o /build \
    --additional-properties=generateAliasAsModel=false \
    -t /templates 
rm $temp $temp2
echo "Doing some custom search and replace in Data.elm"
sed -i 's/"txType"/"tx_type"/' "${dir}/../openapi/src/Api/Data.elm"
sed -i 's/Addressstatus/AddressStatus/' "${dir}/../openapi/src/Api/Data.elm"
sed -i 's/addressstatus/addressStatus/' "${dir}/../openapi/src/Api/Data.elm"
sed -i 's/Json.Decode.dict tagCloudEntryDecodertagCloudEntryDecoder/Json.Decode.dict tagCloudEntryDecoder/' "${dir}/../openapi/src/Api/Data.elm"

sed -i 's/labelSummaryDecoderlabelSummaryDecoder/labelSummaryDecoder/' "${dir}/../openapi/src/Api/Data.elm"
sed -i 's/TaginheritedFrom/TagInheritedFrom/' "${dir}/../openapi/src/Api/Data.elm"
sed -i 's/LabelSummaryinheritedFrom/LabelSummaryInheritedFrom/' "${dir}/../openapi/src/Api/Data.elm"
sed -i 's/AddressTaginheritedFrom/AddressTagInheritedFrom/' "${dir}/../openapi/src/Api/Data.elm"
sed -i 's/addressTaginheritedFromVariants/addressTagInheritedFromVariants/' "${dir}/../openapi/src/Api/Data.elm"
sed -i 's/labelSummaryinheritedFromVariants/labelSummaryInheritedFromVariants/' "${dir}/../openapi/src/Api/Data.elm"
sed -i 's/taginheritedFromVariants/tagInheritedFromVariants/' "${dir}/../openapi/src/Api/Data.elm"


# remove duplicate Direction and order types
patterns='^type Direction,^directionVariants : List Direction,^stringFromDirection : Direction -> String,^makeDirectionFromString : String -> Maybe Direction,^type Order_,^orderVariants : List Order_,^stringFromOrder_ : Order_ -> String,^makeOrder_FromString : String -> Maybe Order_'
python ${dir}/removeDuplicateOccurances.py "${patterns}" ${dir}/../openapi/src/Api/Request/Entities.elm
python ${dir}/removeDuplicateOccurances.py "${patterns}" ${dir}/../openapi/src/Api/Request/Addresses.elm


sed -i 's/tx_hash/txHash/' "${dir}/../openapi/src/Api/Request/Txs.elm"
printf "\n\nvaluesDecodervaluesDecoder = valuesDecoder" >> "${dir}/../openapi/src/Api/Data.elm"
rm ${dir}/../openapi/src/Api/Request/Bulk.elm
