docker run --rm -v "${PWD}/graphsense.yaml:/spec.yaml" -v "${PWD}/build:/build" openapitools/openapi-generator-cli:latest generate -i /spec.yaml -g elm -o /build --additional-properties=generateAliasAsModel=false

