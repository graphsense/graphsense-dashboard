openapi:
	docker run --rm \
		-v "${PWD}/openapi:/build" 
		openapitools/openapi-generator-cli:latest generate \
			-i https://raw.githubusercontent.com/graphsense/graphsense-openapi/develop/graphsense.yaml \
			-g elm \
			-o /build \
			--additional-properties=generateAliasAsModel=false

.PHONY: openapi
