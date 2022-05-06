include .env

API_ELM=openapi/src/Api.elm

openapi:
	tools/generate-openapi.sh $(OPENAPI_LOCATION) $(REST_URL)
			#--global-property=debugModels \
			#--global-property=debugOperations \

watch:
	find . -name \*.elm | entr make dev

dev: $(API_ELM) $(wildcard src/**)
	npx elm-test-rs tests/Graph/Update/TestLayer.elm

$(API_ELM): $(wildcard templates/*) $(OPENAPI_LOCATION)
	make openapi

setem:
	yarn setem --output src/

.PHONY: openapi
