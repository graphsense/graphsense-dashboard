include .env

API_ELM=openapi/src/Api.elm

openapi:
	tools/generate-openapi.sh $(OPENAPI_LOCATION)
			#--global-property=debugModels \
			#--global-property=debugOperations \

watch:
	find . -name \*.elm | entr make dev

dev: $(API_ELM) $(wildcard src/**)
	npx elm-test

$(API_ELM): $(wildcard templates/*)
	make openapi

.PHONY: openapi
