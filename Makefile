include .env

API_ELM=openapi/src/Api.elm

openapi:
	tools/generate-openapi.sh https\://$(OPENAPI_LOCATION) $(REST_URL)
			#--global-property=debugModels \
			#--global-property=debugOperations \

watch:
	find . -name \*.elm | entr make dev

dev: $(API_ELM) $(wildcard src/**)
	make setem
	npx elm-test-rs --watch tests/Graph/View/TestLabel.elm

$(API_ELM): $(wildcard templates/*) $(OPENAPI_LOCATION)
	make openapi

setem:
	npx setem --output src/

serve:
	npm run dev

test:
	npx elm-test

.PHONY: openapi serve test
