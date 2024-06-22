include .env

API_ELM=openapi/src/Api.elm
REST_URL?=https://app.ikna.io

install:
	pip install pre-commit
	pre-commit install

openapi:
	tools/generate-openapi.sh $(OPENAPI_LOCATION) $(REST_URL)
			#--global-property=debugModels \
			#--global-property=debugOperations \

dev: $(API_ELM) $(wildcard src/**)
	make generated
	npx elm-test-rs --watch tests/Graph/View/TestLabel.elm

#$(API_ELM): $(wildcard templates/*) $(OPENAPI_LOCATION)
	#make openapi

setem:
	npx setem --output generated

serve:
	npm run dev

test:
	npx elm-test-rs

build:
	npm run build

build-docker:
	docker build . -t graphsense-dashboard

serve-docker: build-docker
	docker run -it --network='host' -e REST_URL=http://localhost:9000 localhost/graphsense-dashboard:latest

format:
	npx elm-format --yes src

theme:
	mkdir -p theme
	curl 'https://api.figma.com/v1/files/$(FIGMA_FILE_ID)?geometry=paths' -H 'X-Figma-Token: $(FIGMA_API_TOKEN)' | jq > theme/figma.json
	npx elm-codegen run --debug --flags-from=./theme/figma.json --output theme

gen:
	rm -rf generated/*
	node generate.js
	-make theme
	make setem

.PHONY: openapi serve test format build build-docker serve-docker gen theme
