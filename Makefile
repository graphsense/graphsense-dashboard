-include .env

API_ELM=openapi/src/Api.elm
REST_URL?=https://app.ikna.io

install:
	pip install pre-commit
	pre-commit install
	npm install

openapi:
	tools/generate-openapi.sh $(OPENAPI_LOCATION) $(REST_URL)
			#--global-property=debugModels \
			#--global-property=debugOperations \

dev: $(API_ELM) $(wildcard src/**)
	make generated
	npx elm-test-rs --watch tests/Graph/View/TestLabel.elm

#$(API_ELM): $(wildcard templates/*) $(OPENAPI_LOCATION)
	#make openapi

clean:
	rm -rf ./elm-stuff/
	rm -rf ./dist/
	rm -rf ./theme/Theme/
	rm -rf ./generated/
	rm -rf ./codegen/generated/
	rm -rf elm.json


setem:
	npx setem --output generated
	cd codegen && mkdir -p codegen/generated && npx setem --output generated

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
	npx elm-format --yes src tests plugins

format-plugins:
	npx elm-format --yes plugins

lint:
	npx elm-review

lint-fix:
	npx elm-review --fix-all

lint-ci:
	npx elm-review --ignore-files src/Util/View.elm,src/View/Box.elm,src/View/Locale.elm,src/Update/Search.elm,src/Route/Graph.elm,src/Route.elm,src/View/Graph/Table.elm,src/Css/Button.elm,config/Config.elm

theme-refresh: 
	mkdir -p theme
	npx elm-codegen run --debug --flags='{"figma_file_id": "$(FIGMA_FILE_ID)", "api_key": "$(FIGMA_API_TOKEN)"}' --output theme

theme:
	npx elm-codegen run --debug --flags-from=./theme/figma.json --output theme

gen:
	rm -rf generated/*
	node generate.js
	make setem
	-make theme
	make setem
	

.PHONY: openapi serve test format format-plugins lint lint-fix lint-ci build build-docker serve-docker gen theme
