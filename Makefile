-include .env

API_ELM=openapi/src/Api.elm
REST_URL?=https://app.ikna.io
FIGMA_WHITELIST_FRAMES?=[]
CODEGEN_CONFIG=codegen/config/Config.elm

install:
	pip install pre-commit
	pre-commit install --hook-type pre-commit --hook-type pre-push
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
	npx setem --output generated/utils
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

$(CODEGEN_CONFIG):
	[ ! -e $(CODEGEN_CONFIG) ] && cp $(CODEGEN_CONFIG).tmp $(CODEGEN_CONFIG)

theme-refresh: 
	./tools/codegen.sh --refresh
	make theme

theme: $(CODEGEN_CONFIG)
	./tools/codegen.sh -w=$(FIGMA_WHITELIST_FRAMES)
	make setem

plugin-theme-refresh:
	./tools/codegen.sh --plugin=$(PLUGIN_NAME) --file-id=$(FIGMA_FILE_ID) --refresh 
	make plugin-theme

plugin-theme: $(CODEGEN_CONFIG)
	./tools/codegen.sh --plugin=$(PLUGIN_NAME) --file-id=$(FIGMA_FILE_ID)
	make setem

gen:
	rm -rf generated/*
	cp elm.json.base elm.json
	make setem # for codegen/generated
	-node generate.js
	-make theme
	

.PHONY: openapi serve test format format-plugins lint lint-fix lint-ci build build-docker serve-docker gen theme
