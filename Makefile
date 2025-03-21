-include .env

API_ELM=openapi/src/Api.elm
REST_URL?=https://app.ikna.io
ELM_CODEGEN=./node_modules/.bin/elm-codegen run --debug 
FIGMA_WHITELIST_FRAMES?=[]

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
	$(ELM_CODEGEN) --flags='{"figma_file": "$(FIGMA_FILE_ID)", "api_key": "$(FIGMA_API_TOKEN)"}' --output theme
	make theme

theme: 
	{ echo '{"whitelist": {"frames": $(FIGMA_WHITELIST_FRAMES)}, "theme":'; cat ./theme/figma.json; echo '}'; } > ./theme/.gen.json
	$(ELM_CODEGEN) --output theme --flags-from=./theme/.gen.json
	rm ./theme/.gen.json

plugin-theme-refresh:
	$(ELM_CODEGEN) --flags='{"plugin_name": "$(PLUGIN_NAME)", "figma_file": "$(FIGMA_FILE_ID)", "api_key": "$(FIGMA_API_TOKEN)"}' --output plugins/$(PLUGIN_NAME)/theme
	make plugin-theme

plugin-theme:
	echo "{\"colormaps\": `cat ./theme/colormaps.json`, \"theme\": `cat ./plugins/$(PLUGIN_NAME)/theme/figma.json`}" > ./theme/.gen.json
	$(ELM_CODEGEN) --output theme --flags-from=./theme/.gen.json
	rm ./theme/.gen.json

gen:
	rm -rf generated/*
	cp elm.json.base elm.json
	make setem # for codegen/generated
	-node generate.js
	-make theme
	make setem # for theme related recordsetters
	

.PHONY: openapi serve test format format-plugins lint lint-fix lint-ci build build-docker serve-docker gen theme
