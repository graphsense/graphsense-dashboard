-include .env

API_ELM=openapi/src/Api.elm
REST_URL?=https://app.ikna.io
FIGMA_WHITELIST_FRAMES?=[]
CODEGEN_CONFIG=codegen/config/Config.elm
CODEGEN_RECORDSETTER=codegen/generated/RecordSetter.elm
CODEGEN_SRC=$(shell find codegen/src -name *.elm -type f)
FIGMA_JSON=./theme/figma.json
GENERATED=./generated
CODEGEN=./codegen
CODEGEN_GENERATED=$(CODEGEN)/$(GENERATED)
GENERATED_PLUGINS=$(GENERATED)/plugins
GENERATED_UTILS=$(GENERATED)/utils
GENERATED_THEME=$(GENERATED)/theme
GENERATED_THEME_THEME=$(GENERATED_THEME)/Theme
GENERATED_THEME_COLORMAPS=$(GENERATED_THEME)/colormaps.json
PLUGINS=$(shell find plugins -mindepth 1 -maxdepth 1 \( -type d -o -type l \) -exec basename {} \;)
CAPITALIZED_PLUGINS = $(foreach dir,$(PLUGINS),$(shell echo $(dir) | awk '{ $$1=toupper(substr($$1,1,1)) substr($$1,2); print }'))
THEME_GENERATED_MARKER=.generated
PLUGIN_INSTALLED_MARKER=.installed


serve: prepare
	make gen
	npm run dev

build: prepare
	make gen
	npm run build

install: 
	pip install pre-commit
	pre-commit install --hook-type pre-commit --hook-type pre-push

openapi:
	tools/generate-openapi.sh $(OPENAPI_LOCATION) $(REST_URL)
			#--global-property=debugModels \
			#--global-property=debugOperations \

clean:
	rm -rf ./elm-stuff/
	rm -rf ./dist/
	rm -rf $(GENERATED)
	rm -rf $(CODEGEN_GENERATED)
	rm -rf elm.json

setem: 
	npx setem --output $(GENERATED_UTILS)

setem-codegen: $(CODEGEN_RECORDSETTER)

$(CODEGEN_RECORDSETTER):
	cd $(CODEGEN) && mkdir -p $(GENERATED) && npx setem --output $(GENERATED)

test:
	npx elm-test-rs

prepare: 
	npm install
	make elm.json
	make plugins-install
	make $(GENERATED_THEME_COLORMAPS)
	make plugin-themes

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

theme: $(GENERATED_THEME_COLORMAPS)

theme-refresh: 
	rm $(FIGMA_JSON)
	make theme

$(FIGMA_JSON): 
	./tools/codegen.sh --refresh

$(GENERATED_THEME_COLORMAPS): $(FIGMA_JSON) $(CODEGEN_CONFIG) $(CODEGEN_SRC)
	make setem setem-codegen
	./tools/codegen.sh -w=$(FIGMA_WHITELIST_FRAMES)
	make setem

plugin-theme-refresh:
	./tools/codegen.sh --plugin=$(PLUGIN_NAME) --file-id=$(FIGMA_FILE_ID) --refresh 
	PLUGIN_NAME_CAPITALIZED=$(shell echo $(PLUGIN_NAME) | awk '{ $$1=toupper(substr($$1,1,1)) substr($$1,2); print }'); \
		rm -f $(GENERATED_THEME_THEME)/$$PLUGIN_NAME_CAPITALIZED/$(THEME_GENERATED_MARKER); \
		make PLUGIN_NAME=$$PLUGIN_NAME_CAPITALIZED plugin-theme

plugin-theme: 
	make $(GENERATED_THEME_THEME)/$(PLUGIN_NAME)/$(THEME_GENERATED_MARKER)
	make setem

$(GENERATED_THEME_THEME)/%/$(THEME_GENERATED_MARKER): $(CODEGEN_RECORDSETTER)
	./tools/codegen.sh --plugin=$(shell echo $* | tr '[:upper:]' '[:lower:]') --file-id=$(FIGMA_FILE_ID) 
	mkdir -p $(GENERATED_THEME_THEME)/$*
	touch $@

plugin-themes: $(CAPITALIZED_PLUGINS:%=$(GENERATED_THEME_THEME)/%/$(THEME_GENERATED_MARKER))
	make setem

$(GENERATED_PLUGINS)/%/$(PLUGIN_INSTALLED_MARKER): 
	while read dep; do yes | npx elm install $${dep}; done < plugins/$*/dependencies.txt
	cd plugins/$*; test -f plugins/$*/package.json && npm install || true
	mkdir -p $(GENERATED_PLUGINS)/$*
	touch $@

plugins-install: $(PLUGINS:%=$(GENERATED_PLUGINS)/%/.installed)

elm.json: elm.json.base
	cp elm.json.base elm.json
	mkdir -p $(GENERATED_THEME) $(GENERATED_UTILS) $(GENERATED_PLUGINS)

gen: 
	node generate.js
	make setem




.PHONY: openapi serve test format format-plugins lint lint-fix lint-ci build build-docker serve-docker gen theme-refresh
