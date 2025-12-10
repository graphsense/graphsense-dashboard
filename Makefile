-include .env


API_ELM=openapi/src/Api.elm
REST_URL?=https://app.ikna.io
FIGMA_WHITELIST_FRAMES?=[]
CONFIG=./config/Config.elm
CODEGEN_CONFIG=$(CODEGEN)/$(CONFIG)
FIGMA_JSON=./theme/figma.json
GENERATED=./generated
GENERATE_JS=tools/generate.js

CODEGEN=./codegen
CODEGEN_GENERATED=$(CODEGEN)/$(GENERATED)
CODEGEN_RECORDSETTER=$(CODEGEN_GENERATED)/RecordSetter.elm
CODEGEN_SRC=$(shell find codegen/src -name *.elm -type f)

PLUGINS_DIR=./plugins

PLUGINS=$(shell grep "|> Plugin" $(CONFIG) | grep -v "\-\-" | sed -r 's/\|>\s*Plugin\.\w+\s+\(?(\w+)\)?\..*/\1/' | tr -s ' ')
SRC_FILES=$(shell find src $(PLUGINS_DIR) -type f -name \*.elm)
PLUGIN_TEMPLATES=$(shell find plugin_templates -type f -name \*.mustache)

GENERATED_PLUGINS=$(GENERATED)/$(PLUGINS_DIR)
GENERATED_PLUGIN_ELM=$(GENERATED_PLUGINS)/Plugin.elm
GENERATED_UTILS=$(GENERATED)/utils
GENERATED_THEME=$(GENERATED)/theme
GENERATED_PUBLIC=$(GENERATED)/$(PUBLIC_DIR)
GENERATED_LANG=$(GENERATED_PUBLIC)/lang
GENERATED_THEME_THEME=$(GENERATED_THEME)/Theme
GENERATED_THEME_COLORMAPS=$(GENERATED_THEME)/colormaps.json

RECORDSETTER_ELM=$(GENERATED_UTILS)/RecordSetter.elm

THEME_GENERATED_MARKER=.generated
PLUGIN_INSTALLED_MARKER=.installed

PUBLIC_DIR=./public
PUBLIC_FILES=$(shell find $(PUBLIC_DIR) -type f)

SETEM=npx setem --output $(GENERATED_UTILS) && touch $(RECORDSETTER_ELM)

serve: prepare gen
	npm run dev

build: prepare gen
	npm run build

check-plugin-folders:
	@bash -c 'cd $(PLUGINS_DIR); for i in `ls -1`; do \
		if [ ! -e "$${i^}" ]; then \
			echo "Plugins need to starts with an uppercase letter: $$i"; \
			echo "Run \"make fix-plugin-folders\" to fix."; \
			exit 1; \
		fi; done'

fix-plugin-folders:
	# ensure plugin folder start with uppercase letter
	bash -c 'cd $(PLUGINS_DIR); for i in `ls -1`; do [ -e "$${i^}" ] || mv $$i $${i^}; done'

install: 
	pre-commit install --hook-type pre-commit --hook-type pre-push

node_modules: package.json
	npm install

openapi:
	tools/generate-openapi.sh $(OPENAPI_LOCATION) $(REST_URL)
			#--global-property=debugModels \
			#--global-property=debugOperations \

clean-all: clean-install clean-generated
	rm -rf ./dist/

clean-install:
	rm -rf node_modules
	rm -rf ./elm-stuff

clean-generated: clean-generated-themes clean-generated-plugins clean-generated-utils

clean-generated-themes:
	rm -rf $(GENERATED_THEME)

clean-generated-plugins:
	rm -rf $(GENERATED_PLUGINS)
	rm -rf elm.json

clean-generated-utils:
	rm -rf $(GENERATED_UTILS)
	rm -rf $(CODEGEN_GENERATED)

clean-figma-json:
	rm $(FIGMA_JSON)

clean-plugin-figma-json:
	rm $(PLUGINS_DIR)/$(PLUGIN_NAME)/$(FIGMA_JSON)

clean-public:
	rm -rf $(GENERATED_PUBLIC)

setem: $(RECORDSETTER_ELM)

$(RECORDSETTER_ELM): elm.json $(SRC_FILES) $(GENERATED_THEME_COLORMAPS) $(PLUGINS:%=$(GENERATED_THEME_THEME)/%/$(THEME_GENERATED_MARKER)) $(GENERATED_PLUGIN_ELM)
	$(SETEM)

setem-codegen: $(CODEGEN_RECORDSETTER)

$(CODEGEN_RECORDSETTER): $(CODEGEN_SRC)
	cd $(CODEGEN); \
		mkdir -p $(GENERATED); \
		npx setem --output $(GENERATED) && touch $(GENERATED)/RecordSetter.elm

test:
	npx elm-test-rs

prepare: check-plugin-folders node_modules elm.json plugins-install theme plugin-themes

build-docker:
	docker build . -t graphsense-dashboard

serve-docker: build-docker
	docker run -it --network='host' -e REST_URL=http://localhost:9000 localhost/graphsense-dashboard:latest

format:
	npx elm-format --yes src tests 

format-plugins:
	npx elm-format --yes $(PLUGINS_DIR)

lint: 
	npx elm-review

lint-fix: 
	npx elm-review --fix-all

lint-plugins:
	@for p in $(PLUGINS); do \
		if [ -e $(PLUGINS_DIR)/$$p/elm.json -a -e $(PLUGINS_DIR)/$$p/review ]; then \
			echo "Linting $$p ..."; \
			cd plugins/$$p; npx elm-review; cd -; \
		fi \
	done

$(CODEGEN_CONFIG):
	[ ! -e $(CODEGEN_CONFIG) ] && cp $(CODEGEN_CONFIG).tmp $(CODEGEN_CONFIG)

theme: $(GENERATED_THEME_COLORMAPS) setem

theme-refresh: clean-figma-json theme

$(FIGMA_JSON): 
	./tools/codegen.sh --refresh

$(GENERATED_THEME_COLORMAPS): $(FIGMA_JSON) $(CODEGEN_CONFIG) $(CODEGEN_SRC) $(CODEGEN_RECORDSETTER)
	./tools/codegen.sh -w=$(FIGMA_WHITELIST_FRAMES)

check-plugin-exists:
	@if [ ! -z "$(PLUGIN_NAME)" -a ! -e $(PLUGINS_DIR)/$(PLUGIN_NAME) ]; then \
		echo "$(PLUGIN_NAME) does not exists"; \
		exit 1; \
	fi

plugin-theme-refresh: 
	./tools/codegen.sh --plugin=$(PLUGIN_NAME) --file-id=$(FIGMA_FILE_ID) --refresh

$(PLUGINS_DIR)/%/$(FIGMA_JSON):
	@# only update an existing figma.json
	if [ -e $(PLUGINS_DIR)/%/$(FIGMA_JSON) ]; then \
		./tools/codegen.sh --plugin=$* --file-id=$(FIGMA_FILE_ID) --refresh; \
	fi

plugin-theme: check-plugin-exists $(GENERATED_THEME_THEME)/$(PLUGIN_NAME)/$(THEME_GENERATED_MARKER) setem

$(GENERATED_THEME_THEME)/%/$(THEME_GENERATED_MARKER): $(GENERATED_THEME_COLORMAPS) $(CODEGEN_RECORDSETTER) $(PLUGINS_DIR)/%/$(FIGMA_JSON)
	./tools/codegen.sh --plugin=$* 
	mkdir -p $(GENERATED_THEME_THEME)/$*
	touch $@

plugin-themes: $(PLUGINS:%=$(GENERATED_THEME_THEME)/%/$(THEME_GENERATED_MARKER)) setem

$(GENERATED_PLUGINS)/%/$(PLUGIN_INSTALLED_MARKER): 
	jq -r '.dependencies | keys[]' $(PLUGINS_DIR)/$*/elm.json \
		| while read dep; do \
			yes | npx elm install $$dep; \
		done 
	cd $(PLUGINS_DIR)/$*; test -f package.json && npm install || true
	mkdir -p $(GENERATED_PLUGINS)/$*
	touch $@

plugins-install: $(PLUGINS:%=$(GENERATED_PLUGINS)/%/$(PLUGIN_INSTALLED_MARKER))

elm.json: elm.json.base
	cp elm.json.base elm.json
	mkdir -p $(GENERATED_THEME) $(GENERATED_UTILS) $(GENERATED_PLUGINS)

gen: copy-public $(GENERATED_PLUGIN_ELM) setem

$(GENERATED_PLUGIN_ELM): elm.json $(GENERATE_JS) $(CONFIG) $(PLUGIN_TEMPLATES) $(wildcard ./lang/*) $(wildcard $(PLUGINS_DIR)/*/lang/*)
	node $(GENERATE_JS) $(PLUGINS)

copy-public: 
	cp -r $(PUBLIC_DIR) $(GENERATED_PUBLIC)
	for p in $(PLUGINS); do rsync -r $(PLUGINS_DIR)/$$p/$(PUBLIC_DIR)/ $(GENERATED_PUBLIC)/; done


.PHONY: openapi serve test format format-plugins lint lint-fix lint-ci build build-docker serve-docker gen theme-refresh 
