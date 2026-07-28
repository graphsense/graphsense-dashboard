-include .env


API_ELM=openapi/src/Api.elm
REST_URL?=https://app.ikna.io
CONFIG=./config/Config.elm
CODEGEN_CONFIG=$(CODEGEN)/$(CONFIG)
FIGMA_JSON=./theme/figma.json
GENERATED=./generated
GENERATE_JS=tools/generate.js

CODEGEN=./codegen
CODEGEN_TOOL=npx ts-node ./tools/codegen.js
CODEGEN_GENERATED=$(CODEGEN)/$(GENERATED)
CODEGEN_RECORDSETTER=$(CODEGEN_GENERATED)/RecordSetter.elm
CODEGEN_SRC=$(shell find codegen/src -name *.elm -type f)

PLUGINS_DIR=./plugins

# `--` is the elm comment marker: skip commented-out plugin registrations. Pass it
# after `--` instead of escaping it; `\-` is not a valid escape and makes every
# single make invocation print "grep: warning: stray \ before -".
PLUGINS=$(shell grep -v -e '--' ${CONFIG} | sed -n 's/.*>\s*Plugin\.\([^} ]*\).*/\1/p' | awk '{print toupper(substr($$0,1,1)) substr($$0,2)}')
SRC_FILES=$(shell find src $(PLUGINS_DIR) -type f -name \*.elm -not -path '*/node_modules/*')
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

export ELM_HOME=$(PWD)/elm_packages
# The compiler keeps its packages in $(ELM_HOME)/<compiler version>/packages.
# Derive the version from the installed binary: a hardcoded path silently drops
# the patched packages of virtual-dom-fix on every compiler upgrade.
ELM_VERSION=$(shell npx elm --version)
ELM_PACKAGES_DIR=$(ELM_HOME)/$(ELM_VERSION)/packages

# A function only elm-safe-virtual-dom defines. Its absence from the kernel the
# compiler actually reads means the app is running unpatched (see virtual-dom-fix).
SAFE_VDOM_MARKER=_VirtualDom_createTNode
SAFE_VDOM_KERNEL=$(ELM_PACKAGES_DIR)/elm/virtual-dom/1.0.5/src/Elm/Kernel/VirtualDom.js

export NODE_OPTIONS=--max-old-space-size=8192

serve: prepare gen
	npm run dev

build: prepare gen
	npm run build

compile: prepare gen
	npm run compile

# Filters elm's progress chatter (stdout) while letting its error report (stderr)
# through untouched. The status of a pipeline is the status of its *last* command,
# so without PIPESTATUS this reports grep's: 1 whenever the filter leaves nothing
# to print, i.e. it failed on a successful compile. Needs bash for PIPESTATUS.
compile-quiet: SHELL := /bin/bash
compile-quiet:
	@$(MAKE) prepare gen > /dev/null
	@elm make src/Main.elm --output=/dev/null | tr '\r' '\n' | grep -v "^Compiling"; exit $${PIPESTATUS[0]}

check-plugin-folders:
	@bash -c 'cd $(PLUGINS_DIR); for i in *; do \
		if [ ! -e "$$i" ] && [ ! -L "$$i" ]; then \
			continue; \
		fi; \
		case "$$i" in \
			[A-Z]*) ;; \
			*) \
				echo "Plugins need to starts with an uppercase letter: $$i"; \
				echo "Run \"make fix-plugin-folders\" to fix."; \
				exit 1; \
				;; \
		esac; \
		done'

fix-plugin-folders:
	# ensure plugin folder start with uppercase letter
	bash -c 'cd $(PLUGINS_DIR); for i in *; do \
		if [ ! -e "$$i" ] && [ ! -L "$$i" ]; then \
			continue; \
		fi; \
		case "$$i" in \
			[A-Z]*) ;; \
			*) mv "$$i" "$${i^}" ;; \
		esac; \
		done'

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
	rm -rf $(ELM_PACKAGES_DIR)
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
	-rm $(FIGMA_JSON)

clean-plugin-figma-json:
	-rm $(PLUGINS_DIR)/$(PLUGIN_NAME)/$(FIGMA_JSON)

clean-public:
	rm -rf $(GENERATED_PUBLIC)

setem: $(RECORDSETTER_ELM)

$(RECORDSETTER_ELM): elm.json virtual-dom-fix $(SRC_FILES) $(GENERATED_THEME_COLORMAPS) $(PLUGINS:%=$(GENERATED_THEME_THEME)/%/$(THEME_GENERATED_MARKER)) $(GENERATED_PLUGIN_ELM)
	$(SETEM)

setem-codegen: $(CODEGEN_RECORDSETTER)

$(CODEGEN_RECORDSETTER): $(CODEGEN_SRC)
	cd $(CODEGEN); \
		mkdir -p $(GENERATED); \
		npx setem --output $(GENERATED) && touch $(GENERATED)/RecordSetter.elm

test: check-lang
	npx elm-test-rs

# Fails when a locale lost a key the code (or lang/en.yaml) still uses. Known
# gaps live in lang/untranslated-baseline.json; refresh it with
# `node tools/check_lang.mjs --update-baseline`.
check-lang:
	node tools/check_lang.mjs

# Regenerates tests/Fixtures/Api.elm from the response examples in the OpenAPI
# spec. The result is committed so `make test` needs no network; re-run this
# (and `make test`) after every `make openapi`.
api-fixtures:
	node tools/gen_api_fixtures.mjs $(OPENAPI_LOCATION)

prepare: check-plugin-folders node_modules elm.json virtual-dom-fix plugins-install theme plugin-themes

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
	$(CODEGEN_TOOL) --refresh --file-id=$(FIGMA_FILE_ID) --api-token=$(FIGMA_API_TOKEN)

$(GENERATED_THEME_COLORMAPS): $(FIGMA_JSON) $(CODEGEN_CONFIG) $(CODEGEN_SRC) $(CODEGEN_RECORDSETTER)
	/usr/bin/time -v $(CODEGEN_TOOL) -w="$(FIGMA_WHITELIST_FRAMES)" -c="$(FIGMA_WHITELIST_COMPONENTS)"

check-plugin-exists:
	@if [ ! -z "$(PLUGIN_NAME)" -a ! -e $(PLUGINS_DIR)/$(PLUGIN_NAME) ]; then \
		echo "$(PLUGIN_NAME) does not exists"; \
		exit 1; \
	fi

plugin-theme-refresh: 
	$(CODEGEN_TOOL) --plugin=$(PLUGIN_NAME) --file-id=$(FIGMA_FILE_ID) --refresh --api-token=$(FIGMA_API_TOKEN)

$(PLUGINS_DIR)/%/$(FIGMA_JSON):
	@# only update an existing figma.json
	if [ -e $(PLUGINS_DIR)/%/$(FIGMA_JSON) ]; then \
		$(CODEGEN_TOOL) --plugin=$* --file-id=$(FIGMA_FILE_ID) --api-token=$(FIGMA_API_TOKEN) --refresh; \
	fi

plugin-theme: check-plugin-exists $(GENERATED_THEME_THEME)/$(PLUGIN_NAME)/$(THEME_GENERATED_MARKER) setem

$(GENERATED_THEME_THEME)/%/$(THEME_GENERATED_MARKER): $(GENERATED_THEME_COLORMAPS) $(CODEGEN_RECORDSETTER) $(PLUGINS_DIR)/%/$(FIGMA_JSON)
	/usr/bin/time -v $(CODEGEN_TOOL) --plugin=$* 
	mkdir -p $(GENERATED_THEME_THEME)/$*
	touch $@

plugin-themes: $(PLUGINS:%=$(GENERATED_THEME_THEME)/%/$(THEME_GENERATED_MARKER)) setem

# `elm install` adds the plugin's dependencies to elm.json in place, so this
# marker has to be invalidated by everything that can drop them again:
#   - the elm.json target, which copies elm.json.base over elm.json and resets it
#     to a state without any plugin dependency. It deletes these markers itself,
#     right where it does the copy: keying the invalidation off elm.json.base's
#     mtime instead would miss every other reason elm.json gets regenerated.
#   - the plugin's own elm.json: its dependency list is what gets installed here.
# Do NOT depend on the generated elm.json: each plugin's install rewrites it,
# which would invalidate the markers of the plugins installed before it and
# reinstall everything on every build. It is an order-only prerequisite, so the
# copy is guaranteed to happen before the install even under `make -j`.
$(GENERATED_PLUGINS)/%/$(PLUGIN_INSTALLED_MARKER): $(PLUGINS_DIR)/%/elm.json | elm.json
	jq -r '.dependencies | keys[]' $(PLUGINS_DIR)/$*/elm.json \
		| while read dep; do \
			yes | npx elm install $$dep || exit 1; \
		done
	cd $(PLUGINS_DIR)/$*; test -f package.json && npm install || true
	mkdir -p $(GENERATED_PLUGINS)/$*
	touch $@

plugins-install: $(PLUGINS:%=$(GENERATED_PLUGINS)/%/$(PLUGIN_INSTALLED_MARKER))

elm.json: elm.json.base
	cp elm.json.base elm.json
	# The copy wipes the plugin dependencies that plugins-install added to elm.json
	# with `elm install`; drop the markers so that it re-adds them.
	rm -f $(GENERATED_PLUGINS)/*/$(PLUGIN_INSTALLED_MARKER)
	mkdir -p $(GENERATED_THEME) $(GENERATED_UTILS) $(GENERATED_PLUGINS)

gen: copy-public $(GENERATED_PLUGIN_ELM) setem

$(GENERATED_PLUGIN_ELM): elm.json $(GENERATE_JS) $(CONFIG) $(PLUGIN_TEMPLATES) $(wildcard ./lang/*) $(wildcard $(PLUGINS_DIR)/*/lang/*)
	node $(GENERATE_JS) $(PLUGINS) 

# Mirror ./public and every plugin's public/ into generated/public. A `cp -r` into
# the already existing target nested the whole tree a second time as
# generated/public/public, and neither cp nor a plain rsync ever removed anything:
# an asset deleted on another branch survived in generated/public and shipped.
# One rsync over all sources at once, so --delete only removes what no source
# provides (a --delete per source would delete the files of the other sources).
# lang/ is excluded because generate.js owns it: it writes the core translations
# merged with the plugin ones there, and it does not run on every `make gen`.
copy-public:
	@mkdir -p $(GENERATED_PUBLIC)
	@srcs="$(PUBLIC_DIR)/"; \
	for p in $(PLUGINS); do \
		if [ -d $(PLUGINS_DIR)/$$p/$(PUBLIC_DIR) ]; then \
			srcs="$$srcs $(PLUGINS_DIR)/$$p/$(PUBLIC_DIR)/"; \
		fi; \
	done; \
	echo rsync -rlt --delete --exclude=/lang/ $$srcs $(GENERATED_PUBLIC)/; \
	rsync -rlt --delete --exclude=/lang/ $$srcs $(GENERATED_PUBLIC)/

print-plugins:
	@echo $(PLUGINS)

# Clones a patched package into the elm package cache, replacing whatever is
# there unless it already is the pinned commit. Anything else is either a
# package elm downloaded from the registry (the unpatched one) or an outdated
# clone. elm-stuff caches compiled dependencies by package version, not by
# content, so it has to go too or the build keeps the unpatched kernel.
define clone-repo
	if [ "$$(git -C $(4) rev-parse HEAD 2>/dev/null)" != "$(3)" ]; then \
		rm -rf $(4) elm-stuff; \
		git clone --depth=1 --branch=$(2) https://github.com/$(1) $(4) && \
		cd $(4) && \
		git reset --hard $(3) && \
		git clean -df; \
	fi
endef

virtual-dom-fix:
	mkdir -p $(ELM_PACKAGES_DIR)
	$(call clone-repo,omnibs/elm-css,safe,e54998ce73b64c374b1457d5734c85d3f5b909fb,$(ELM_PACKAGES_DIR)/rtfeldman/elm-css/18.0.0)
	$(call clone-repo,lydell/html,safe,b35c476a69f0ba9bf8282d8c15df65e63aefea8f,$(ELM_PACKAGES_DIR)/elm/html/1.0.1)
	$(call clone-repo,lydell/virtual-dom,safe,e1fae6aabd65539db2c94a98220a45cfc624b633,$(ELM_PACKAGES_DIR)/elm/virtual-dom/1.0.5)
	$(call clone-repo,lydell/browser,safe,f5de544c8033d934285501f78f09e2eaf0171d55,$(ELM_PACKAGES_DIR)/elm/browser/1.0.2)
	@$(MAKE) --no-print-directory check-virtual-dom-fix

# Fails the build when the compiler would read an unpatched virtual-dom. That is
# what happens when the clones above land in a package directory of a different
# elm version, or when elm re-downloads the registry package over them.
check-virtual-dom-fix:
	@grep -q '$(SAFE_VDOM_MARKER)' '$(SAFE_VDOM_KERNEL)' 2>/dev/null || { \
		echo ''; \
		echo 'ERROR: elm-safe-virtual-dom is missing from the packages of elm $(ELM_VERSION).'; \
		echo '       Expected the patched kernel in:'; \
		echo '         $(SAFE_VDOM_KERNEL)'; \
		echo '       Without it the app crashes whenever a browser extension (or anything'; \
		echo '       else) touches the DOM: "Node.removeChild: Argument 1 is not an object".'; \
		echo '       Remove that package directory and re-run `make virtual-dom-fix`.'; \
		echo ''; \
		exit 1; \
	}

# Target to create a version tag and commit
# Usage: make tag-version VERSION=v1.0.0
tag-version:
	@echo "Setting version to $(VERSION)"
	@echo "module Version exposing (version)" > src/Version.elm
	@echo "" >> src/Version.elm
	@echo "" >> src/Version.elm
	@echo "version : String" >> src/Version.elm
	@echo "version =" >> src/Version.elm
	@echo "    \"$(VERSION)\"" >> src/Version.elm
	git add src/Version.elm
	git commit -m "$(VERSION)" -n
	git tag $(VERSION)
	@echo "Created version $(VERSION)"

.PHONY: openapi serve test check-lang api-fixtures format format-plugins lint lint-fix lint-ci build build-docker serve-docker gen theme-refresh virtual-dom-fix tag-version compile-quiet
