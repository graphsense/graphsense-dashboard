# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [25.11.5] - 2025-12-16

### Fixed

- Fix data loading issue in side panel sub transaction table
 
## [25.11.4] - 2025-12-09

### Fixed

- Fix data loading issue in side panel tables and improve caching

## [25.11.3] - 2025-12-04

### Fixed

- reset url if address is removed

## [25.11.2] - 2025-11-21

### Fixed

- improve csv download message, add italian
- fetch tag summeries for csv export
- merge utxo inputs/outputs into csv output
- extended warning for add tag
- allow canceling request on table loading avoiding loading data into table from older requests
- include best cluster tag in neighbor table tag summary request


## [25.11.1] - 2025-11-10

### Fixed

- Multiselect tool only works with an existing address selection
- Closing of transaction table in address side panel

## [25.11.0] - 2025-11-07

### Added

- add new tag type attribute hover description
- add csv download to relation txs and address txs tables
- show tx hash option
- UI - Add note to Cluster info that info is based on multiple-input heuristics
- add help to tracing mode switch
- Tagging - Some users mistake reporting a tag by connecting to case: added additional warning
- UI - Add Retirement warning to PF1

### Changed

- use dash in date range filter
- rename relations->relationships, Relation->Beziehung
- sync url with transaction table
- faster actor search
- improve possible service handling
- UI - make toolbar components stack on small screens
- UI - Improve search feedback
- UI - Shorten Tx hash in Search
- UI - Rename tx/agg toggle to Transaction level/Relation level
- improve tx placement, disable autolink for nbrsearch and multiadd
- Use includeSubTxIdentifiers field in search endpoint

### Fixed

- fix table filters
- only show loading spinner if table is really loading
- fix: assets filter incomplete
- set scrollTop 0 on InfiniteTable.reset
- show daterangepicker button if not dates set
- fix: reset all filter in tx table
- fix missing tooltip in relations table
- fix: open in new tab only active on single selection
- fix search bar highlighting
- fix deletion of conversion edges


## [25.09.3] - 2025-10-03

### Fixed
- path endpoint was casing sensitive
- better handling for eth addresses with and without 0x prefix
- tag loading issue (new tagsummary data did not overwrite old tags.)

## [25.09.2] - 2025-09-23

### Fixed
- loading token transactions from relation table
- show same value in neighbors table as on relation edge label 
- loading infinite table with only one item which is filtered
- showing tooltips from plugins

### Changed
- show cluster size number in data tab
- improve preselecting the related addresses table type


## [25.09.1] - 2025-09-08

### Fixed
- show whole graph on loading from .gs file
- show infinite table after closing/opening the table tab in the side panel

## [25.09.0] - 2025-09-05

### Added
- edge shadow on transactions
- sorting to timestamp columns
- swap and bridge transaction support (rendered as special edges)
- new address clustering (by public key), now displayed in the clustered addresses dropdown table.

### Changed
- auto remove dangling addresses on unchecking all txs
- made tables "infinitely" scrollable
- Overhauled Tx Details Panel in Account model currencies (now show base tx and a list of sub txs, instead of the sub tx directly)

### Fixed
- fix missing translation
- infinite table with sorting
- rendering of self loops in account model currencies
- fixed ambiguous handling of sub tx hashes (could lead to users being able to add the same payment twice to the graph)

## [25.08.1] - 2025-08-07

### Added

- add hint to report tag icon

### Changed

- Minor label changes
- reporting tags form: hide actor field description if filled

### Fixed

- remove sort handle on neighbor value column
- fix hovering agg edges


## [25.08.0] - 2025-08-01

### Added
- Aggregated tracing mode allowing to get a overview of the interactions between addresses (complementing the existing tracking mode)
- Automatically find transactions between added addresses in transaction tracing mode


### Fixed
- Mac shortcut handling (Meta keyup related problems) never leaving ModKey mode
- Backspace node removal handling in textareas (accidental removal of nodes)
- Disable side panel tabs if there is no data
- color whole path in transaction annotation color
- make values in account address txs absolute, fix #505
- decrease zoom step by zoomfactor, fix #503
- make datepicker focus the latest date, on relation txs the latest tx
- fix fiat currency switch
- fix path fill with none instead of transparent

## [25.07.0] - 2025-06-23

### Added
- tag reporting fearture, users can now submit new tags if something is not annotated correctly.


## [25.06.1] - 2025-06-13

### Added
- route to prefilter tx table (e.g. pathfinder/{network}/address/{address}/transactions?from=2016-03-10T14:00:00Z&to=2016-03-10T16:00:00Z) (closes #499)

## [25.06.0] - 2025-06-06

### Added
- Legend/help to pathfinder 2
- advanced filtering of transaction table (in, out, asset type in account model currencies)
- Add multiple txs (page) to graph for input/output table and tx table

## [25.05.5] - 2025-06-03

### Fixed

- 6b691bd1 refactor addressesAdded/entitiesAdded hooks

## [25.05.4] - 2025-05-30

### Fixed

- 64460e2b only check addresses of same network for relations

## [25.05.3] - 2025-05-28

### Changed

- 4c9f7b06 auto remove dangling addresses on unchecking txs in table
- 174896dc auto place neighbors in graph, fix #464, fix #478

### Fixed

- bc6e4a09 correct auto-expand for following assets of token transactions, fix #475

## [25.05.2] - 2025-05-15

### Fixed

- 505fdbab check shallPushHistory for AddressDetailsMsgs
- d4f3f8b7 fix selection box
- 5b3d1b54 scrollable token list
- 801e2c60 fix actor label display on address 

## [25.05.1] - 2025-05-07

### Fixed

- Annotation label style
- Minor layout issues in side panel

## [25.05.0] - 2025-05-02

### Fixed

- d2999371 go to root Pathfinder url on restart
- c84bfe61 make button behave like button
- 53e0f10a add a cache bust hash to translation yaml url
- b105ba87 fix actor display
- be41e47d fix tx hash/identifier ambiguity (closes #476)

### Changed

- 6522743d show tagged addresses and cluster addresses in one table
- 90fe99f4 extend sidepanel maxheight
- e5dda9e5 increse notification delay
- 23ada568 reordered Pathfinder settings
- f3896dc4 new token display in eth address details (closes #470)
- 1643364f allow multiple paths in show path plugin interface, select address nodes on new path

### Added

- b77aedd3 plugins elements in side panel header placed in row

## [25.04.2] - 2025-04-28

### Added

- Cache busting hash to translation request urls.

## [25.04.1] - 2025-04-08

### Added

- Checkbox component

### Changed

- Shorten tooltip display
- Plugin build solely depends on configured plugins in config/Config.elm now
- Improved codegen memory consumption

### Fixed

- Fixed Makefile targets plugin-theme-refresh, check-plugin-exists

## [25.04.0] - 2025-04-03

### Added

- "More" tab with Pathfinder 1.0 and help links
- Related addresses table in address details view (tagged/all cluster addresses)
- Success notifications
- Plugin hooks for new Pathfinder, new message based update hook
- Display rate limiting period in settings

### Changed

- Moved Pathfinder 1.0 to "More" tab
- Numerous minor design patches and bug fixes
- Plugins in plugins folder need to be capitialized
- Improved the Makefile and build chain
- Keep transactions table closed on address selection

### Fixed

- App remains usable after network issue
- Trigger search automatically when using one of the examples on the landing page

## [24.01.5] - 2025-01-24

### added
- tag confidence indicator in tags list are now also colored.

### fixed
- tags list table in pathfinder overflowing on small screens.

## [24.01.4] - 2025-01-20

### fixed
- fixed some color issues in the dark theme

## [24.01.3] - 2025-01-20

### fixed
- to short address digest for unique search

## [24.01.2] - 2025-01-14

### fixed
- fixed missing categories in pathsearch (Pathfinder 1)

## [24.01.1] - 2025-01-10

### fixed
- incoorperated upstream fixes

## [24.01.0] - 2025-01-10

### Changed
- changed tagging appearance in Pathfind 2.0 now showing categories instead of labels
- include new tagpack 25.01.0 schema changes (tag_type)

## Added
- new tags list in Pathfinder 2.0 look and feel

## [24.11.0] - 2024-11-20

### Added
- Pathfinder 2.0 for transaction level tracing

### Changed
- Color scheme, side panel and user settings overhauled


## [24.06.0] - 2024-06-28

### Added
- precommit hook to set version automatically

### Changed

- better auto complete lib (needed for some plugins)

### Fixed

- Fixed loading eth address path

## [24.04.0] - 2024-04-05

### Changed

- Hard-coded color scheme mapping for tag categories.
- Removed duplicated (static) fields in block transaction table

### Fixed

- Fixed jumping to graph after loading gs file from landing-page
- Fixed button alignment in confirm dialog
- Fixed search with trailing zero
- Fixed overlapping addresses in search

## [24.02.1] - 2024-02-29

### Added

- Disabled style for switch knob

### Changed

- Keep custom color on tool icon on hovering

## [24.02.0] - 2024-02-29

### Added
- Added address path route functions in `Route.Graph`
- Added functions for date and time formatting to `View.Locale`
- Added `Update.Config` parameter to `PluginInterface.Update.update` and `updateByUrl`

## [24.01.1] - 2024-02-27

### Fixed

- Layering of property box (z-index)

## [24.01.0] - 2024-01-10
### Added
- beta support for the tron currency ([382](https://github.com/graphsense/graphsense-dashboard/issues/382))
- added supported tokens on statistic page ([382](https://github.com/graphsense/graphsense-dashboard/issues/382))
- added support for missing current exchange rates (for sync states before exchange rates where avail.)
- Allow for opening a node in new browser tab ([426](https://github.com/graphsense/graphsense-dashboard/issues/426))
- new setting to filter zero value transactions (often smart contract calls in ethereum)
- Italian translation
- add option to open address in new tab ([426](https://github.com/graphsense/graphsense-dashboard/issues/426))
### Changed
- improved viewport centering when adding new nodes to the graph ([421](https://github.com/graphsense/graphsense-dashboard/issues/421))
- keep browser table open when switching nodes ([422](https://github.com/graphsense/graphsense-dashboard/issues/422))
- improved display for nodes/edges with multi-currency transaction (token transaction) ([377](https://github.com/graphsense/graphsense-dashboard/issues/377))
- improved placing of popup dialogs ([407](https://github.com/graphsense/graphsense-dashboard/issues/407))
- open user profile on click only (instead of hovering)
- upgrade docker image to alpine:3.19
- add new color to highlighter on picking instead of updating existing coloring ([419](https://github.com/graphsense/graphsense-dashboard/issues/419))
### Fixed
- skip existing entities in path search ([429](https://github.com/graphsense/graphsense-dashboard/issues/429))
- Language settings get lost after login ([427](https://github.com/graphsense/graphsense-dashboard/issues/427))
- search parameter editable via keyboard ([428](https://github.com/graphsense/graphsense-dashboard/issues/428))
- fixed multiline node labels ([416](https://github.com/graphsense/graphsense-dashboard/issues/416))
- fix positioning of contextmenu, ([424](https://github.com/graphsense/graphsense-dashboard/issues/424))
- add total received address label type in graph configuration ([438](https://github.com/graphsense/graphsense-dashboard/issues/438))
- add history entries on url change ([431](https://github.com/graphsense/graphsense-dashboard/issues/431))
- preserve graph url when switching to other tab via sidebar ([403](https://github.com/graphsense/graphsense-dashboard/issues/403))
- also keep highlight color trashing in history ([434](https://github.com/graphsense/graphsense-dashboard/issues/434))
- allow changing search params with keyboard ([428](https://github.com/graphsense/graphsense-dashboard/issues/428))
- preserve propertybox table when switching things ([422](https://github.com/graphsense/graphsense-dashboard/issues/422))

## [23.09] - 2023-09-20
### Added
- new more user friendly landing page, statistics is now found in the left menu
- New setting to select in which timezone dates are show (user, UTC) [#408](https://github.com/graphsense/graphsense-dashboard/issues/408)
- User settings are now preserved in localStorage [#399](https://github.com/graphsense/graphsense-dashboard/issues/399)
- Api key field is now type password to enhance pw-manager integration [#409](https://github.com/graphsense/graphsense-dashboard/issues/409)
- Add links to blockexplorer on tx views [#381](https://github.com/graphsense/graphsense-dashboard/issues/381)
- Added context menu options to property boxes [#353](https://github.com/graphsense/graphsense-dashboard/issues/353)

### Changes
- improved handling plugin handling and code generation
- clear search after item was selected [#410](https://github.com/graphsense/graphsense-dashboard/issues/410)
- avoid inserting self referential neighbor links [#378](https://github.com/graphsense/graphsense-dashboard/issues/378)
- fixed undo history [#393](https://github.com/graphsense/graphsense-dashboard/issues/393)
- fixed search with space chars [#396](https://github.com/graphsense/graphsense-dashboard/issues/396)
- fixed show warning on close only if graph is dirty [#370](https://github.com/graphsense/graphsense-dashboard/issues/370)
- fixed nodes out of viewport on inserting long paths [#394](https://github.com/graphsense/graphsense-dashboard/issues/394)
- fixed text overlap on long entity labels [#406](https://github.com/graphsense/graphsense-dashboard/issues/406)
- fixed confusing large integers (satoshis, Wei) on csv export [#389](https://github.com/graphsense/graphsense-dashboard/issues/389) 
- fixed allow search for txhashes with leading 0x


## [23.06] - 2023-06-12
### Added
- Add address path link that allows importing an entire address path to the graph
- Coingecko contribution statement
### Changes
- set light mode as default
- update iknaio theme to match new CI
- Display all identifiers uniformly (shorted) [#386](https://github.com/graphsense/graphsense-dashboard/issues/386), [#385](https://github.com/graphsense/graphsense-dashboard/issues/385)
- Show confidence label instead of numeric level [#376](https://github.com/graphsense/graphsense-dashboard/issues/376)
- Fix min height of property box tables [#374](https://github.com/graphsense/graphsense-dashboard/issues/374)
- Fix resize behavior on graph centering [#388](https://github.com/graphsense/graphsense-dashboard/issues/388)
- Improved display of large numbers, added new setting [#383](https://github.com/graphsense/graphsense-dashboard/issues/383)

## [23.03] - 2023-03-28
### Added
- Actor browser to show actor details. [#369](https://github.com/graphsense/graphsense-dashboard/issues/369)
- Entities and Addresses can be liked to actors. [#369](https://github.com/graphsense/graphsense-dashboard/issues/369)
- added links to external tools (block explorers etc.) for addresses [#290](https://github.com/graphsense/graphsense-dashboard/issues/290)
- search for any category [#329](https://github.com/graphsense/graphsense-dashboard/issues/329)
### Changes
- Improved address display [#359](https://github.com/graphsense/graphsense-dashboard/issues/359)
- Improved currency value display [#371](https://github.com/graphsense/graphsense-dashboard/issues/371)

## [23.01] - 2023-01-30
### Added
- add plugin logout hook
- display error popup on address not found
- allow decoding of 0.4.5 and 0.4.4 gs files

### Changes
- make all tables initially unsorted (leverage responses order)
- hide table toolbar if no tools
- fix browser and footer transition
- lighten default node color
- fix alignment of new entities with anchors
- fix syncing links between entities on adding entities via url
- refresh table height on browser content load
- fix url navigation after invalid url, [#366](https://github.com/graphsense/graphsense-dashboard/issues/366)
- remove entity column from address tags table, [#306](https://github.com/graphsense/graphsense-dashboard/issues/306)
- fix highlighter layout
- improve node coloring in lightmode, [#348](https://github.com/graphsense/graphsense-dashboard/issues/348)
- improve proprietary tag label position
- improve highlighter, underline selected color, [#303](https://github.com/graphsense/graphsense-dashboard/issues/303)
- close highlighter by clicking somewhere, [#304](https://github.com/graphsense/graphsense-dashboard/issues/304)
- fix displaced labels in safari, [#361](https://github.com/graphsense/graphsense-dashboard/issues/361)
- fix [#367](https://github.com/graphsense/graphsense-dashboard/issues/367)
- improve messages
- fix link label background alignment
- rename neighbors column for account coins
- show token values in entity prop box
- disambiguate token currencies when converted
- display tokens on edge labels
- sticky table headers
- improve token values display in neighbors column, hide 0 values
- display tokens in neighbor tables
- show token txs table, create correct urls for links
- align total received/balance values
- display tokens in tx account table
- display tokens values in address prop box, display smart contract flag
- improve documentation of plugin development and hooks
- load address connections on deserializing
- dont jump to graph root url on deserializing
- allow plugins to show dialogs
- show "proprietary tag" label in tags table


## [22.11] - 2022-11-25
### Changes
- have address tags table initially unsorted, so server side sorting is effective
- improve search/autocomplete ux, [#362](https://github.com/graphsense/graphsense-dashboard/issues/362)
- fix concept loading message translation
- improve "not found" error message, [#336](https://github.com/graphsense/graphsense-dashboard/issues/336)
- remove "entity for address not found" message, if there is a "address not found" message already
- fix submit forms with enter
- fix translation of "search neighbors" statusbar message
- fix tag color assignment
- allow csv download of all tables, [#349](https://github.com/graphsense/graphsense-dashboard/349)
- performance improvements
- improve placing of plugin flags
- pre-select currency in graph config, [#355](https://github.com/graphsense/graphsense-dashboard/355)
- limit neighbor search depth, change default depth to 2, [#357](https://github.com/graphsense/graphsense-dashboard/357)
- fix min table height
- run npm install for plugins
- display stats timestamp in utc
### Added
- add icon to log messages
- merge plugin translations, remove unused strings with %, [#300](https://github.com/graphsense/graphsense-dashboard/300)

## [22.10] - 2022-10-10
### Added
- delete address, entities, links with DEL key #330
- show entity tags table on click on tags flag #334
- address shadow links
- option to show/hide address/entity shadow links #350
- automatically connect addresses #339
- display whether address is new and has no statistics yet calculated, #347
- plugin title hook
- plugin hooks for port in and out msgs
- show version number in statusbar

### Changed
- fix highlighter selecton on new/deserialize
- fix pick label from tag label search on annotation
- fix decoding null values in tagpack file
- hide entity links if there are address links, optionally show them  #352, #335
- load addresses one by one if entered in bulk in search #339
- show specific error message if eth address has no ext. txs #344
- reload statistics on navigating to stats page
- fix svg export
- switch instead of checkboxes
- trigger plugin newgraph hook before loading external file
- fix decoding empty deserialized graph
- plugin hook for new graph
- fix vertical table scrollbar
- table row styling improved
- fix missing optional tag pack fields, #343
- fix quotes in tag labels an yaml import/export

## [1.0.1] - 2022-08-26

### Added
- add favicon
- light mode, #315
- plugin hooks

### Changed
- fix loading ETH address from table
- local fontawesome, #338
- fix updating user tags table on annotating
- delete tag #337
- rename tag locked to proprietary tag
- warn before closing window
- stop highlighter by escape and clicking highlighting tool again, fix #324
- suppress errors on bulk address input, #326
- distinguish entity/address tag on root address tags, #332
- improve rendering performance
- fix search input matching result list for ETH (case insensitive)
- on deserialize, only load outgoing entity neighbors in bulk 

## [1.0.0] - 2022-07-13

Complete rewrite in [Elm](https://elm-lang.org).

## Added
- plugin architecture

## Removed
- entity tags
- export feature REST calls and audit log
- hints

## [0.5.2] - 2022-03-25
### Added
- Show number of tagged addresses and tag coverage
- Remove shadows on demand
- Highlight cluster defining tag in tags table
- Highlight category in graph category color
### Changed
- Reduce table page size for faster data loading
- Prepend ~ to estimated values in graph
- Improve neighbor table, see #248
- Fuzzy label search
- Reorder properties in address/entity box

## [0.5.1] - 2021-11-30
### Added
- View transactions between two entities #159 #226
- Remove links on demand #217
- Display rate limit in statusbar #173
- Bug fixes
### Changed
- Label search result view
- Draw edges in black on SVG export #229
- improve usability of tag dialog, #204
### Removed
- Tag coherence

## [0.5.0] - 2021-06-02
### Added
- Ethereum support #176
- Local address and entity tag tables
- Search neighbors by local entity tags
- Node coloring #158, #169
- Logout button
### Changed
- minor layout improvements
- Possibility to tag entities instead of notes
- display nodes with uncategorized tags darker #186
- highlight links on hovering over nodes #174
- show transaction value by default #175
- change names of properties of nodes #162, align table buttons with properties
- generate TITANIUM court report in browser
- make tag categories linked to certain URI
### Removed
- TITANIUM tag sharing options

## [0.4.5] - 2020-11-18
### Added
- 652bba6 add csv download to addresses table, link transactions table, address txs table
### Changed
- f9e3fb4 adapt to REST service v0.4.5, replace username/password by API key
- c343af2 make snapshot after loading gs file
- 936ccb5 fix x coord of layers after deserialization, fix #152
- a731f34 fix repositioning of entities when getting addresses in, fix #151, #149

## [0.4.4] - 2020-06-12
### Changed
- 09cfef6 keep node selected on move
- 550c0d9 don't zoom on selecting node
- a2a723c fix thousands seperator in property box
- 86be354 rewrite layout algorithm, fix #142
- d9fd723 query and show links between entities explicitly, fix #140
### Added
- a1883ba display tag coherence

## [0.4.3] - 2020-05-11
### Changed
- 15b0b7b fix message on found paths, #138
- 6647295 improve setting min/max in neighbor search
- 2146fc4 dont make nodes stuck on layers borders
- cbdc3e7 tremendously improve moving of nodes, #115
- f55f5c7 improve drag/drop and zooming of graph
- 9dda161 render entitys above links
- dcceba7 integrate taxonomies and concepts in legend, make legend items sortable, only show legend items for categories in graph, #120
- b0eb812 remove static pages (project website moved to github pages)
- 671321f fix undo, #137
- 58a732b include datatables translations, #125
- 9071ddc add more info to link prop box, fix currency switching, fix #85
- 39a842c add russian
- 13385aa fix import of tagpack yaml
- dad30bb add active field to user defined tag
- b98873a link transactions table, #85
- 9f50abc completed translation, #125
- 6fbb524 fix deserialize of zoom, #91
- a44519c show categories in prop box, hide abuses and categories if empty
- f9512d0 fix search neighbor by address
- d87c03b order addresses by active state, #98
- 6dc6c73 fix search for labels from 3 characters on
- 4bea99c grey out addresses which have not been used but been tagged, #98
- 1a65a28 improve moving of nodes, fix #115
- b9b6ce1 move label column next to entity/address id, #54
- 9f274da show user-defined labels in tag annotation search, #118
- 8de90c0 redesign search (append results)
- ae79a04 add tagpack creator on export, #118
- cc71239 redesign modal
- d1306be fix lastmod timestamp of tag, #118
- 7b773cf move same tagpack values to top level on export, #118
- c54dc96 update address color on tag annotation (#118), hide search results when clicking anywhere
- 3ae305a selectable source in tag annotation, #118
- ec7a047 load tags when annotating and prefill fields, #118
- 63c27ff reload categories/abuses after loading gs file, fix #129
- e61e54d remove browser history at all, fix #124
- f29566c clickable links, #85
- 60ab74e fix missing links between big entities after removing an address
- 5cbdd33 fix wrong cross-ledger linking of nodes, fix #127
- d3d8cbd multi select nodes, calculate node property sum (cross-ledger merge), fix #116
- 08a8b37 fix deep search address search
- b66854b fix env var injection
- 401a380 report export dialog, use generate_timestamp webservice, #121
- 4f9e7cd rename "add note" to "annotate", #118
- d5bc3ba add source to user defined label annotation, #118
- 5f78e54 add abuses to user defined label annotation, #118
- 3c6fd73 add category option to user defined label annotation, #118
- c48ecd8 show labels per row in neighbors table, fix a bug in neighbors search btw, closes #54
- b0289b0 generate PDF through titanium report webservice, closes #121
- d352a88 show abuses in tag table, on address node and in property box, closes #106
- 341c53e fix supported currencies in tags table
- da49b3a use standardjs to lint code, cleanup
- 58fe763 fix sorting of addresses by final balance
- 0b9150a generate report from stats, #121
- 4cdb7f6 more precise activity period, fix #108
- 18ac5c8 dashed entity border, fix #110
- 445bf38 collapsable tables, fix #109
- 4d847b9 smaller label size to not overlap with currency, fix #112
- 3207d20 dont strip whitespaces on input, fix #113
- ad06470 export address tags in tagpack, fix #119
- b3cc71c add user defined labels as tags, fix #118

## [0.4.2] - 2019-12-20
### Changed
- 83a8d37 adapt to cleaned up rest interface
- 693c0fa fix reload categories on new graph, fix #103
- 2290990 fix typo in stats
- daeb8e2 improve labeling: entities: label (category), notes override all. Position labels better in space + 1 linebreak, remove entity label options at all, fix #88
- 7079f8a fix wrong layerid addup, #102
- d1d76bf improve default color scheme
- 0941e5e improve labeling by tag, fix #99
- b2c7ac4 improve in/outgoing neighbors icons, fix #101
- 0cedb38 add button to head to dashboard directly from landing page, fix #93
- ddbb6da allow undefined currency in tag import (apply to all supported currencies then)
- e8d422e add titanium tag sharing schema for import/export, fix #97
- 4e15f12 import tagpack as tags, #97
- 04adb98 trim whitespaces on search input
- efcd7f0 add option for downloading tags as json
- cc8d228 configurable category colors, fix #73
- 6a6cc8b fix null currencies in tags table after neighbor search, fix #79
- 8129aed export rest calls as csv, fix #89
- e51a84d now that we have cookie based auth, download csv via link (=streaming), fix #44
- 05a51d4 authentication via cookies, #58
- c1df2bf show empty graph message, center graph, slice aspectratio, fix #84
- 6658ed9 improve footer, fix #80
- 360f38c export notes as tag pack, fix #64
- d025db9 add compression to js files
- 9655bda rename cluster to entity (everywhere), fix #92
- 7a7a369 skip searchNeighbors by num. adresses, fix #76
- 596d301 import/export user notes, fix #64
- 6e05ce6 load all numeral locales; fallback if locale cannot be found; overwrite thousands delimiter for german locale, fix #83
- 79036c4 lazy loading of app, related to #77
- a0fd990 improve offical landing page design
- 3af7178 add acknowledgements and logos to footer, use handlebars-template-loader
- 6e4f4e6 show stats on official landing page
- a054d1a performance improvement when moving nodes (dont redraw everything)
- eae27a1 increase searchDepth threshold, improve setting search params
- b400ecb hide search loading message and show error on search neighbors error

## [0.4.1] - 2019-06-28
### Changed
- 43776ec add license
- f7d13af increase table "small" size threshold being below 10000
- 2c51436 disable unsupported currencies in tags table, fix #69
- c82dab0 rename Exchanges to Exchange
- 7b5f05f add token injection for production build, improve readme
- 5a0b988 fix display fetch error in log
- d1556de remove darknet crawl category
- af31108 fix tag table, adapt to new rest model
- 63b04ec improve coloring
- 2c31f88 legend colors bigger
- 4e14a74 remove Exchanges category 
- 710a06c fix positioning of search dialog, #49
- 3e1b902 merge contextmenus (fix #66), add dedicated search dialog, prepare for id search (#49)
- 6d9823d download table as csv (without streaming) #44
- 96d0f42 improve node position after adding, fix #63
- fcc75a1 hide legend/config when clicking somewhere (deselecting)
- b1cfdf5 serialize current graph transformation, fix #29
- 3d1b53c link to tag uri if starts with http, fix #38
- e533372 keep table sorting after currency change, fix #45
- 292bfae fix loading address from addresses table to correct node
- 5afca09 search neighbors by category, #52, #50
- 2956a4f add search config params
- 195f1f7 format number of items in table
- 10cd02a expand neighbors or show table if too many
- a9ae992 expand/collapse/show table on click on addresses number
- 9b42e57 fix serializing/deserializing with version upgrade, #56
- cf55ef5 keep table open after adding node from it, fix #55
- 0455d39 partition store by keyspace, fix #56
- e19f1a5 fix scrollbar on landingpage and app layout
- 27985ca fix red search result list after error
- 2a66e38 put currency switch into navbar, fix #48
### Added
- be64fd0 show number of labels on statistics
- 1d463b3 add currency to cluster node label, fix #68
- 5bab9b9 configurable which keyspaces are supported 
- 65494e4 locale user config option
- 30581f7 use browser locale and timezone for number/date formatting
- ac4a309 add tooltips, fix #27
- 96822a1 add all nodes in table option, fix #59
- ad770f4 mark addresses/clusters in tables if present in graph, #59
- 6f3be3f zoom to new nodes, fix #60
- c3d3a73 highlight nodes in graph with same id on select, fix #61
- b3f4267 add address from tag table, show tag from tag table
- cfcb3f5 label search + adapt to new rest model, fix #47
- 5003e45 add search for neighbors by id, #49
- 6880319 add actor category legend, fix #19
- 752d82d flatten svg styles to allow for export svg, fix #23
- c7d64ea search tables, fix #41
- d388a47 show message if client sorting/filtering is not possible because of size, fix #42
- 15f3e81 add commandline webtoken, #58
- 572fda9 serialize dragging, make undoable, #14
- d19b915 add hints to nav buttons, fix layout
- 23c8590 undo/redo graph changes, #14
- e381450 move nodes improved, #10
- b0ece2e move clusters on x-axis
- 9fb053e add status bar search message, #50
- 296b720 show number of visible addresses in cluster footer, fix #51
- 0f2ce48 add expandable threshold, #52
- a179a50 collapse cluster (remove its addresses), fix #40
- 4cdc49f sortable cluster addresses, #52
- e5459e4 add simple contextmenu, #52
- 82d4b49 search for block, block view, block transactions table, #26
- dda020a search for transactions, #26
- 7e9babb add import of multiple addresses, retrieve address on enter (not waiting for autocompletion), fix search error handling, #24
- 652f741 add button for blank graph, add dirty flag, prompt before loading graph from file or blank graph, fix #43
- 5ad693c add service worker for caching requests in dev mode (for developing offline/not connected to the backend)

## [0.4.0] - 2019-02-01
### Changed
- Complete redesign and reimplementation from scratch on top of Webpack, ES6 and TailwindCSS
- Responsive layout
- Adapt to version 0.4 of Graphsense REST service
- Visualize transaction flow from left to right (in-degree to out-degree)
- Boxes instead of bubbles 
- Merge cluster and address graph views
- Integrate data tables in graph view 
- Infinite scrolling on tables (leveraging paging in the backend)
- Remove block view
- Restrict autocomplete results (addresses only)
- Collapse property box if no node is selected
### Added
- Support multiple keyspaces (cryptocurrencies)
- Show statistics for each cryptocurrency on the landing page
- Search bar on graph view (load more addresses)
- Data tables for incoming/outgoing neighbors of address/cluster
- Show number of neighbors per node and in property box
- Show number of addresses per cluster node
- Node label switch (id, tags, category, ...)
- User-defined notes per node
- Coloring of nodes based on tags, notes and categories
- Loading indicators
- Logs and error messages (collapsable)
- Pan and zoom
- Save and restore the application state to/from file
- Landing page footer and couple of informational static pages 
- Dockerfile for production deployment
