# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## Unreleased
### Added
- Show number of tagged addresses
- Remove shadows on demand

### Changed
- Reduce table page size for faster data loading
- Prepend ~ to estimated values in graph
- Improve neighbor table, see #248
- Fuzzy label search

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
