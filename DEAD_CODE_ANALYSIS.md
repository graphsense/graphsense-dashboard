# Dead code in graphsense-dashboard, and why we couldn't detect it

_2026-07-29 — analysis and first cleanup pass, branch `develop`._

## Summary

`elm-review`'s `NoUnused` rules had been commented out in `review/src/ReviewConfig.elm`
for a long time. With them on, the project reported **633 findings**. This document
explains what they were, why the rules were off, what the risk actually was, and what
has now been removed.

The short version of the plugin problem: **the rules were not wrong, the checkout was.**
`elm.json` is generated and its `source-directories` gain one entry per plugin
registered in `config/Config.elm`. elm-review analyses exactly those directories, so
the answer to "is this export used?" depends on which plugins happen to be checked out.
CI has none.

## What was measured

Baseline, before any changes, with all six plugins present:

| Rule | Findings |
|---|---|
| `NoUnused.Exports` | 365 |
| `NoUnused.CustomTypeConstructors` | 114 |
| `NoUnused.CustomTypeConstructorArgs` | 52 |
| `NoImportingEverything` (already on, suppressed) | 98 |
| `NoUnused.Parameters` (already on, suppressed) | 4 |

Plus 17 modules in `src/` that nothing on the build path imported at all.

## The plugin problem, precisely

Plugins are symlinks into six separate repositories:

```
plugins/Casemgm -> ../../gs-casemgm-dashboard-plugin/
plugins/Quicklock -> ../../gs-quicklock-dashboard-plugin/
plugins/Taxreport -> ../../gs-blockpit-dashboard-plugin/
… plus the three *_preview variants
```

`tools/generate.js` reads `config/Config.elm` and writes their `src` directories into
the generated `elm.json`. So on a full checkout elm-review reads 197 plugin `.elm`
files and counts what they use.

Two things follow, and only the second is a problem:

1. **`Rule.ignoreErrorsForDirectories [ …, "plugins", … ]` is not the issue.** That
   only filters *reported* errors; the files are still parsed and their usages still
   count. Verified: of the 365 reported-unused exports, resolving imports and aliases
   across all 197 plugin files found **0** that a plugin actually references.

2. **CI runs with no plugins at all.** `.github/workflows/build.yml` does
   `cp config/Config.elm.tmp config/Config.elm`, and the template registers none. In
   that checkout `elm.json` has no plugin source directories, so plugin usage is
   invisible.

How much does that change the answer? Exactly **41 exports** in `src/` are referenced
only from plugin code and nowhere else on the build path:

```
Util.andWithCmd                      <- Casemgm/CaseSummaryChart.elm
View.sidebarMenuItem                 <- Quicklock/View.elm
Css.Button.danger                    <- Casemgm/View/Import.elm
Model.Notification.fromHttpErrorWithMoreInfo  <- Casemgm/Update.elm
Update.Dialog.httpError / .info      <- Casemgm/Update.elm
Update.Search.filterByPrefix         <- Quicklock/Update/Form.elm
Util.Nullable.fromMaybe / .toMaybe   <- Casemgm/Rest.elm
View.Button.button / .buttonWithAttributes / .linkButtonUnderlinedGray
View.Graph.Table.stringColumn / .intColumn / .htmlColumn / .simpleTheadHelp
View.Graph.Browser.properties / .propertyBox
Components.InfiniteTable.getCurrentData / .removeItem / .setCountable / .updateItem
Components.Tooltip.withMaxHeight / .withMinWidth
View.Locale.time / .title / .interpolatedMarkdown / .durationToStringWithPrecision
… 41 in total
```

Turning the rules on in the shared config would therefore have made CI red on 41 false
positives, and acting on that report would have broken all six plugin repositories.
Disabling the rules was a reasonable response to that. Deleting them entirely was not
the only option, though — see below.

There is a second-order finding here worth stating plainly: **plugins import 59
different modules from `src/`.** Four modules (`View.Graph.Table`, `View.Graph.Browser`,
`Model.Graph.Browser`, `Util.Nullable`) exist *only* to serve plugins. That is an
unversioned, undocumented, accidental API. Any refactor in `src/` can silently break
six external repos, and nothing in this repository records the contract.

## The fix that was applied

Dead-code analysis is now a **separate, guarded target** rather than part of `make lint`:

- `review/` — unchanged. Used by `make lint`, pre-commit and CI. Plugin-agnostic, safe
  everywhere.
- `review-deadcode/` — new. Holds only the three `NoUnused` rules, with a header
  comment explaining the constraint.
- `make lint-deadcode` / `make lint-deadcode-fix` — run it, but **refuse** when
  `$(PLUGINS)` is empty, and print which plugins were analysed so the scope of the
  claim is visible in the output:

```
$ make lint-deadcode
lint-deadcode: analysing with plugins: Quicklock Casemgm Taxreport …

$ make lint-deadcode PLUGINS=          # simulating CI
lint-deadcode: no plugins registered in ./config/Config.elm.
Dead-code analysis needs a checkout with every plugin present,
otherwise exports used only by plugins are reported as unused.
```

A suppression baseline was considered and rejected: elm-review's baselines fail both
when there are *more* errors than recorded **and** when there are fewer, so a baseline
generated from one kind of checkout breaks the other kind.

## The better long-term fix

Give plugins an explicit API surface. Re-export the ~41 symbols they need through a
dedicated module (or extend the existing `src/PluginInterface/`), and stop plugins
importing arbitrary `src/` modules. Then:

- `src/` internals become analysable with **zero** plugins checked out, so the
  `NoUnused` rules can move into `review/` and run in CI like everything else;
- the plugin contract becomes greppable, reviewable and versionable;
- `src/` refactors stop breaking six external repositories by accident.

This is worth doing on its own merits, independent of dead code.

## What was removed

Verified after every step: `make compile-quiet`, `make test` (266 tests), `make lint`.

**16 modules plus one vendored library, entirely unreachable** — nothing on the build
path imported them:

```
src/Css/ContextMenu.elm            src/Model/Loadable.elm
src/Css/Header.elm                 src/Model/Pathfinder/Id/Address.elm
src/Effect/Store.elm               src/Model/Pathfinder/Id/Tx.elm
src/Init/Store.elm                 src/Model/Pathfinder/Tooltip.elm   (146 lines)
src/Lense/Dialog.elm               src/Model/Store.elm
src/Model/Actor.elm                src/Msg/Store.elm
src/Model/Block.elm                src/Util/Pathfinder.elm
src/Model/Graph/Table.elm          src/View/Box.elm
lib/pagedtable/                    (275 lines, not in source-directories at all)
```

Notes on two of them:

- `Model/Pathfinder/Tooltip.elm` was superseded by `Util.TooltipType` +
  `Components.Tooltip` during the tooltip refactor; the old module was left behind.
- `Model/Graph/Table.elm` looked reachable because `lib/pagedtable/src/PagedTable.elm`
  imports it — but `lib/pagedtable/src` is not in `elm.json`'s `source-directories`, so
  neither is compiled. Both are gone; `src/Components/PagedTable.elm` is the live one.

**~350 unused exports**, applied with `elm-review --fix-all` and the definitions they
kept alive removed by `NoUnused.Variables` in the same pass. Concentrated in the pf1
leftovers: `Css/Graph.elm` (−293), `View/Graph/Table.elm` (−198), `Config/Graph.elm`
(−166), `Css/Pathfinder.elm` (−133), `Css/Browser.elm` (−81), `Util/View.elm` (−78).

**Total: 108 files, +115 / −3256 lines.**

## What was deliberately *not* removed

**`src/Util/Debug.elm`** — a dev helper (`addDebugToUpdate`) that is wired in by hand
when tracing update calls, so it is always "unused" in a clean tree. It already had an
explicit `NoDebug.Log` exemption, which shows the intent. Now also exempted in
`review-deadcode/`.

**`themes/Iknaio/ColorScheme.elm`** — the auto-fix wanted to delete six annotation
colours (`annotationDarkBlue`, `annotationLightBlue`, `annotationYellow`,
`annotationPink`, `annotationTurquoise`, `annotationPurple`). They really are
superseded — the annotation picker in `View/Pathfinder.elm` uses the generated
`Theme.Colors.annotation1_color … annotation9_color`, and only `annotationRed` and
`annotationGreen` are still referenced (by `Update/Pathfinder.elm`, for conversion
annotations). But deleting entries from a design palette is a product decision, and
`themes/` is not covered by `make format`, so an automatic fix reformats a
hand-maintained file as a side effect. `themes/` is now excluded from the dead-code
config; clean this up deliberately if you want it gone.

## Two findings that are bugs, not dead code

`NoUnused.CustomTypeConstructors` flags constructors that are *handled everywhere but
never created*. Two of those are implemented features that nothing can trigger:

**`UserClickedContextMenuAlignVertically`** — fully implemented in
`Update/Pathfinder.elm:2816`, with a history rule in `Util/Pathfinder/History.elm:467`.
The two context-menu entries that would fire it are **commented out** in
`View/Pathfinder.elm:221` and `:325`. "Align horizontally" right next to it is live.
So: align-vertically is built and switched off. Either re-enable the menu entries or
delete the handler — but this should be a decision, not a cleanup.

**`UserSelectedAggEdgeFilter`** — handled in `Update/Pathfinder.elm:2652`, has a history
rule, and the resulting `AggEdgeFilter` is read by the view
(`View/Pathfinder/Network.elm:78`). Nothing ever emits the message, so the filter can
only ever hold its initial value. The UI control appears to be missing.

Also unwired, same pattern, in `Msg/Pathfinder/AddressDetails.elm`:
`UserClickedToggleDisplayAllTagsInDetails`, `UserClickedToggleTokenBalancesSelect`.

This is the strongest argument for keeping the analysis running: it finds features that
silently stopped being reachable.

## Remaining work

`make lint-deadcode` currently reports **128 findings in 28 files** (96
`CustomTypeConstructors`, 28 `CustomTypeConstructorArgs`, 4 `Exports`). These were left
alone on purpose — removing a type constructor is a semantic change, and as shown above
a blind `--fix-all` would have deleted the evidence of two real bugs.

Largest clusters, roughly in order of how safe they look:

| File | Findings | Character |
|---|---|---|
| `src/Effect/Api.elm` | 15 | Retired `/entities/` and block endpoints — `GetEntityEffect`, `BulkGetEntityNeighborsEffect`, `GetBlockTxsEffect`, … Unambiguously dead. |
| `src/Model/Graph/Browser.elm` | 17 | pf1 leftovers, but the module is plugin-facing — check `Casemgm/View/Import.elm` first. |
| `src/Api/Request/MyBulk.elm` | 10 | Generated-adjacent; check against the OpenAPI regen. |
| `src/Model/Graph/Tool.elm` | 10 | pf1 leftovers. |
| `src/Model/Pathfinder/Error.elm` | 9 | Error variants that are never constructed. |
| `src/Msg/Pathfinder*.elm` | 13 | **Read individually** — this is where the unwired features are. |

Suggested order: `Effect/Api.elm` first (mechanical), the `Msg/*` ones last and by hand.

## Reproducing any of this

```bash
make lint-deadcode          # the analysis, guarded
make lint-deadcode-fix      # apply automatic fixes (review the diff!)
make lint                   # unchanged; what CI and pre-commit run
```

The one rule to remember: **never act on a dead-code report from a checkout that is
missing plugins.** The `make` target enforces it, but the same applies to anything you
run by hand.
