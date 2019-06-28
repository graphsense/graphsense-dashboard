import 'datatables.net-scroller-dt/css/scroller.dataTables.css'
import 'datatables.net-dt/css/jquery.dataTables.css'
import '@fortawesome/fontawesome-free/css/all.css'
import './style/Octarine-Bold/fonts.css'
import './style/Octarine-Light/fonts.css'
import 'd3-context-menu/css/d3-context-menu.css'
import './style/style.css'
import Model from './model.js'
import {dispatch} from './dispatch.js'
import numeral from 'numeral'
import moment from 'moment'
import Logger from './logger.js'
import jstz from 'jstimezonedetect'
import 'moment-timezone'

Logger.setLogLevel(IS_DEV ? Logger.LogLevels.DEBUG : Logger.LogLevels.ERROR) // eslint-disable-line no-undef

numeral.register('locale', 'de', {
  delimiters: {
    thousands: '.',
    decimal: ','
  }
})

const getNavigatorLanguage = () => {
  if (navigator.languages && navigator.languages.length) {
    return navigator.languages[0]
  } else {
    return navigator.userLanguage || navigator.language || navigator.browserLanguage || 'en'
  }
}

const locale = getNavigatorLanguage().split('-')[0]
numeral.locale(locale)
moment.locale(locale)

const timezone = jstz.determine().name()
moment.tz.setDefault(timezone)

const dispatcher = dispatch(IS_DEV, // eslint-disable-line no-undef
  'initSearch',
  'search',
  'searchresult',
  'searchresultLabels',
  'clickSearchResult',
  'blurSearch',
  'fetchError',
  'resultNodeForBrowser',
  'resultTransactionForBrowser',
  'resultLabelForBrowser',
  'resultBlockForBrowser',
  'addNode',
  'addNodeCont',
  'loadNode',
  'loadClusterForAddress',
  'resultNode',
  'resultClusterForAddress',
  'selectNode',
  'loadEgonet',
  'loadClusterAddresses',
  'removeClusterAddresses',
  'resultEgonet',
  'resultClusterAddresses',
  'initTransactionsTable',
  'initBlockTransactionsTable',
  'loadTransactions',
  'resultTransactions',
  'initAddressesTable',
  'initAddressesTableWithCluster',
  'loadAddresses',
  'resultAddresses',
  'initTagsTable',
  'loadTags',
  'resultTags',
  'resultTagsTable',
  'clickTransaction',
  'clickBlock',
  'resultTransaction',
  'selectAddress',
  'clickAddress',
  'clickLabel',
  'changeCurrency',
  'changeClusterLabel',
  'changeAddressLabel',
  'changeTxLabel',
  'removeNode',
  'initIndegreeTable',
  'initOutdegreeTable',
  'initNeighborsTableWithNode',
  'initTxInputsTable',
  'initTxOutputsTable',
  'loadNeighbors',
  'resultNeighbors',
  'selectNeighbor',
  'excourseLoadDegree',
  'inputNotes',
  'toggleConfig',
  'stats',
  'receiveStats',
  'noteDialog',
  'hideContextmenu',
  'save',
  'load',
  'loadFile',
  'deselect',
  'showLogs',
  'toggleErrorLogs',
  'moreLogs',
  'hideLogs',
  'gohome',
  'new',
  'sortClusterAddresses',
  'dragNode',
  'dragNodeEnd',
  'changeSearchDepth',
  'changeSearchBreadth',
  'searchNeighborsDialog',
  'searchNeighbors',
  'resultSearchNeighbors',
  'redrawGraph',
  'createSnapshot',
  'undo',
  'redo',
  'disableUndoRedo',
  'toggleSearchTable',
  'exportSvg',
  'toggleLegend',
  'downloadTable',
  'changeSearchCategory',
  'changeSearchCriterion',
  'addAllToGraph',
  'tooltip',
  'hideTooltip',
  'receiveCSV',
  'changeLocale'
)

let model = new Model(dispatcher, locale)

model.render(document.body)

if (module.hot) {
  module.hot.accept([
    './browser.js',
    './browser/address.html',
    './browser/address.js',
    './browser/addresses_table.js',
    './browser/cluster.html',
    './browser/cluster.js',
    './browser/component.js',
    './browser/layout.html',
    './browser/option.html',
    './search/search.html',
    './search/search.js',
    './status/status.html',
    './statusbar.js',
    './browser/table.html',
    './browser/table.js',
    './browser/tags_table.js',
    './browser/transaction.html',
    './browser/transaction.js',
    './browser/transaction_addresses_table.js',
    './browser/transactions_table.js',
    './nodeGraph.js',
    './nodeGraph/addressNode.js',
    './nodeGraph/clusterNode.js',
    './nodeGraph/graphNode.js',
    './nodeGraph/layer.js',
    './config.js',
    './config/address.html',
    './config/cluster.html',
    './config/filter.html',
    './config/graph.html',
    './config/layout.html',
    './layout.js',
    './layout/layout.html',
    './component.js',
    './model.js',
    './rest.js',
    './store.js',
    './template_utils.js',
    './utils.js'
  ], () => {
    // dispatcher.history = [debugHistory[0]]

    model = new Model(dispatcher)
    model.replay()
    model.render(document.body)
  })
  if ('serviceWorker' in navigator) {
    navigator.serviceWorker.register('./sw.js')
  }
}
