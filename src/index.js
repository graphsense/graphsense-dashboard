import 'datatables.net-scroller-dt/css/scroller.dataTables.css'
import 'datatables.net-dt/css/jquery.dataTables.css'
import '@fortawesome/fontawesome-free/css/all.css'
import './style/Octarine-Bold/fonts.css'
import './style/Octarine-Light/fonts.css'
import './style/style.css'
import Model from './model.js'
import {dispatch} from './dispatch.js'
import Browser from './browser.js'

const dispatcher = dispatch(IS_DEV,
  'initSearch',
  'search',
  'searchresult',
  'clickSearchResult',
  'blurSearch',
  'resultNodeForBrowser',
  'resultTransactionForBrowser',
  'addNode',
  'addNodeCont',
  'loadNode',
  'loadClusterForAddress',
  'resultNode',
  'resultClusterForAddress',
  'selectNode',
  'loadEgonet',
  'loadClusterAddresses',
  'resultEgonet',
  'resultClusterAddresses',
  'initTransactionsTable',
  'loadTransactions',
  'resultTransactions',
  'initAddressesTable',
  'loadAddresses',
  'resultAddresses',
  'initTagsTable',
  'loadTags',
  'resultTags',
  'clickTransaction',
  'resultTransaction',
  'selectAddress',
  'clickAddress',
  'changeCurrency',
  'changeClusterLabel',
  'changeAddressLabel',
  'changeTxLabel',
  'removeNode',
  'initIndegreeTable',
  'initOutdegreeTable',
  'loadNeighbors',
  'resultNeighbors',
  'selectNeighbor',
  'excourseLoadDegree',
  'inputNotes',
  'switchConfig',
  'stats',
  'receiveStats'
)

let debugHistory = [{type: 'clickSearchResult', context: null, data: [{id: '1Archive1n2C579dMsAu3iC6tWzuQJz8dN', type: 'address'}]}]

// dispatcher.history = debugHistory

let model = new Model(dispatcher)
// model.replay()

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
    console.log('now render')
    model.render(document.body)
  })
}
