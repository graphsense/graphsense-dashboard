import {dispatch} from './dispatch.js'
import Logger from './logger.js'

const logger = Logger.create('Callable') // eslint-disable-line no-unused-vars

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
  'loadEntityForAddress',
  'resultNode',
  'resultEntityForAddress',
  'selectNode',
  'loadEgonet',
  'loadEntityAddresses',
  'removeEntityAddresses',
  'resultEgonet',
  'resultEntityAddresses',
  'initTransactionsTable',
  'initBlockTransactionsTable',
  'loadTransactions',
  'resultTransactions',
  'initAddressesTable',
  'initAddressesTableWithEntity',
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
  'changeEntityLabel',
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
  'toggleExport',
  'toggleImport',
  'stats',
  'receiveStats',
  'noteDialog',
  'hideContextmenu',
  'save',
  'saveNotes',
  'saveYAML',
  'load',
  'loadNotes',
  'loadYAML',
  'loadFile',
  'deselect',
  'showLogs',
  'toggleErrorLogs',
  'moreLogs',
  'hideLogs',
  'gohome',
  'new',
  'sortEntityAddresses',
  'dragNode',
  'dragNodeEnd',
  'changeSearchDepth',
  'changeSearchBreadth',
  'changeSkipNumAddresses',
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
  'exportRestLogs',
  'toggleLegend',
  'downloadTable',
  'downloadTagsAsJSON',
  'changeSearchCategory',
  'changeSearchCriterion',
  'addAllToGraph',
  'tooltip',
  'hideTooltip',
  'receiveCSV',
  'changeLocale',
  'login',
  'loginResult',
  'refreshResult',
  'appLoaded',
  'receiveCategories',
  'receiveCategoryColors'
)

// synchronous messages
// get handled by model in current rendering frame
const syncMessages = ['search', 'changeSearchBreadth', 'changeSearchDepth']

// messages that change the graph
const dirtyMessages = [
  'addNode',
  'addNodeCont',
  'resultNode',
  'resultEntityAddresses',
  'resultEgonet',
  'removeNode',
  'resultSearchNeighbors',
  'dragNodeEnd'
]

// time to wait after a dirty message before creating a snapshot
const idleTimeToSnapshot = 2000

export default class Callable {
  constructor () {
    this.dispatcher = dispatcher
    this.call = (message, data) => {
      if (this.isReplaying) {
        logger.debug('omit calling while replaying', message, data)
        return
      }

      let fun = () => {
        logger.boldDebug('calling', message, data)
        this.dispatcher.call(message, null, data)
        logger.debug('this is', this)
        if (dirtyMessages.indexOf(message) === -1) {
          this.render()
          return
        }
        this.isDirty = true
        this.dispatcher.call('disableUndoRedo')
        this.render()

        if (this.snapshotTimeout) clearTimeout(this.snapshotTimeout)
        this.snapshotTimeout = setTimeout(() => {
          this.call('createSnapshot')
          this.snapshotTimeout = null
        }, idleTimeToSnapshot)
      }
      if (syncMessages.indexOf(message) !== -1) {
        fun()
      } else {
        setTimeout(fun, 1)
      }
    }
  }
  mapResult (promise, msg, context) {
    let onSuccess = result => {
      this.call(msg, {context, result})
    }
    let onReject = error => {
      this.call('fetchError', {context, msg, error})
    }
    if (this.isReplaying) {
      onSuccess = () => {}
      onReject = () => {}
    }
    return promise.then(onSuccess, onReject)
  }
  registerDispatchEvents (actions) {
    for (let ev in actions) {
      logger.debug('register dispatch event', ev)
      this.dispatcher.on(ev, actions[ev].bind(this))
    }
  }
}
