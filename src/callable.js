import { dispatch } from './dispatch.js'
import Logger from './logger.js'

const logger = Logger.create('Callable') // eslint-disable-line no-unused-vars

const dispatcher = dispatch(IS_DEV, // eslint-disable-line no-undef
  'search',
  'searchresult',
  'submitSearchResult',
  'clickSearchResult',
  'blurSearch',
  'setLabels',
  'removeLabel',
  'fetchError',
  'resultNodeForBrowser',
  'resultTransactionForBrowser',
  'resultLabelForBrowser',
  'resultLabelTagsForTag',
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
  'initLinkTransactionsTable',
  'initBlockTransactionsTable',
  'loadTransactions',
  'loadLinkTransactions',
  'resultTransactions',
  'initAddressesTable',
  'initAddressesTableWithEntity',
  'loadAddresses',
  'resultAddresses',
  'initTagsTable',
  'initEntityTagsTable',
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
  'exportReport',
  'saveReport',
  'saveReportJSON',
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
  'blank',
  'sortEntityAddresses',
  'dragNodeStart',
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
  'changeUserDefinedTag',
  'addAllToGraph',
  'hoverNode',
  'leaveNode',
  'hoverLink',
  'leaveLink',
  'hoverShadow',
  'leaveShadow',
  'receiveCSV',
  'changeLocale',
  'localeLoaded',
  'login',
  'loginResult',
  'refreshResult',
  'appLoaded',
  'receiveTaxonomies',
  'receiveConcepts',
  'receiveConceptsColors',
  'saveTagsJSON',
  'loadTagsJSON',
  'jumpToApp',
  'inputMetaData',
  'pressShift',
  'releaseShift',
  'clickLink',
  'hideModal',
  'exportYAML',
  'changeMin',
  'changeMax',
  'sortCategories',
  'screenDragStart',
  'screenDragStop',
  'screenDragMove',
  'screenZoom',
  'zoomToHighlightedNodes',
  'logout',
  'loggedout',
  'toggleHighlight',
  'addHighlight',
  'pickHighlight',
  'inputHighlight',
  'removeHighlight',
  'editHighlight',
  'colorNode',
  'clickSidebarMyEntityTags',
  'clickSidebarMyAddressTags'
)

// synchronous messages
// get handled by model in current rendering frame
const syncMessages = [
  'search',
  'changeSearchBreadth',
  'changeSearchDepth',
  'changeUserDefinedTag',
  'dragNode',
  'dragNodeEnd',
  'screenDragStart',
  'screenDragStop',
  'screenDragMove',
  'screenZoom'
]

// messages that change the graph
const dirtyMessages = [
  'addNode',
  'addNodeCont',
  'resultNode',
  'resultEntityAddresses',
  'resultEgonet',
  'removeNode',
  'resultSearchNeighbors',
  'dragNodeEnd',
  'excourseLoadDegree',
  'resultTags'
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

      const render = () => {
        if (this._omitUpdate) {
          this._omitUpdate = false
          return
        }
        this.render()
      }

      const fun = () => {
        logger.boldDebug('calling', message, data)
        this.reportLogger.log(message, data)
        this.dispatcher.call(message, null, data)

        if (dirtyMessages.indexOf(message) === -1) {
          render()
          return
        }
        this.isDirty = true
        this.dispatcher.call('disableUndoRedo')
        render()

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
      this.call(msg, { context, result })
    }
    let onReject = error => {
      this.call('fetchError', { context, msg, error })
    }
    if (this.isReplaying) {
      onSuccess = () => {}
      onReject = () => {}
    }
    return promise.then(onSuccess, onReject)
  }

  registerDispatchEvents (actions) {
    for (const ev in actions) {
      logger.debug('register dispatch event', ev)
      this.dispatcher.on(ev, actions[ev].bind(this))
    }
  }

  omitUpdate () {
    this._omitUpdate = true
  }

  debounce (id, callback, timeout) {
    if (this.debouncing[id]) {
      clearTimeout(this.debouncing[id])
      this.debouncing[id] = null
    }
    this.debouncing[id] = setTimeout(() => {
      callback()
      this.debouncing[id] = null
      this.render()
    })
  }
}
