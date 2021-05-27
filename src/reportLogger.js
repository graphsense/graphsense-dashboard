import moment from 'moment'
import Logger from './logger.js'

const logger = Logger.create('ReportLogger') // eslint-disable-line no-unused-vars

const outg = isOutgoing => isOutgoing ? 'outgoing' : 'incoming'

const searchbar = context => context === 'neighborsearch' ? 'neighbor search' : (context === 'tagpack' ? 'label search bar in annotation dialog' : 'main search bar')

const keyspace = keyspace => keyspace ? `of keyspace ${keyspace} ` : ''

const messages = {
  __fromURL: (payload) => !payload.target ? `load ${payload.type} ${decodeURIComponent(payload.id)} ${keyspace(payload.keyspace)}from URL` : `load ${payload.id} and ${payload.target} ${keyspace(payload.keyspace)}from URL`,
  changeLocale: (payload) => `change locale to ${payload}`,
  addAllToGraph: (payload) => 'add all from current table to graph',
  downloadTagsAsJSON: (payload) => 'download tags table as JSON',
  downloadTable: (payload) => 'download current table',
  toggleSearchTable: (payload) => 'toggle search feature in table',
  redo: (payload) => 'redo',
  undo: (payload) => 'undo',
  searchNeighbors: (payload) => 'start search neighbors',
  changeSkipNumAddresses: (payload) => 'change number of addresses to skip further search from an entity',
  changeSearchBreadth: (payload) => `change search breadth to ${payload}`,
  changeSearchDepth: (payload) => `change search depth to ${payload}`,
  sortEntityAddresses: (payload) => `sort addresses of entity ${payload.entity} by ${payload.property}`,
  gohome: (payload) => 'go to landing page',
  loadFile: (params) => {
    const type = params[0]
    const filename = params[2]
    const stage = params[3]
    if (!stage) return
    switch (type) {
      case 'load':
        return `load GS file ${filename}`
      case 'loadYAML':
        return `load tagpack YAML file ${filename}`
      case 'loadTagsJSON':
        return `load titanium tags file ${filename}`
    }
  },
  blank: (payload) => 'start from scratch (clear graph)',
  changeSearchCategory: (payload) => `change search category to ${payload}`,
  changeSearchCriterion: (payload) => `change search criterion to ${payload}`,
  searchNeighborsDialog: (payload) => `open dialog to deep search ${outg(payload.isOutgoing)} neighbors on ${payload.type} ${payload.id}`,
  noteDialog: (payload) => `open note dialog on ${payload.nodeType} ${payload.nodeId}`,
  toggleConfig: (payload) => 'toggle config',
  removeNode: (payload) => `remove ${payload[0]} ${payload[1]} from graph`,
  changeTxLabel: (payload) => `change transaction label display to ${payload}`,
  changeCurrency: (payload) => `change currency display to ${payload}`,
  changeAddressLabel: (payload) => `change address label display to ${payload}`,
  removeEntityAddresses: (payload) => `hide addresses of entity ${payload.id} in graph`,
  loadEntityAddresses: (payload) => `load addresses of entity ${payload.id} of keyspace ${payload.keyspace} in graph`,
  loadEgonet: (payload) => `load ${outg(payload.isOutgoing)} neighbors of ${payload.type} ${payload.id} of keyspace ${payload.keyspace} in graph`,
  selectAddress: (payload) => `select address ${payload.address} in table`,
  selectNeighbor: (payload) => `select ${payload.nodeType} ${payload.id} in neighbor table`,
  initTxInputsTable: (payload) => 'open table of transaction outputs',
  initTxOutputsTable: (payload) => 'open table of transaction inputs',
  initNeighborsTableWithNode: (payload) => `open property box of ${payload.type} ${payload.id} of keyspace ${payload.keyspace} and the table of ` + outg(payload.isOutgoing) + ' neighbors',
  initOutdegreeTable: (payload) => 'open table of outgoing neighbors',
  initIndegreeTable: (payload) => 'open table of incoming neighbors',
  initTagsTable: (payload) => 'open tags table',
  initAddressesTableWithEntity: (payload) => `open property box of entity ${payload.id} of keyspace ${payload.keyspace} and its address table`,
  initAddressesTable: (payload) => 'open address table',
  initBlockTransactionsTable: (payload) => 'open transactions table',
  initTransactionsTable: (payload) => 'open transactions table',
  clickBlock: (payload) => `click block ${payload.height} of keyspace ${payload.keyspace} in table`,
  clickTransaction: (payload) => `click transaction ${payload.tx_hash} of keyspace ${payload.keyspace} in table`,
  deselect: (payload) => 'deselect node',
  clickLabel: (payload) => `click label ${payload.label} of keyspace ${payload.keyspace} in table`,
  clickAddress: (payload) => `click address ${payload.address} of keyspace ${payload.keyspace} in table`,
  selectNode: (payload) => `click node ${payload[1]} of type ${payload[0]} in graph`,
  search: (payload) => `search for ${payload.term} in ` + searchbar(payload.context),
  clickSearchResult: (payload) => `select ${payload.type} ${payload.id} ${keyspace(payload.keyspace)}in ` + searchbar(payload.context)
}

export default class ReportLogger {
  constructor () {
    this.logs = []
  }

  log (eventName, eventData) {
    if (!messages[eventName]) return
    this.logs.push({
      visible_data: messages[eventName](eventData),
      timestamp: moment().format(),
      data: { eventName, eventData }
    })
  }

  getLogs () {
    return this.logs
  }
}
