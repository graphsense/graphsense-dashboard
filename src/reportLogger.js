import moment from 'moment'
import Logger from './logger.js'

const logger = Logger.create('ReportLogger') // eslint-disable-line no-unused-vars

const outg = (isOutgoing) => isOutgoing ? 'outgoing' : 'incoming'

const messages = {
  'noteDialog': (payload) => `open note dialog`,
  'toggleConfig': (payload) => `toggle config`,
  'inputNotes': (payload) => `add notes to ${payload.type} ${payload.id} of keyspace ${payload.keyspace}`,
  'removeNode': (payload) => `remove ${payload[0]} ${payload[1]} from graph`,
  'changeTxLabel': (payload) => `change transaction label display to ${payload}`,
  'changeCurrency': (payload) => `change currency display to ${payload}`,
  'changeAddressLabel': (payload) => `change address label display to ${payload}`,
  'removeEntityAddresses': (payload) => `hide addresses of entity ${payload.id} in graph`,
  'loadEntityAddresses': (payload) => `load addresses of entity ${payload.id} of keyspace ${payload.keyspace} in graph`,
  'loadEgonet': (payload) => `load ${outg(payload.isOutgoing)} neighbors of ${payload.type} ${payload.id} of keyspace ${payload.keyspace} in graph`,
  'selectAddress': (payload) => `select address ${payload.address} in table`,
  'selectNeighbor': (payload) => `select ${payload.nodeType} ${payload.id} in neighbor table`,
  'initTxInputsTable': (payload) => `open table of transaction outputs`,
  'initTxOutputsTable': (payload) => `open table of transaction inputs`,
  'initNeighborsTableWithNode': (payload) => `open property box of ${payload.type} ${payload.id} of keyspace ${payload.keyspace} and the table of ` + outg(payload.isOutgoing) + ` neighbors`,
  'initOutdegreeTable': (payload) => `open table of outgoing neighbors`,
  'initIndegreeTable': (payload) => `open table of incoming neighbors`,
  'initTagsTable': (payload) => `open tags table`,
  'initAddressesTableWithEntity': (payload) => `open property box of entity ${payload.id} of keyspace ${payload.keyspace} and its address table`,
  'initAddressesTable': (payload) => `open address table`,
  'initBlockTransactionsTable': (payload) => `open transactions table`,
  'initTransactionsTable': (payload) => `open transactions table`,
  'clickBlock': (payload) => `click block ${payload.height} of keyspace ${payload.keyspace} in table`,
  'clickTransaction': (payload) => `click transaction ${payload.tx_hash} of keyspace ${payload.keyspace} in table`,
  'deselect': (payload) => `deselect node`,
  'clickLabel': (payload) => `click label ${payload.label} of keyspace ${payload.keyspace} in table`,
  'clickAddress': (payload) => `click address ${payload.address} of keyspace ${payload.keyspace} in table`,
  'selectNode': (payload) => `click node ${payload[1]} of type ${payload[0]} in graph`,
  'search': (payload) => `search for ${payload.term} in ` + (payload.isInDialog ? 'neighbor search' : 'search bar'),
  'clickSearchResult': (payload) => 'select result in ' + (payload.isInDialog ? 'neighbor search' : 'search bar')
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
      data: {eventName, eventData}
    })
    logger.debug('logs', this.logs)
  }
  getLogs () {
    return this.logs
  }
}
