import Store from './store.js'
import Search from './search/search.js'
import Browser from './browser.js'
import Rest from './rest.js'
import Layout from './layout.js'
import NodeGraph from './nodeGraph.js'
import Config from './config.js'
import Menu from './menu.js'
import Statusbar from './statusbar.js'
import Landingpage from './landingpage.js'
import moment from 'moment'
import FileSaver from 'file-saver'
import {pack, unpack} from 'lzwcompress'
import {Base64} from 'js-base64'
import Logger from './logger.js'

const logger = Logger.create('Model') // eslint-disable-line no-unused-vars

const baseUrl = REST_ENDPOINT // eslint-disable-line no-undef

const searchlimit = 100
const prefixLength = 5

// synchronous messages
// get handled by model in current rendering frame
const syncMessages = ['search']

// messages that change the graph
const dirtyMessages = [
  'clickSearchResult',
  'addNode',
  'addNodeCont',
  'resultNode',
  'resultClusterAddresses',
  'resultEgonet',
  'removeNode',
  'resultSearchNeighbors',
  'dragNodeEnd'
]

const historyPushState = (keyspace, type, id) => {
  let s = window.history.state
  if (s && keyspace === s.keyspace && type === s.type && id == s.id) return // eslint-disable-line eqeqeq
  let url = keyspace && type && id ? '#!' + [keyspace, type, id].join('/') : '/'
  if (url === '/') {
    window.history.pushState({keyspace, type, id}, null, url)
    return
  }
  window.history.replaceState({keyspace, type, id}, null, url)
}

const degreeThreshold = 100

let defaultLabelType =
      { clusterLabel: 'id',
        addressLabel: 'id'
      }

const defaultCurrency = 'satoshi'

const defaultTxLabel = 'noTransactions'

const defaultSearchDepth = 2

const defaultSearchBreadth = 16

const keyspaces =
  {
    'btc': 'Bitcoin',
    'ltc': 'Litecoin',
    'bch': 'Bitcoin Cash',
    'zec': 'Zcash'
  }

const allowedUrlTypes = ['address', 'cluster', 'transaction', 'block']

const fromURL = (url) => {
  let hash = url.split('#!')[1]
  if (!hash) return {id: '', type: '', keyspace: ''} // go home
  let split = hash.split('/')
  let id = split[2]
  let type = split[1]
  let keyspace = split[0]
  if (Object.keys(keyspaces).indexOf(keyspace) === -1) {
    logger.error(`invalid keyspace ${keyspace}`)
    return
  }
  if (allowedUrlTypes.indexOf(type) === -1) {
    logger.error(`invalid type ${type}`)
    return
  }
  return {keyspace, id, type}
}

// time to wait after a dirty message before creating a snapshot
const idleTimeToSnapshot = 2000

export default class Model {
  constructor (dispatcher) {
    this.dispatcher = dispatcher
    this.isReplaying = false
    this.showLandingpage = true

    this.snapshotTimeout = null
    this.call = (message, data) => {
      if (this.isReplaying) {
        logger.debug('omit calling while replaying', message, data)
        return
      }

      let fun = () => {
        logger.debug('calling', message, data)
        this.dispatcher.call(message, null, data)
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

    this.statusbar = new Statusbar(this.call)
    this.searchTimeout = {}
    for (let key in keyspaces) {
      this.searchTimeout[key] = null
    }
    this.rest = new Rest(baseUrl, prefixLength)
    this.createComponents()

    this.dispatcher.on('search', (term) => {
      this.search.setSearchTerm(term, prefixLength)
      this.search.hideLoading()
      for (let keyspace in keyspaces) {
        if (this.search.needsResults(keyspace, searchlimit, prefixLength)) {
          if (this.searchTimeout[keyspace]) clearTimeout(this.searchTimeout[keyspace])
          this.search.showLoading()
          this.searchTimeout[keyspace] = setTimeout(() => {
            this.mapResult(this.rest.search(keyspace, term, searchlimit), 'searchresult', term)
          }, 250)
        }
      }
    })
    this.dispatcher.on('clickSearchResult', ({id, type, keyspace}) => {
      this.browser.loading.add(id)
      this.statusbar.addLoading(id)
      if (this.showLandingpage) {
        this.showLandingpage = false
        this.layout.setUpdate(true)
      }
      this.search.clear()
      if (type === 'address' || type === 'cluster') {
        this.graph.selectNodeWhenLoaded([id, type, keyspace])
        this.mapResult(this.rest.node(keyspace, {id, type}), 'resultNode', id)
      } else if (type === 'transaction') {
        this.mapResult(this.rest.transaction(keyspace, id), 'resultTransactionForBrowser', id)
      } else if (type === 'block') {
        this.mapResult(this.rest.block(keyspace, id), 'resultBlockForBrowser', id)
      }
      this.statusbar.addMsg('loading', type, id)
    })
    this.dispatcher.on('blurSearch', () => {
      this.search.clear()
    })
    this.dispatcher.on('fetchError', ({context, msg, error}) => {
      switch (msg) {
        case 'searchresult':
          this.search.hideLoading()
          this.search.error(error.keyspace, error.message)
          // this.statusbar.addMsg('error', error)
          break
        case 'resultNode':
          this.statusbar.removeLoading(context)
          break
        case 'resultTransactionForBrowser':
          this.statusbar.removeLoading(context)
          break
        case 'resultBlockForBrowser':
          this.statusbar.removeLoading(context)
          break
        case 'resultEgonet':
          this.statusbar.removeLoading(`neighbors of ${context.type} ${context.id[0]}`)
          break
        case 'resultClusterAddresses':
          this.statusbar.removeLoading('addresses of cluster ' + context[0])
          break
        default:
          this.statusbar.addMsg('error', error)
      }
    })
    this.dispatcher.on('resultNode', ({context, result}) => {
      let a = this.store.add(result)
      if (context && context.focusNode) {
        let f = this.store.get(context.focusNode.keyspace, context.focusNode.type, context.focusNode.id)
        if (f) {
          if (context.focusNode.isOutgoing === true) {
            this.store.linkOutgoing(f.id, a.id, f.keyspace, context.focusNode.linkData)
          } else if (context.focusNode.isOutgoing === false) {
            this.store.linkOutgoing(a.id, f.id, a.keyspace, context.focusNode.linkData)
          }
        }
      }
      let anchor
      if (context && context.anchorNode) {
        anchor = context.anchorNode
      }
      if (this.browser.loading.has(a.id)) {
        this.browser.setResultNode(a)
        historyPushState(a.keyspace, a.type, a.id)
      }
      this.statusbar.removeLoading(a.id)
      this.statusbar.addMsg('loaded', a.type, a.id)
      this.call('addNode', {id: a.id, type: a.type, keyspace: a.keyspace, anchor})
    })
    this.dispatcher.on('resultTransactionForBrowser', ({result}) => {
      // historyPushState('resultTransaction', response)
      this.browser.setTransaction(result)
      historyPushState(result.keyspace, 'transaction', result.txHash)
      this.statusbar.removeLoading(result.txHash)
      this.statusbar.addMsg('loaded', 'transaction', result.txHash)
    })
    this.dispatcher.on('resultBlockForBrowser', ({result}) => {
      this.browser.setBlock(result)
      historyPushState(result.keyspace, 'block', result.height)
      this.statusbar.removeLoading(result.height)
      this.statusbar.addMsg('loaded', 'block', result.height)
    })
    this.dispatcher.on('searchresult', ({context, result}) => {
      this.search.hideLoading()
      this.search.setResult(context, result)
    })
    this.dispatcher.on('selectNode', ([type, nodeId]) => {
      logger.debug('selectNode', type, nodeId)
      let o = this.store.get(nodeId[2], type, nodeId[0])
      if (!o) {
        throw new Error(`selectNode: ${nodeId} of type ${type} not found in store`)
      }
      historyPushState(o.keyspace, o.type, o.id)
      if (type === 'address') {
        this.browser.setAddress(o)
      } else if (type === 'cluster') {
        this.browser.setCluster(o)
      }
      this.graph.selectNode(type, nodeId)
    })
    // user clicks address in transactions table
    this.dispatcher.on('clickAddress', ({address, keyspace}) => {
      this.statusbar.addLoading(address)
      this.mapResult(this.rest.node(keyspace, {id: address, type: 'address'}), 'resultNode', address)
    })
    this.dispatcher.on('deselect', () => {
      this.browser.deselect()
      this.graph.deselect()
    })
    this.dispatcher.on('clickTransaction', ({txHash, keyspace}) => {
      this.browser.loading.add(txHash)
      this.statusbar.addLoading(txHash)
      this.mapResult(this.rest.transaction(keyspace, txHash), 'resultTransactionForBrowser', txHash)
    })
    this.dispatcher.on('clickBlock', ({height, keyspace}) => {
      this.browser.loading.add(height)
      this.statusbar.addLoading(height)
      this.mapResult(this.rest.block(keyspace, height), 'resultBlockForBrowser', height)
    })

    this.dispatcher.on('loadAddresses', ({keyspace, params, nextPage, request, drawCallback}) => {
      this.statusbar.addMsg('loading', 'addresses')
      this.mapResult(this.rest.addresses(keyspace, {params, nextPage, pagesize: request.length}), 'resultAddresses', {page: nextPage, request, drawCallback})
    })
    this.dispatcher.on('resultAddresses', ({context, result}) => {
      this.statusbar.addMsg('loaded', 'addresses')
      this.browser.setResponse({...context, result})
    })
    this.dispatcher.on('loadTransactions', ({keyspace, params, nextPage, request, drawCallback}) => {
      this.statusbar.addMsg('loading', 'transactions')
      this.mapResult(this.rest.transactions(keyspace, {params, nextPage, pagesize: request.length}), 'resultTransactions', {page: nextPage, request, drawCallback})
    })
    this.dispatcher.on('resultTransactions', ({context, result}) => {
      this.statusbar.addMsg('loaded', 'transactions')
      this.browser.setResponse({...context, result})
    })
    this.dispatcher.on('loadTags', ({keyspace, params, nextPage, request, drawCallback}) => {
      this.statusbar.addMsg('loading', 'tags')
      this.mapResult(this.rest.tags(keyspace, {params, nextPage, pagesize: request.length}), 'resultTagsTable', {page: nextPage, request, drawCallback})
    })
    this.dispatcher.on('resultTagsTable', ({context, result}) => {
      this.browser.setResponse({...context, result})
    })
    this.dispatcher.on('initTransactionsTable', (request) => {
      this.browser.initTransactionsTable(request)
    })
    this.dispatcher.on('initBlockTransactionsTable', (request) => {
      this.browser.initBlockTransactionsTable(request)
    })
    this.dispatcher.on('initAddressesTable', (request) => {
      this.browser.initAddressesTable(request)
    })
    this.dispatcher.on('initAddressesTableWithCluster', ({id, keyspace}) => {
      let cluster = this.store.get(keyspace, 'cluster', id)
      if (!cluster) return
      this.browser.setCluster(cluster)
      this.browser.initAddressesTable({index: 0, id, type: 'cluster'})
    })
    this.dispatcher.on('initTagsTable', (request) => {
      this.browser.initTagsTable(request)
    })
    this.dispatcher.on('initIndegreeTable', (request) => {
      this.browser.initNeighborsTable(request, false)
    })
    this.dispatcher.on('initOutdegreeTable', (request) => {
      this.browser.initNeighborsTable(request, true)
    })
    this.dispatcher.on('initNeighborsTableWithNode', ({id, keyspace, type, isOutgoing}) => {
      let node = this.store.get(keyspace, type, id)
      if (!node) return
      if (type === 'address') {
        this.browser.setAddress(node)
      } else if (type === 'cluster') {
        this.browser.setCluster(node)
      }
      this.browser.initNeighborsTable({id, keyspace, type, index: 0}, isOutgoing)
    })
    this.dispatcher.on('initTxInputsTable', (request) => {
      this.browser.initTxAddressesTable(request, false)
    })
    this.dispatcher.on('initTxOutputsTable', (request) => {
      this.browser.initTxAddressesTable(request, true)
    })
    this.dispatcher.on('loadNeighbors', ({keyspace, params, nextPage, request, drawCallback}) => {
      let id = params[0]
      let type = params[1]
      let isOutgoing = params[2]
      this.mapResult(this.rest.neighbors(keyspace, id, type, isOutgoing, request.length, nextPage), 'resultNeighbors', {page: nextPage, request, drawCallback})
    })
    this.dispatcher.on('resultNeighbors', ({context, result}) => {
      this.browser.setResponse({...context, result})
    })
    this.dispatcher.on('selectNeighbor', (data) => {
      logger.debug('selectNeighbor', data)
      if (!data.id || !data.nodeType || !data.keyspace) return
      let focusNode = this.browser.getCurrentNode()
      let anchorNode = this.graph.selectedNode
      let isOutgoing = this.browser.isShowingOutgoingNeighbors()
      let o = this.store.get(data.keyspace, data.nodeType, data.id)
      let context =
        {
          focusNode:
            {
              id: focusNode.id,
              type: focusNode.type,
              keyspace: data.keyspace,
              linkData: {...data},
              isOutgoing: isOutgoing
            }
        }
      if (anchorNode) {
        context['anchorNode'] = {nodeId: anchorNode.id, isOutgoing}
      }
      if (!o) {
        this.statusbar.addLoading(data.id)
        this.mapResult(this.rest.node(data.keyspace, {id: data.id, type: data.nodeType}), 'resultNode', context)
      } else {
        this.call('resultNode', { context, result: o })
      }
    })
    this.dispatcher.on('selectAddress', (data) => {
      logger.debug('selectAdress', data)
      if (!data.address || !data.keyspace) return
      this.mapResult(this.rest.node(data.keyspace, {id: data.address, type: 'address'}), 'resultNode', data.address)
    })
    this.dispatcher.on('addNode', ({id, type, keyspace, anchor}) => {
      this.graph.adding.add(id)
      this.statusbar.addLoading(id)
      this.call('addNodeCont', {context: {stage: 1, id, type, keyspace, anchor}, result: null})
    })
    this.dispatcher.on('addNodeCont', ({context, result}) => {
      let anchor = context.anchor
      let keyspace = context.keyspace
      if (context.stage === 1 && context.type && context.id) {
        let a = this.store.get(context.keyspace, context.type, context.id)
        if (!a) {
          this.statusbar.addMsg('loading', context.type, context.id)
          this.mapResult(this.rest.node(keyspace, {type: context.type, id: context.id}), 'addNodeCont', {stage: 2, keyspace, anchor})
        } else {
          this.call('addNodeCont', {context: {stage: 2, keyspace, anchor}, result: a})
        }
      } else if (context.stage === 2 && result) {
        let o = this.store.add(result)
        this.statusbar.addMsg('loaded', o.type, o.id)
        if (anchor && anchor.isOutgoing === false) {
          // incoming neighbor node
          this.store.linkOutgoing(o.id, anchor.nodeId[0], o.keyspace)
        }
        if (!this.graph.adding.has(o.id)) return
        logger.debug('cluster', o.cluster)
        if (o.type === 'address' && !o.cluster) {
          this.statusbar.addMsg('loadingClusterFor', o.id)
          this.mapResult(this.rest.clusterForAddress(keyspace, o.id), 'addNodeCont', {stage: 3, addressId: o.id, keyspace, anchor})
        } else {
          this.call('addNodeCont', {context: {stage: 4, id: o.id, type: o.type, keyspace, anchor}})
        }
      } else if (context.stage === 3 && context.addressId) {
        if (!this.graph.adding.has(context.addressId)) return
        let resultCopy = {...result}
        // seems there exist addresses without cluster ...
        // so mockup cluster with the address id
        if (!resultCopy.cluster) {
          resultCopy.cluster = 'mockup' + context.addressId
          resultCopy.mockup = true
          this.statusbar.addMsg('noClusterFor', context.addressId)
        } else {
          this.statusbar.addMsg('loadedClusterFor', context.addressId)
        }
        this.store.add({...resultCopy, forAddresses: [context.addressId]})
        this.call('addNodeCont', {context: {stage: 4, id: context.addressId, type: 'address', keyspace, anchor}})
      } else if (context.stage === 4 && context.id && context.type) {
        let backCall = {msg: 'addNodeCont', data: {context: { ...context, stage: 5 }}}
        let o = this.store.get(context.keyspace, context.type, context.id)
        if (context.type === 'cluster') {
          this.call('excourseLoadDegree', {context: {backCall, id: o.id, type: 'cluster', keyspace}})
        } else if (context.type === 'address') {
          if (o.cluster && !o.cluster.mockup) {
            this.call('excourseLoadDegree', {context: {backCall, id: o.cluster.id, type: 'cluster', keyspace}})
          } else {
            this.call(backCall.msg, backCall.data)
          }
        }
      } else if (context.stage === 5 && context.id && context.type) {
        let o = this.store.get(context.keyspace, context.type, context.id)
        if (!o.tags) {
          this.statusbar.addMsg('loadingTagsFor', o.type, o.id)
          this.mapResult(this.rest.tags(keyspace, {id: o.id, type: o.type}), 'resultTags', {id: o.id, type: o.type, keypspace: o.keyspace})
        }
        this.graph.add(o, context.anchor)
        this.statusbar.removeLoading(o.id)
      }
    })
    this.dispatcher.on('excourseLoadDegree', ({context, result}) => {
      let keyspace = context.keyspace
      if (!context.stage) {
        let o = this.store.get(context.keyspace, context.type, context.id)
        if (o.inDegree >= degreeThreshold) {
          this.call('excourseLoadDegree', {context: { ...context, stage: 2 }})
          return
        }
        this.statusbar.addMsg('loadingNeighbors', o.id, o.type, false)
        this.mapResult(this.rest.neighbors(keyspace, o.id, o.type, false, degreeThreshold), 'excourseLoadDegree', { ...context, stage: 2 })
      } else if (context.stage === 2) {
        this.statusbar.addMsg('loadedNeighbors', context.id, context.type, false)
        let o = this.store.get(context.keyspace, context.type, context.id)
        if (result && result.neighbors) {
          // add the node in context to the outgoing set of incoming relations
          result.neighbors.forEach((neighbor) => {
            if (neighbor.nodeType !== o.type) return
            this.store.linkOutgoing(neighbor.id, o.id, neighbor.keyspace, neighbor)
          })
          // this.storeRelations(result.neighbors, o, o.keyspace, false)
        }
        if (o.outDegree >= degreeThreshold || o.outDegree === o.outgoing.size()) {
          this.call(context.backCall.msg, context.backCall.data)
          return
        }
        this.statusbar.addMsg('loadingNeighbors', o.id, o.type, true)
        this.mapResult(this.rest.neighbors(keyspace, o.id, o.type, true, degreeThreshold), 'excourseLoadDegree', {...context, stage: 3})
      } else if (context.stage === 3) {
        let o = this.store.get(context.keyspace, context.type, context.id)
        this.statusbar.addMsg('loadedNeighbors', context.id, context.type, true)
        if (result && result.neighbors) {
          // add outgoing relations to the node in context
          result.neighbors.forEach((neighbor) => {
            if (neighbor.nodeType !== o.type) return
            this.store.linkOutgoing(o.id, neighbor.id, o.keyspace, neighbor)
          })
          // this.storeRelations(result.neighbors, o, o.keyspace, true)
        }
        this.call(context.backCall.msg, context.backCall.data)
      }
    })
    this.dispatcher.on('resultTags', ({context, result}) => {
      let o = this.store.get(context.keyspace, context.type, context.id)
      this.statusbar.addMsg('loadedTagsFor', o.type, o.id)
      o.tags = result || []
      let nodes = null
      if (context.type === 'address') {
        nodes = this.graph.addressNodes
      }
      if (context.type === 'cluster') {
        nodes = this.graph.clusterNodes
      }
      if (!nodes) return
      nodes.each((node) => { if (node.id[0] == context.id) node.setUpdate(true) }) // eslint-disable-line eqeqeq
    })
    this.dispatcher.on('loadEgonet', ({id, type, keyspace, isOutgoing, limit}) => {
      this.statusbar.addLoading(`neighbors of ${type} ${id[0]}`)
      this.statusbar.addMsg('loadingNeighbors', id, type, isOutgoing)
      this.mapResult(this.rest.neighbors(keyspace, id[0], type, isOutgoing, limit), 'resultEgonet', {id, type, isOutgoing, keyspace})
    })
    this.dispatcher.on('resultEgonet', ({context, result}) => {
      let a = this.store.get(context.keyspace, context.type, context.id[0])
      this.statusbar.addMsg('loadedNeighbors', context.id[0], context.type, context.isOutgoing)
      this.statusbar.removeLoading(`neighbors of ${context.type} ${context.id[0]}`)
      result.neighbors.forEach((node) => {
        if (node.id === context.id[0] || node.nodeType !== context.type) return
        let anchor = {
          nodeId: context.id,
          nodeType: context.type,
          isOutgoing: context.isOutgoing
        }
        if (context.isOutgoing === true) {
          this.store.linkOutgoing(a.id, node.id, a.keyspace, node)
        } else if (context.isOutgoing === false) {
          this.store.linkOutgoing(node.id, a.id, node.keyspace, node)
        }
        this.call('addNode', {id: node.id, type: node.nodeType, keyspace: node.keyspace, anchor})
      })
    })
    this.dispatcher.on('loadClusterAddresses', ({id, keyspace, limit}) => {
      this.statusbar.addMsg('loadingClusterAddresses', id, limit)
      this.statusbar.addLoading('addresses of cluster ' + id[0])
      this.mapResult(this.rest.clusterAddresses(keyspace, id[0], limit), 'resultClusterAddresses', {id, keyspace})
    })
    this.dispatcher.on('removeClusterAddresses', id => {
      this.graph.removeClusterAddresses(id)
    })
    this.dispatcher.on('resultClusterAddresses', ({context, result}) => {
      let id = context && context.id
      let keyspace = context && context.keyspace
      let addresses = []
      this.statusbar.removeLoading('addresses of cluster ' + id[0])
      result.addresses.forEach((address) => {
        let copy = {...address, toCluster: id[0]}
        let a = this.store.add(copy)
        addresses.push(a)
        if (!a.tags) {
          let request = {id: a.id, type: 'address', keyspace}
          this.mapResult(this.rest.tags(keyspace, request), 'resultTags', request)
        }
      })
      this.statusbar.addMsg('loadedClusterAddresses', id, addresses.length)
      this.graph.setResultClusterAddresses(id, addresses)
    })
    this.dispatcher.on('changeClusterLabel', (labelType) => {
      this.config.setClusterLabel(labelType)
      this.graph.setClusterLabel(labelType)
    })
    this.dispatcher.on('changeAddressLabel', (labelType) => {
      this.config.setAddressLabel(labelType)
      this.graph.setAddressLabel(labelType)
    })
    this.dispatcher.on('changeCurrency', (currency) => {
      this.browser.setCurrency(currency)
      this.graph.setCurrency(currency)
      this.layout.setCurrency(currency)
    })
    this.dispatcher.on('changeTxLabel', (type) => {
      this.graph.setTxLabel(type)
      this.config.setTxLabel(type)
    })
    this.dispatcher.on('removeNode', ([nodeType, nodeId]) => {
      this.statusbar.addMsg('removeNode', nodeType, nodeId[0])
      this.graph.remove(nodeType, nodeId)
    })
    this.dispatcher.on('inputNotes', ({id, type, keyspace, note}) => {
      let o = this.store.get(keyspace, type, id)
      o.notes = note
      if (this.graph.labelType[type + 'Label'] !== 'tag') return
      let nodes
      if (type === 'address') {
        nodes = this.graph.addressNodes
      } else if (type === 'cluster') {
        nodes = this.graph.clusterNodes
      }
      nodes.each((node) => {
        if (node.data.id === id) {
          node.setUpdate('label')
        }
      })
    })
    this.dispatcher.on('toggleConfig', () => {
      this.config.toggleConfig()
    })
    this.dispatcher.on('stats', () => {
      this.mapResult(this.rest.stats(), 'receiveStats')
    })
    this.dispatcher.on('receiveStats', ({context, result}) => {
      this.landingpage.setStats({...result})
      this.search.setStats({...result})
    })
    this.dispatcher.on('contextmenu', ({x, y, node}) => {
      this.menu.showNodeConfig(x, y, node)
      this.call('selectNode', [node.data.type, node.id])
    })
    this.dispatcher.on('hideContextmenu', () => {
      this.menu.hideMenu()
    })
    this.dispatcher.on('new', () => {
      if (this.isReplaying) return
      if (!this.promptUnsavedWork('start a new graph')) return
      this.createComponents()
    })
    this.dispatcher.on('save', (stage) => {
      if (this.isReplaying) return
      if (!stage) {
        // update status bar before starting serializing
        this.statusbar.addMsg('saving')
        this.call('save', true)
        return
      }
      let filename = moment().format('YYYY-MM-DD HH-mm-ss') + '.gs'
      this.statusbar.addMsg('saved', filename)
      this.download(filename, this.serialize())
    })
    this.dispatcher.on('load', () => {
      if (this.isReplaying) return
      if (this.promptUnsavedWork('load another file')) {
        this.layout.triggerFileLoad()
      }
    })
    this.dispatcher.on('loadFile', (params) => {
      let data = params[0]
      let filename = params[1]
      let stage = params[2]
      if (!stage) {
        this.statusbar.addMsg('loadFile', filename)
        this.call('loadFile', [data, filename, true])
        return
      }
      this.statusbar.addMsg('loadedFile', filename)
      this.deserialize(data)
    })
    this.dispatcher.on('showLogs', () => {
      this.statusbar.show()
    })
    this.dispatcher.on('hideLogs', () => {
      this.statusbar.hide()
    })
    this.dispatcher.on('moreLogs', () => {
      this.statusbar.moreLogs()
    })
    this.dispatcher.on('toggleErrorLogs', () => {
      this.statusbar.toggleErrorLogs()
    })
    this.dispatcher.on('gohome', () => {
      logger.debug('going home')
      this.showLandingpage = true
      historyPushState()
      this.landingpage.setUpdate(true)
      this.layout.setUpdate(true)
      this.render()
    })
    this.dispatcher.on('sortClusterAddresses', ({cluster, property}) => {
      this.graph.sortClusterAddresses(cluster, property)
    })
    this.dispatcher.on('dragNode', ({id, type, dx, dy}) => {
      this.graph.dragNode(id, type, dx, dy)
    })
    this.dispatcher.on('dragNodeEnd', ({id, type}) => {
      this.graph.dragNodeEnd(id, type)
    })
    this.dispatcher.on('changeSearchDepth', value => {
      this.config.setSearchDepth(value)
    })
    this.dispatcher.on('changeSearchBreadth', value => {
      this.config.setSearchBreadth(value)
    })
    this.dispatcher.on('searchNeighbors', ({id, type, isOutgoing, params}) => {
      this.graph.searchingNeighbors(id, type, isOutgoing, true)
      let search = {
        id,
        type,
        isOutgoing,
        params,
        depth: this.config.searchDepth,
        breadth: this.config.searchBreadth
      }
      this.statusbar.addSearching(search)
      this.mapResult(this.rest.searchNeighbors(id[2], id[0], type, isOutgoing, params, this.config.searchDepth, this.config.searchBreadth), 'resultSearchNeighbors', search)
    })
    this.dispatcher.on('resultSearchNeighbors', ({result, context}) => {
      this.graph.searchingNeighbors(context.id, context.type, context.isOutgoing, false)
      this.statusbar.removeSearching(context)
      let count = 0
      let add = (anchor, paths) => {
        if (!paths) {
          count++
          return
        }
        paths.forEach(path => {
          path[0].node.keyspace = result.keyspace

          // store relations
          let node = this.store.add(path[0].node)
          let src = context.isOutgoing ? anchor.nodeId[0] : node.id
          let dst = context.isOutgoing ? node.id : anchor.nodeId[0]
          this.store.linkOutgoing(src, dst, result.keyspace, path[0].relation)

          // fetch all relations
          let backCall = {msg: 'redrawGraph', data: null}
          this.call('excourseLoadDegree', {context: {backCall, id: node.id, type: context.type, keyspace: result.keyspace}})

          let parent = this.graph.add(node, anchor)
          add({nodeId: parent.id, isOutgoing: context.isOutgoing}, path[1])
        })
      }
      add({nodeId: context.id, isOutgoing: context.isOutgoing}, result.paths)
      this.statusbar.addMsg('searchResult', count, context.params.category)
    })
    this.dispatcher.on('redrawGraph', () => {
      this.graph.setUpdate('layers')
    })
    this.dispatcher.on('createSnapshot', () => {
      this.graph.createSnapshot()
      this.layout.disableButton('undo', !this.graph.thereAreMorePreviousSnapshots())
      this.layout.disableButton('redo', !this.graph.thereAreMoreNextSnapshots())
    })
    this.dispatcher.on('undo', () => {
      this.graph.loadPreviousSnapshot(this.store)
      this.layout.disableButton('undo', !this.graph.thereAreMorePreviousSnapshots())
      this.layout.disableButton('redo', !this.graph.thereAreMoreNextSnapshots())
    })
    this.dispatcher.on('redo', () => {
      this.graph.loadNextSnapshot(this.store)
      this.layout.disableButton('undo', !this.graph.thereAreMorePreviousSnapshots())
      this.layout.disableButton('redo', !this.graph.thereAreMoreNextSnapshots())
    })
    this.dispatcher.on('disableUndoRedo', () => {
      this.layout.disableButton('undo', true)
      this.layout.disableButton('redo', true)
    })
    window.onhashchange = (e) => {
      let params = fromURL(e.newURL)
      logger.debug('hashchange', e, params)
      if (!params) return
      this.paramsToCall(params)
    }
    let that = this
    window.addEventListener('beforeunload', function (evt) {
      if (IS_DEV) return // eslint-disable-line no-undef
      if (!that.showLandingpage) {
        let message = 'You are about to leave the site. Your work will be lost. Sure?'
        if (typeof evt === 'undefined') {
          evt = window.event
        }
        if (evt) {
          evt.returnValue = message
        }
        return message
      }
    })
    let initParams = fromURL(window.location.href)
    if (initParams.id) {
      this.paramsToCall(initParams)
    }
  }
  storeRelations (relations, anchor, keyspace, isOutgoing) {
    relations.forEach((relation) => {
      if (relation.nodeType !== anchor.type) return
      let src = isOutgoing ? relation.id : anchor.id
      let dst = isOutgoing ? anchor.id : relation.id
      this.store.linkOutgoing(src, dst, keyspace, relation)
    })
  }
  promptUnsavedWork (msg) {
    if (!this.isDirty) return true
    return confirm('You have unsaved changes. Do you really want to ' + msg + '?') // eslint-disable-line no-undef
  }
  paramsToCall ({id, type, keyspace}) {
    this.call('clickSearchResult', {id, type, keyspace})
  }
  createComponents () {
    this.isDirty = false
    this.store = new Store()
    this.browser = new Browser(this.call, defaultCurrency)
    this.config = new Config(this.call, defaultLabelType, defaultTxLabel, defaultSearchDepth, defaultSearchBreadth)
    this.menu = new Menu(this.call)
    this.graph = new NodeGraph(this.call, defaultLabelType, defaultCurrency, defaultTxLabel)
    this.search = new Search(this.call, keyspaces)
    this.layout = new Layout(this.call, this.browser, this.graph, this.config, this.menu, this.search, this.statusbar, defaultCurrency)
    this.layout.disableButton('undo', !this.graph.thereAreMorePreviousSnapshots())
    this.layout.disableButton('redo', !this.graph.thereAreMoreNextSnapshots())
    this.landingpage = new Landingpage(this.call, this.search, keyspaces)
  }
  compress (data) {
    return new Uint32Array(
      pack(
        // convert to base64 (utf-16 safe)
        Base64.encode(
          JSON.stringify(data)
        )
      )
    ).buffer
  }
  decompress (data) {
    return JSON.parse(
      Base64.decode(
        unpack(
          [...new Uint32Array(data)]
        )
      )
    )
  }
  serialize () {
    return this.compress([
      VERSION, // eslint-disable-line no-undef
      this.store.serialize(),
      this.graph.serialize(),
      this.config.serialize(),
      this.layout.serialize()
    ])
  }
  deserialize (buffer) {
    let data = this.decompress(buffer)
    this.createComponents()
    this.store.deserialize(data[0], data[1])
    this.graph.deserialize(data[0], data[2], this.store)
    this.config.deserialize(data[0], data[3])
    this.layout.deserialize(data[0], data[4])
    this.layout.setUpdate(true)
  }
  download (filename, buffer) {
    var blob = new Blob([buffer], {type: 'application/octet-stream'}) // eslint-disable-line no-undef
    FileSaver.saveAs(blob, filename)
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
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (this.showLandingpage) {
      return this.landingpage.render(this.root)
    }
    logger.debug('model render')
    logger.debug('model', this)
    return this.layout.render(this.root)
  }
  replay () {
    this.rest.disable()
    logger.debug('replay')
    this.isReplaying = true
    this.dispatcher.replay()
    this.isReplaying = false
    this.rest.enable()
  }
}
