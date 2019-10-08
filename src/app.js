import Callable from './callable.js'
import Store from './store.js'
import Login from './login/login.js'
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
import numeral from 'numeral'
import FileSaver from 'file-saver'
import {pack, unpack} from 'lzwcompress'
import {Base64} from 'js-base64'
import Logger from './logger.js'
import {map} from 'd3-collection'
import NeighborsTable from './browser/neighbors_table.js'
import TagsTable from './browser/tags_table.js'
import TransactionsTable from './browser/transactions_table.js'
import BlockTransactionsTable from './browser/block_transactions_table.js'
import startactions from './actions/start.js'
import {prefixLength} from './globals.js'
import YAML from 'yaml'

const logger = Logger.create('Model') // eslint-disable-line no-unused-vars

const baseUrl = REST_ENDPOINT // eslint-disable-line no-undef

const historyPushState = (keyspace, type, id) => {
  let s = window.history.state
  if (s && keyspace === s.keyspace && type === s.type && id == s.id) return // eslint-disable-line eqeqeq
  let url = '/'
  if (type && id) {
    url = '#!' + (keyspace ? keyspace + '/' : '') + [type, id].join('/')
  }
  if (url === '/') {
    window.history.pushState({keyspace, type, id}, null, url)
    return
  }
  window.history.replaceState({keyspace, type, id}, null, url)
}

const degreeThreshold = 100

let defaultLabelType =
      { entityLabel: 'category',
        addressLabel: 'id'
      }

const defaultCurrency = 'satoshi'

const defaultTxLabel = 'noTransactions'

const allowedUrlTypes = ['address', 'entity', 'transaction', 'block', 'label']

const fromURL = (url, keyspaces) => {
  let hash = url.split('#!')[1]
  if (!hash) return {id: '', type: '', keyspace: ''} // go home
  let split = hash.split('/')
  let id = split[2]
  let type = split[1]
  let keyspace = split[0]
  if (split[0] === 'label') {
    keyspace = null
    type = split[0]
    id = split[1]
  } else if (keyspaces.indexOf(keyspace) === -1) {
    logger.error(`invalid keyspace ${keyspace}`)
    return
  }
  if (allowedUrlTypes.indexOf(type) === -1) {
    logger.error(`invalid type ${type}`)
    return
  }
  return {keyspace, id, type}
}

export default class Model extends Callable {
  constructor (locale, rest, stats) {
    super()
    this.locale = locale
    this.isReplaying = false
    this.showLandingpage = true
    this.stats = stats || {}
    this.keyspaces = Object.keys(this.stats)
    logger.debug('keyspaces', this.keyspaces)
    this.snapshotTimeout = null

    this.statusbar = new Statusbar(this.call)
    this.rest = rest || new Rest(baseUrl, prefixLength)
    this.createComponents()
    this.registerDispatchEvents(startactions)

    this.dispatcher.on('clickSearchResult', ({id, type, keyspace, isInDialog}) => {
      if (isInDialog) {
        if (!this.menu.search || type !== 'address') return
        this.menu.addSearchAddress(id)
        this.menu.search.clear()
        return
      }
      this.browser.loading.add(id)
      this.statusbar.addLoading(id)
      if (this.showLandingpage) {
        this.showLandingpage = false
        this.layout.setUpdate(true)
      }
      this.search.clear()
      if (type === 'address' || type === 'entity') {
        this.graph.selectNodeWhenLoaded([id, type, keyspace])
        this.mapResult(this.rest.node(keyspace, {id, type}), 'resultNode', id)
      } else if (type === 'transaction') {
        this.mapResult(this.rest.transaction(keyspace, id), 'resultTransactionForBrowser', id)
      } else if (type === 'label') {
        this.mapResult(this.rest.label(id), 'resultLabelForBrowser', id)
      } else if (type === 'block') {
        this.mapResult(this.rest.block(keyspace, id), 'resultBlockForBrowser', id)
      }
      this.statusbar.addMsg('loading', type, id)
    })
    this.dispatcher.on('blurSearch', (isInDialog) => {
      let search = isInDialog ? this.menu.search : this.search
      if (!search) return
      search.clear()
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
      this.browser.setTransaction(result)
      historyPushState(result.keyspace, 'transaction', result.txHash)
      this.statusbar.removeLoading(result.txHash)
      this.statusbar.addMsg('loaded', 'transaction', result.txHash)
    })
    this.dispatcher.on('resultLabelForBrowser', ({result, context}) => {
      this.browser.setLabel(result)
      historyPushState(null, 'label', result.label)
      this.statusbar.removeLoading(context)
      this.statusbar.addMsg('loaded', 'label', result.label)
      this.call('initTagsTable', {id: result.label, type: 'label', index: 0})
    })
    this.dispatcher.on('resultBlockForBrowser', ({result}) => {
      this.browser.setBlock(result)
      historyPushState(result.keyspace, 'block', result.height)
      this.statusbar.removeLoading(result.height)
      this.statusbar.addMsg('loaded', 'block', result.height)
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
      } else if (type === 'entity') {
        this.browser.setEntity(o)
      }
      this.graph.selectNode(type, nodeId)
    })
    // user clicks address in a table
    this.dispatcher.on('clickAddress', ({address, keyspace}) => {
      if (this.keyspaces.indexOf(keyspace) === -1) return
      this.statusbar.addLoading(address)
      this.mapResult(this.rest.node(keyspace, {id: address, type: 'address'}), 'resultNode', address)
    })
    // user clicks label in a table
    this.dispatcher.on('clickLabel', ({label, keyspace}) => {
      this.statusbar.addLoading(label)
      this.mapResult(this.rest.label(label), 'resultLabelForBrowser', label)
    })
    this.dispatcher.on('deselect', () => {
      this.browser.deselect()
      this.config.hide()
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
      this.mapResult(this.rest.tags(keyspace, {id: params[0], type: params[1], nextPage, pagesize: request.length}), 'resultTagsTable', {page: nextPage, request, drawCallback})
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
    this.dispatcher.on('initAddressesTableWithEntity', ({id, keyspace}) => {
      let entity = this.store.get(keyspace, 'entity', id)
      if (!entity) return
      this.browser.setEntity(entity)
      this.browser.initAddressesTable({index: 0, id, type: 'entity'})
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
      } else if (type === 'entity') {
        this.browser.setEntity(node)
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
          data,
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
        logger.debug('entity', o.entity)
        if (o.type === 'address' && !o.entity) {
          this.statusbar.addMsg('loadingEntityFor', o.id)
          this.mapResult(this.rest.entityForAddress(keyspace, o.id), 'addNodeCont', {stage: 3, addressId: o.id, keyspace, anchor})
        } else {
          this.call('addNodeCont', {context: {stage: 4, id: o.id, type: o.type, keyspace, anchor}})
        }
      } else if (context.stage === 3 && context.addressId) {
        if (!this.graph.adding.has(context.addressId)) return
        let resultCopy = {...result}
        // seems there exist addresses without entity ...
        // so mockup entity with the address id
        if (!resultCopy.entity) {
          resultCopy.entity = 'mockup' + context.addressId
          resultCopy.mockup = true
          this.statusbar.addMsg('noEntityFor', context.addressId)
        } else {
          this.statusbar.addMsg('loadedEntityFor', context.addressId)
        }
        this.store.add({...resultCopy, forAddresses: [context.addressId]})
        this.call('addNodeCont', {context: {stage: 4, id: context.addressId, type: 'address', keyspace, anchor}})
      } else if (context.stage === 4 && context.id && context.type) {
        let backCall = {msg: 'addNodeCont', data: {context: { ...context, stage: 5 }}}
        let o = this.store.get(context.keyspace, context.type, context.id)
        if (context.type === 'entity') {
          this.call('excourseLoadDegree', {context: {backCall, id: o.id, type: 'entity', keyspace}})
        } else if (context.type === 'address') {
          if (o.entity && !o.entity.mockup) {
            this.call('excourseLoadDegree', {context: {backCall, id: o.entity.id, type: 'entity', keyspace}})
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
        this.browser.setUpdate('tables_with_addresses')
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
      if (context.type === 'entity') {
        nodes = this.graph.entityNodes
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
    this.dispatcher.on('loadEntityAddresses', ({id, keyspace, limit}) => {
      this.statusbar.addMsg('loadingEntityAddresses', id, limit)
      this.statusbar.addLoading('addresses of entity ' + id[0])
      this.mapResult(this.rest.entityAddresses(keyspace, id[0], limit), 'resultEntityAddresses', {id, keyspace})
    })
    this.dispatcher.on('removeEntityAddresses', id => {
      this.graph.removeEntityAddresses(id)
      this.browser.setUpdate('tables_with_addresses')
    })
    this.dispatcher.on('resultEntityAddresses', ({context, result}) => {
      let id = context && context.id
      let keyspace = context && context.keyspace
      let addresses = []
      this.statusbar.removeLoading('addresses of entity ' + id[0])
      result.addresses.forEach((address) => {
        let copy = {...address, toEntity: id[0]}
        let a = this.store.add(copy)
        addresses.push(a)
        if (!a.tags) {
          let request = {id: a.id, type: 'address', keyspace}
          this.mapResult(this.rest.tags(keyspace, request), 'resultTags', request)
        }
      })
      this.statusbar.addMsg('loadedEntityAddresses', id, addresses.length)
      this.graph.setResultEntityAddresses(id, addresses)
      this.browser.setUpdate('tables_with_addresses')
    })
    this.dispatcher.on('changeEntityLabel', (labelType) => {
      this.config.setEntityLabel(labelType)
      this.graph.setEntityLabel(labelType)
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
      this.browser.setUpdate('tables_with_addresses')
    })
    this.dispatcher.on('inputNotes', ({id, type, keyspace, note}) => {
      let o = this.store.get(keyspace, type, id)
      o.notes = note
      let nodes
      if (type === 'address') {
        nodes = this.graph.addressNodes
      } else if (type === 'entity') {
        nodes = this.graph.entityNodes
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
    this.dispatcher.on('noteDialog', ({x, y, node}) => {
      this.menu.showNodeDialog(x, y, {dialog: 'note', node})
      this.call('selectNode', [node.data.type, node.id])
    })
    this.dispatcher.on('searchNeighborsDialog', ({x, y, id, type, isOutgoing}) => {
      this.menu.showNodeDialog(x, y, {dialog: 'search', id, type, isOutgoing})
      this.call('selectNode', [type, id])
    })
    this.dispatcher.on('changeSearchCriterion', criterion => {
      this.menu.setSearchCriterion(criterion)
    })
    this.dispatcher.on('changeSearchCategory', category => {
      this.menu.setSearchCategory(category)
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
        this.config.hide()
        this.call('save', true)
        return
      }
      let filename = moment().format('YYYY-MM-DD HH-mm-ss') + '.gs'
      this.statusbar.addMsg('saved', filename)
      this.download(filename, this.serialize())
    })
    this.dispatcher.on('saveNotes', (stage) => {
      if (this.isReplaying) return
      if (!stage) {
        // update status bar before starting serializing
        this.statusbar.addMsg('saving')
        this.config.hide()
        this.call('saveNotes', true)
        return
      }
      let filename = moment().format('YYYY-MM-DD HH-mm-ss') + '.notes.gs'
      this.statusbar.addMsg('saved', filename)
      this.download(filename, this.serializeNotes())
    })
    this.dispatcher.on('saveYAML', (stage) => {
      if (this.isReplaying) return
      if (!stage) {
        // update status bar before starting serializing
        this.statusbar.addMsg('saving')
        this.config.hide()
        this.call('saveYAML', true)
        return
      }
      let filename = moment().format('YYYY-MM-DD HH-mm-ss') + '.yaml'
      this.statusbar.addMsg('saved', filename)
      this.download(filename, this.generateTagpack())
    })
    this.dispatcher.on('exportSvg', () => {
      if (this.isReplaying) return
      let classMap = map()
      let rules = document.styleSheets[0].cssRules
      for (let i = 0; i < rules.length; i++) {
        let selectorText = rules[i].selectorText
        let cssText = rules[i].cssText
        if (!selectorText || !selectorText.startsWith('svg')) continue
        let s = selectorText.replace('.', '').replace('svg', '').trim()
        classMap.set(s, cssText.split('{')[1].replace('}', ''))
      }
      let svg = this.graph.getSvg()
      // replace classes by inline styles
      svg = svg.replace(new RegExp('class="(.+?)"', 'g'), (_, classes) => {
        logger.debug('classes', classes)
        let repl = classes.split(' ')
          .map(cls => classMap.get(cls) || '')
          .join('')
        logger.debug('repl', repl)
        if (repl.trim() === '') return ''
        return 'style="' + repl.replace(/"/g, '\'').replace('"', '\'') + '"'
      })
      // replace double quotes and quot (which was created by innerHTML)
      svg = svg.replace(new RegExp('style="(.+?)"', 'g'), (_, style) => 'style="' + style.replace(/&quot;/g, '\'') + '"')
      // merge double style definitions
      svg = svg.replace(new RegExp('style="([^"]+?)"([^>]+?)style="([^"]+?)"', 'g'), 'style="$1$3" $2')
      let filename = moment().format('YYYY-MM-DD HH-mm-ss') + '.svg'
      this.download(filename, svg)
      this.config.hide()
    })
    this.dispatcher.on('load', () => {
      if (this.isReplaying) return
      if (this.promptUnsavedWork('load another file')) {
        this.layout.triggerFileLoad('load')
      }
      this.config.hide()
    })
    this.dispatcher.on('loadNotes', () => {
      if (this.isReplaying) return
      this.layout.triggerFileLoad('loadNotes')
      this.config.hide()
    })
    this.dispatcher.on('loadYAML', () => {
      if (this.isReplaying) return
      this.layout.triggerFileLoad('loadYAML')
      this.config.hide()
    })
    this.dispatcher.on('loadFile', (params) => {
      let type = params[0]
      let data = params[1]
      let filename = params[2]
      let stage = params[3]
      if (!stage) {
        this.statusbar.addMsg('loadFile', filename)
        this.call('loadFile', [type, data, filename, true])
        return
      }
      this.statusbar.addMsg('loadedFile', filename)
      if (type === 'load') {
        this.deserialize(data)
      } else if (type === 'loadNotes') {
        this.deserializeNotes(data)
      } else if (type === 'loadYAML') {
        this.loadTagpack(data)
      }
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
      this.browser.destroyComponentsFrom(0)
      this.landingpage.setUpdate(true)
      this.layout.setUpdate(true)
      this.render()
    })
    this.dispatcher.on('sortEntityAddresses', ({entity, property}) => {
      this.graph.sortEntityAddresses(entity, property)
    })
    this.dispatcher.on('dragNode', ({id, type, dx, dy}) => {
      this.graph.dragNode(id, type, dx, dy)
    })
    this.dispatcher.on('dragNodeEnd', ({id, type}) => {
      this.graph.dragNodeEnd(id, type)
    })
    this.dispatcher.on('changeSearchDepth', value => {
      this.menu.setSearchDepth(value)
    })
    this.dispatcher.on('changeSearchBreadth', value => {
      this.menu.setSearchBreadth(value)
    })
    this.dispatcher.on('changeSkipNumAddresses', value => {
      this.menu.setSkipNumAddresses(value)
    })
    this.dispatcher.on('searchNeighbors', params => {
      logger.debug('search params', params)
      this.statusbar.addSearching(params)
      this.mapResult(this.rest.searchNeighbors(params), 'resultSearchNeighbors', params)
      this.menu.hideMenu()
    })
    this.dispatcher.on('resultSearchNeighbors', ({result, context}) => {
      this.statusbar.removeSearching(context)
      let count = 0
      let add = (anchor, paths) => {
        if (!paths) {
          count++
          return
        }
        paths.forEach(pathnode => {
          pathnode.node.keyspace = result.keyspace

          // store relations
          let node = this.store.add(pathnode.node)
          let src = context.isOutgoing ? anchor.nodeId[0] : node.id
          let dst = context.isOutgoing ? node.id : anchor.nodeId[0]
          this.store.linkOutgoing(src, dst, result.keyspace, pathnode.relation)

          // fetch all relations
          let backCall = {msg: 'redrawGraph', data: null}
          this.call('excourseLoadDegree', {context: {backCall, id: node.id, type: context.type, keyspace: result.keyspace}})

          let parent = this.graph.add(node, anchor)
          // link addresses to entity and add them (if any returned due of 'addresses' search criterion)
          pathnode.matchingAddresses.forEach(address => {
            address.entity = pathnode.node.cluster
            let a = this.store.add(address)
            // anchor the address to its entity
            this.graph.add(a, {nodeId: parent.id, nodeType: 'entity'})
          })
          add({nodeId: parent.id, isOutgoing: context.isOutgoing}, pathnode.paths)
        })
      }
      add({nodeId: context.id, isOutgoing: context.isOutgoing}, result.paths)
      this.statusbar.addMsg('searchResult', count, context.params.category)
      this.browser.setUpdate('tables_with_addresses')
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
      this.browser.setUpdate('tables_with_addresses')
      this.layout.disableButton('undo', !this.graph.thereAreMorePreviousSnapshots())
      this.layout.disableButton('redo', !this.graph.thereAreMoreNextSnapshots())
    })
    this.dispatcher.on('redo', () => {
      this.graph.loadNextSnapshot(this.store)
      this.browser.setUpdate('tables_with_addresses')
      this.layout.disableButton('undo', !this.graph.thereAreMorePreviousSnapshots())
      this.layout.disableButton('redo', !this.graph.thereAreMoreNextSnapshots())
    })
    this.dispatcher.on('disableUndoRedo', () => {
      this.layout.disableButton('undo', true)
      this.layout.disableButton('redo', true)
    })
    this.dispatcher.on('toggleSearchTable', () => {
      this.browser.toggleSearchTable()
    })
    this.dispatcher.on('toggleLegend', () => {
      this.config.setCategoryColors(this.graph.getCategoryColors())
      this.config.toggleLegend()
    })
    this.dispatcher.on('toggleExport', () => {
      this.config.toggleExport()
    })
    this.dispatcher.on('toggleImport', () => {
      this.config.toggleImport()
    })
    this.dispatcher.on('downloadTable', () => {
      if (this.isReplaying) return
      let table = this.browser.content[1]
      if (!table) return
      let filename
      let request
      if (table instanceof NeighborsTable) {
        let params = table.getParams()
        request = this.rest.neighbors(params.keyspace, params.id, params.type, params.isOutgoing, 0, 0, true)
        filename = (params.isOutgoing ? 'outgoing' : 'incoming') + ` neighbors of ${params.type} ${params.id} (${params.keyspace.toUpperCase()})`
      } else if (table instanceof TagsTable) {
        let params = table.getParams()
        request = this.rest.tags(params.keyspace, params, true)
        filename = `tags of ${params.type} ${params.id} (${params.keyspace.toUpperCase()})`
      } else if (table instanceof TransactionsTable || table instanceof BlockTransactionsTable) {
        let params = table.getParams()
        request = this.rest.transactions(params.keyspace, {params: [params.id, params.type]}, true)
        filename = `transactions of ${params.type} ${params.id} (${params.keyspace.toUpperCase()})`
      }
      if (request) {
        this.mapResult(request, 'receiveCSV', filename + '.csv')
      }
    })
    this.dispatcher.on('receiveCSV', ({context, result}) => {
      FileSaver.saveAs(result, context)
    })
    this.dispatcher.on('addAllToGraph', () => {
      let table = this.browser.content[1]
      if (!table) return
      table.data.forEach(row => {
        if (!row.keyspace) {
          if (row.currency) row.keyspace = row.currency.toLowerCase()
          else row.keyspace = table.keyspace
        }
        this.call(table.selectMessage, row)
      })
    })
    this.dispatcher.on('tooltip', (type) => {
      this.statusbar.showTooltip(type)
    })
    this.dispatcher.on('hideTooltip', (type) => {
      this.statusbar.showTooltip('')
    })
    this.dispatcher.on('changeLocale', (locale) => {
      moment.locale(locale)
      numeral.locale(locale)
      this.locale = locale
      this.config.setLocale(locale)
      this.browser.setUpdate('locale')
      this.graph.setUpdate('layers')
    })
    window.onhashchange = (e) => {
      let params = fromURL(e.newURL, this.keyspaces)
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
    let initParams = fromURL(window.location.href, this.keyspaces)
    if (initParams.id) {
      this.paramsToCall(initParams)
    }
    console.log('model initialized')
    if (!stats) this.call('stats')
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
    this.browser = new Browser(this.call, defaultCurrency, this.keyspaces)
    this.config = new Config(this.call, defaultLabelType, defaultTxLabel, this.locale)
    this.menu = new Menu(this.call, this.keyspaces)
    this.graph = new NodeGraph(this.call, defaultLabelType, defaultCurrency, defaultTxLabel)
    this.browser.setNodeChecker(this.graph.getNodeChecker())
    this.login = new Login(this.call)
    this.search = new Search(this.call)
    this.search.setStats(this.stats)
    this.layout = new Layout(this.call, this.browser, this.graph, this.config, this.menu, this.search, this.statusbar, this.login, defaultCurrency)
    this.layout.disableButton('undo', !this.graph.thereAreMorePreviousSnapshots())
    this.layout.disableButton('redo', !this.graph.thereAreMoreNextSnapshots())
    this.landingpage = new Landingpage(this.call, this.keyspaces)
    this.landingpage.setStats(this.stats)
    this.landingpage.setSearch(this.search)
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
  serializeNotes () {
    return this.compress([
      VERSION, // eslint-disable-line no-undef
      this.store.serializeNotes()
    ])
  }
  generateTagpack () {
    return YAML.stringify({
      title: 'Tagpack exported from GraphSense ' + VERSION, // eslint-disable-line no-undef
      creator: this.rest.username,
      lastmod: moment().format('YYYY-MM-DD'),
      tags: this.store.getNotes()
    })
  }
  loadTagpack (yaml) {
    let data
    try {
      data = YAML.parse(yaml)
      if (!data) throw new Error('result is empty')
    } catch (e) {
      let msg = 'Could not parse YAML file'
      this.statusbar.addMsg('error', msg + ': ' + e.message)
      console.error(msg)
      return
    }
    this.store.addNotes(data.tags)
    this.graph.setUpdate('layers')
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
  deserializeNotes (buffer) {
    let data = this.decompress(buffer)
    this.store.deserializeNotes(data[0], data[1])
    this.graph.setUpdate('layers')
  }
  download (filename, buffer) {
    var blob = new Blob([buffer], {type: 'application/octet-stream'}) // eslint-disable-line no-undef
    logger.debug('saving to file', filename)
    FileSaver.saveAs(blob, filename)
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
