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

const historyPushState = (keyspace, type, id) => {
  let s = window.history.state
  if (s && keyspace === s.keyspace && type === s.type && id == s.id) return // eslint-disable-line eqeqeq
  let url = keyspace && type && id ? '#!' + [keyspace, type, id].join('/') : '/'
  window.history.replaceState({keyspace, type, id}, null, url)
}

const degreeThreshold = 100

let defaultLabelType =
      { clusterLabel: 'id',
        addressLabel: 'id'
      }

const defaultCurrency = 'satoshi'

const defaultTxLabel = 'noTransactions'

const keyspaces =
  {
    'btc': 'Bitcoin',
    'ltc': 'Litecoin',
    'bch': 'Bitcoin Cash',
    'zec': 'Zcash'
  }

const fromURL = (url) => {
  let hash = url.split('#!')[1]
  if (!hash) return
  let id, type, keyspace
  [keyspace, type, id] = hash.split('/')
  if (Object.keys(keyspaces).indexOf(keyspace) === -1) {
    logger.error(`invalid keyspace ${keyspace}`)
    return
  }
  if (type !== 'address' && type !== 'cluster' && type !== 'transaction') {
    logger.error(`invalid type ${type}`)
    return
  }
  return {keyspace, id, type}
}

export default class Model {
  constructor (dispatcher) {
    this.dispatcher = dispatcher
    this.isReplaying = false
    this.showLandingpage = true

    this.call = (message, data) => {
      if (this.isReplaying) {
        logger.debug('omit calling while replaying', message, data)
        return
      }
      setTimeout(() => {
        console.log('calling')
        logger.debug('calling', message, data)
        this.dispatcher.call(message, null, data)
        this.render()
      }, 1)
    }

    this.statusbar = new Statusbar(this.call)
    let rest = {}
    this.searchTimeout = {}
    for (let key in keyspaces) {
      rest[key] = new Rest(baseUrl, key, prefixLength)
      this.searchTimeout[key] = null
    }
    this.rest = (keyspace) => {
      if (!keyspaces[keyspace]) {
        return new Rest(baseUrl, '', prefixLength)
      }
      return rest[keyspace]
    }
    this.createComponents()

    this.dispatcher.on('search', (term) => {
      this.search.setSearchTerm(term, prefixLength)
      this.search.hideLoading()
      for (let keyspace in keyspaces) {
        if (this.search.needsResults(keyspace, searchlimit, prefixLength)) {
          if (this.searchTimeout[keyspace]) clearTimeout(this.searchTimeout[keyspace])
          this.search.showLoading()
          this.searchTimeout[keyspace] = setTimeout(() => {
            this.mapResult(this.rest(keyspace).search(term, searchlimit), 'searchresult', term)
          }, 250)
        }
      }
    })
    this.dispatcher.on('clickSearchResult', ({id, type, keyspace}) => {
      this.browser.loading.add(id)
      this.statusbar.addLoading(id)
      if (this.showLandingpage) {
        this.showLandingpage = false
        this.layout.shouldUpdate(true)
      }
      this.search.clear()
      this.graph.selectNodeWhenLoaded([id, type])
      this.statusbar.addMsg('loading', type, id)
      this.mapResult(this.rest(keyspace).node({id, type}), 'resultNode', id)
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
        let f = this.store.get(context.focusNode.type, context.focusNode.id)
        if (f) {
          if (context.focusNode.isOutgoing === true) {
            this.store.linkOutgoing(f.id, a.id, context.focusNode.linkData)
          } else if (context.focusNode.isOutgoing === false) {
            this.store.linkOutgoing(a.id, f.id, context.focusNode.linkData)
          }
        }
      }
      let anchor
      if (context && context.anchorNode) {
        anchor = context.anchorNode
      }
      this.browser.setResultNode(a)
      historyPushState(a.keyspace, a.type, a.id)
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
    this.dispatcher.on('searchresult', ({context, result}) => {
      this.search.hideLoading()
      this.search.setResult(context, result)
    })
    this.dispatcher.on('selectNode', ([type, nodeId]) => {
      logger.debug('selectNode', type, nodeId)
      let o = this.store.get(type, nodeId[0])
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
      this.browser.loading.add(address)
      this.statusbar.addLoading(address)
      this.graph.selectNodeWhenLoaded([address, 'address'])
      this.mapResult(this.rest(keyspace).node({id: address, type: 'address'}), 'resultNode', address)
    })
    this.dispatcher.on('deselect', () => {
      this.browser.deselect()
      this.graph.deselect()
    })
    this.dispatcher.on('clickTransaction', ({txHash, keyspace}) => {
      this.browser.loading.add(txHash)
      this.statusbar.addLoading(txHash)
      this.mapResult(this.rest(keyspace).transaction(txHash), 'resultTransactionForBrowser', txHash)
    })

    this.dispatcher.on('loadAddresses', ({keyspace, params, nextPage, request, drawCallback}) => {
      this.statusbar.addMsg('loading', 'addresses')
      this.mapResult(this.rest(keyspace).addresses({params, nextPage, pagesize: request.length}), 'resultAddresses', {page: nextPage, request, drawCallback})
    })
    this.dispatcher.on('resultAddresses', ({context, result}) => {
      this.statusbar.addMsg('loaded', 'addresses')
      this.browser.setResponse({...context, result})
    })
    this.dispatcher.on('loadTransactions', ({keyspace, params, nextPage, request, drawCallback}) => {
      this.statusbar.addMsg('loading', 'transactions')
      this.mapResult(this.rest(keyspace).transactions({params, nextPage, pagesize: request.length}), 'resultTransactions', {page: nextPage, request, drawCallback})
    })
    this.dispatcher.on('resultTransactions', ({context, result}) => {
      this.statusbar.addMsg('loaded', 'transactions')
      this.browser.setResponse({...context, result})
    })
    this.dispatcher.on('loadTags', ({keyspace, params, nextPage, request, drawCallback}) => {
      this.statusbar.addMsg('loading', 'tags')
      this.mapResult(this.rest(keyspace).tags({params, nextPage, pagesize: request.length}), 'resultTagsTable', {page: nextPage, request, drawCallback})
    })
    this.dispatcher.on('resultTagsTable', ({context, result}) => {
      this.browser.setResponse({...context, result})
    })
    this.dispatcher.on('initTransactionsTable', (request) => {
      this.browser.initTransactionsTable(request)
    })
    this.dispatcher.on('initAddressesTable', (request) => {
      this.browser.initAddressesTable(request)
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
      this.mapResult(this.rest(keyspace).neighbors(id, type, isOutgoing, request.length, nextPage), 'resultNeighbors', {page: nextPage, request, drawCallback})
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
      let o = this.store.get(data.nodeType, data.id)
      this.graph.selectNodeWhenLoaded([data.id, data.nodeType])
      let context =
        {
          focusNode:
            {
              id: focusNode.id,
              type: focusNode.type,
              linkData: {...data},
              isOutgoing: isOutgoing
            }
        }
      if (anchorNode) {
        context['anchorNode'] = {nodeId: anchorNode.id, isOutgoing}
      }
      if (!o) {
        this.browser.loading.add(data.id)
        this.statusbar.addLoading(data.id)
        this.mapResult(this.rest(data.keyspace).node({id: data.id, type: data.nodeType}), 'resultNode', context)
      } else {
        this.call('resultNode', { context, result: o })
      }
    })
    this.dispatcher.on('selectAddress', (data) => {
      logger.debug('selectAdress', data)
      if (!data.address || !data.keyspace) return
      let a = this.store.add(data)
      this.mapResult(this.rest(data.keyspace).node({id: data.address, type: 'address'}), 'resultNode', data.address)
      // historyPushState('selectAddress', data)
      this.graph.selectNodeWhenLoaded([data.address, 'address'])
      this.browser.setAddress(a)
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
        let a = this.store.get(context.type, context.id)
        if (!a) {
          this.statusbar.addMsg('loading', context.type, context.id)
          this.mapResult(this.rest(keyspace).node({type: context.type, id: context.id}), 'addNodeCont', {stage: 2, keyspace, anchor})
        } else {
          this.call('addNodeCont', {context: {stage: 2, keyspace, anchor}, result: a})
        }
      } else if (context.stage === 2 && result) {
        let o = this.store.add(result)
        this.statusbar.addMsg('loaded', o.type, o.id)
        if (anchor && anchor.isOutgoing === false) {
          // incoming neighbor node
          this.store.linkOutgoing(o.id, anchor.nodeId[0])
        }
        if (!this.graph.adding.has(o.id)) return
        logger.debug('cluster', o.cluster)
        if (o.type === 'address' && !o.cluster) {
          this.statusbar.addMsg('loadingClusterFor', o.id)
          this.mapResult(this.rest(keyspace).clusterForAddress(o.id), 'addNodeCont', {stage: 3, addressId: o.id, keyspace, anchor})
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
        let o = this.store.get(context.type, context.id)
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
        let o = this.store.get(context.type, context.id)
        if (!o.tags) {
          this.statusbar.addMsg('loadingTagsFor', o.type, o.id)
          this.mapResult(this.rest(keyspace).tags({id: o.id, type: o.type}), 'resultTags', {id: o.id, type: o.type})
        }
        this.graph.add(o, context.anchor)
        this.statusbar.removeLoading(o.id)
      }
    })
    this.dispatcher.on('excourseLoadDegree', ({context, result}) => {
      let keyspace = context.keyspace
      if (!context.stage) {
        let o = this.store.get(context.type, context.id)
        if (o.inDegree >= degreeThreshold) {
          this.call('excourseLoadDegree', {context: { ...context, stage: 2 }})
          return
        }
        this.statusbar.addMsg('loadingNeighbors', o.id, o.type, false)
        this.mapResult(this.rest(keyspace).neighbors(o.id, o.type, false, degreeThreshold), 'excourseLoadDegree', { ...context, stage: 2 })
      } else if (context.stage === 2) {
        this.statusbar.addMsg('loadedNeighbors', context.id, context.type, false)
        let o = this.store.get(context.type, context.id)
        if (result && result.neighbors) {
          // add the node in context to the outgoing set of incoming relations
          result.neighbors.forEach((neighbor) => {
            if (neighbor.nodeType !== o.type) return
            this.store.linkOutgoing(neighbor.id, o.id, neighbor)
          })
        }
        if (o.outDegree >= degreeThreshold || o.outDegree === o.outgoing.size()) {
          this.call(context.backCall.msg, context.backCall.data)
          return
        }
        this.statusbar.addMsg('loadingNeighbors', o.id, o.type, true)
        this.mapResult(this.rest(keyspace).neighbors(o.id, o.type, true, degreeThreshold), 'excourseLoadDegree', {...context, stage: 3})
      } else if (context.stage === 3) {
        let o = this.store.get(context.type, context.id)
        this.statusbar.addMsg('loadedNeighbors', context.id, context.type, true)
        if (result && result.neighbors) {
          // add outgoing relations to the node in context
          result.neighbors.forEach((neighbor) => {
            if (neighbor.nodeType !== o.type) return
            this.store.linkOutgoing(o.id, neighbor.id, neighbor)
          })
        }
        this.call(context.backCall.msg, context.backCall.data)
      }
    })
    this.dispatcher.on('resultTags', ({context, result}) => {
      let o = this.store.get(context.type, context.id)
      this.statusbar.addMsg('loadedTagsFor', o.type, o.id)
      o.tags = result.tags || []
      if (context.type === 'address' && this.graph.labelType['addressLabel'] === 'tag') {
        this.graph.addressNodes.each((node) => node.shouldUpdateLabel())
      }
      if (context.type === 'cluster' && this.graph.labelType['clusterLabel'] === 'tag') {
        this.graph.clusterNodes.each((node) => node.shouldUpdateLabel())
      }
    })
    this.dispatcher.on('loadEgonet', ({id, type, keyspace, isOutgoing, limit}) => {
      this.statusbar.addLoading(`neighbors of ${type} ${id[0]}`)
      this.statusbar.addMsg('loadingNeighbors', id, type, isOutgoing)
      this.mapResult(this.rest(keyspace).neighbors(id[0], type, isOutgoing, limit), 'resultEgonet', {id, type, isOutgoing})
    })
    this.dispatcher.on('resultEgonet', ({context, result}) => {
      let a = this.store.get(context.type, context.id[0])
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
          this.store.linkOutgoing(a.id, node.id, node)
        } else if (context.isOutgoing === false) {
          this.store.linkOutgoing(node.id, a.id, node)
        }
        this.call('addNode', {id: node.id, type: node.nodeType, keyspace: node.keyspace, anchor})
      })
    })
    this.dispatcher.on('loadClusterAddresses', ({id, keyspace, limit}) => {
      this.statusbar.addMsg('loadingClusterAddresses', id, limit)
      this.statusbar.addLoading('addresses of cluster ' + id[0])
      this.mapResult(this.rest(keyspace).clusterAddresses(id[0], limit), 'resultClusterAddresses', id)
    })
    this.dispatcher.on('resultClusterAddresses', ({context, result}) => {
      let id = context
      let addresses = []
      this.statusbar.removeLoading('addresses of cluster ' + id[0])
      result.addresses.forEach((address) => {
        let copy = {...address, toCluster: id[0]}
        let a = this.store.add(copy)
        addresses.push(a)
        if (!a.tags) {
          let request = {id: a.id, type: 'address'}
          this.mapResult(this.rest(a.keyspace).tags(request), 'resultTags', request)
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
      this.config.setCurrency(currency)
    })
    this.dispatcher.on('changeTxLabel', (type) => {
      this.graph.setTxLabel(type)
      this.config.setTxLabel(type)
    })
    this.dispatcher.on('removeNode', ([nodeType, nodeId]) => {
      this.statusbar.addMsg('removeNode', nodeType, nodeId[0])
      this.graph.remove(nodeType, nodeId)
    })
    this.dispatcher.on('inputNotes', ({id, type, note}) => {
      let o = this.store.get(type, id)
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
          node.shouldUpdate('label')
        }
      })
    })
    this.dispatcher.on('toggleConfig', () => {
      this.config.toggleConfig()
    })
    this.dispatcher.on('stats', () => {
      this.mapResult(this.rest().stats(), 'receiveStats')
    })
    this.dispatcher.on('receiveStats', ({context, result}) => {
      this.landingpage.setStats({...result})
    })
    this.dispatcher.on('contextmenu', ({x, y, node}) => {
      this.menu.showNodeConfig(x, y, node)
      this.call('selectNode', [node.data.type, node.id])
    })
    this.dispatcher.on('hideContextmenu', () => {
      this.menu.hideMenu()
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
      this.layout.triggerFileLoad()
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
      this.landingpage.shouldUpdate(true)
      this.layout.shouldUpdate(true)
      this.render()
    })
    window.onhashchange = (e) => {
      logger.debug('hashchange', e)
      let params = fromURL(e.newURL)
      if (!params) return
      this.paramsToCall(params)
    }
    window.addEventListener('beforeunload', function (evt) {
      if (!this.showLandingpage) {
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
    if (initParams) {
      this.paramsToCall(initParams)
    }
  }
  paramsToCall ({id, type, keyspace}) {
    if (type === 'cluster' || type === 'address') {
      this.call('clickSearchResult', {id, type, keyspace})
    } else if (type === 'transaction') {
      this.call('clickTransaction', {txHash: id, keyspace})
    }
  }
  createComponents () {
    this.store = new Store()
    this.browser = new Browser(this.call, defaultCurrency)
    this.config = new Config(this.call, defaultLabelType, defaultCurrency, defaultTxLabel)
    this.menu = new Menu(this.call)
    this.graph = new NodeGraph(this.call, defaultLabelType, defaultCurrency, defaultTxLabel)
    this.search = new Search(this.call, keyspaces)
    this.layout = new Layout(this.call, this.browser, this.graph, this.config, this.menu, this.search, this.statusbar)
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
      VERSION,  // eslint-disable-line no-undef
      this.store.serialize(),
      this.graph.serialize()
    ])
  }
  deserialize (buffer) {
    let data = this.decompress(buffer)
    this.createComponents()
    this.store.deserialize(data[1])
    this.graph.deserialize(data[2], this.store)
    this.layout.shouldUpdate(true)
  }
  download (filename, buffer) {
    var blob = new Blob([buffer], {type: "application/octet-stream"}) // eslint-disable-line no-undef
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
    return this.layout.render(this.root)
  }
  replay () {
    this.rest('btc').disable()
    this.rest('ltc').disable()
    logger.debug('replay')
    this.isReplaying = true
    this.dispatcher.replay()
    this.isReplaying = false
    this.rest('btc').enable()
    this.rest('ltc').enable()
  }
}
