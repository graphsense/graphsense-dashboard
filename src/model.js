import Store from './store.js'
import Search from './search/search.js'
import Browser from './browser.js'
import Rest from './rest.js'
import Layout from './layout.js'
import NodeGraph from './nodeGraph.js'
import Config from './config.js'
import Landingpage from './landingpage.js'

const baseUrl = REST_ENDPOINT

const searchlimit = 100
const prefixLength = 5

const historyPushState = (msg, data) => {
  let newState = {message: msg, data: {fromHistory: true, ...data}}
  let mm = msg + '.browser'
  if (data.fromHistory) {
    history.replaceState(newState, mm)
  } else {
    history.pushState(newState, mm)
  }
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
    'zec': 'Z-Cash'
  }

export default class Model {
  constructor (dispatcher) {
    this.dispatcher = dispatcher
    this.store = new Store()
    this.isReplaying = false
    this.showLandingpage = true

    this.call = (message, data) => {
      if (this.isReplaying) {
        console.log('omit calling while replaying', message, data)
        return
      }
      setTimeout(() => {
        console.log('calling', message, data)
        this.dispatcher.call(message, null, data)
        this.render()
      }, 1)
    }

    // VIEWS
    this.browser = new Browser(this.call, defaultCurrency)
    this.graph = new NodeGraph(this.call, defaultLabelType, defaultCurrency, defaultTxLabel)
    this.config = new Config(this.call, defaultLabelType, defaultCurrency, defaultTxLabel)
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
    this.search = new Search(this.call, keyspaces)
    this.layout = new Layout(this.call, this.browser, this.graph, this.config, this.search)
    this.landingpage = new Landingpage(this.call, this.search, keyspaces)

    this.dispatcher.on('search', (term) => {
      this.search.setSearchTerm(term, prefixLength)
      for (let keyspace in keyspaces) {
        if (this.search.needsResults(keyspace, searchlimit, prefixLength)) {
          if (this.searchTimeout[keyspace]) clearTimeout(this.searchTimeout[keyspace])
          this.searchTimeout[keyspace] = setTimeout(() => {
            this.rest(keyspace).search(term, searchlimit).then(this.mapResult('searchresult', term))
          }, 250)
        }
      }
    })
    this.dispatcher.on('clickSearchResult', ({id, type, keyspace}) => {
      this.browser.loading.add(id)
      if (this.showLandingpage) {
        this.showLandingpage = false
        this.layout.shouldUpdate(true)
      }
      this.search.clear()
      this.graph.selectNodeWhenLoaded([id, type])
      this.rest(keyspace).node({id, type}).then(this.mapResult('resultNode'))
    })
    this.dispatcher.on('blurSearch', () => {
      this.search.clear()
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
      if (context && context.anchor) {
        anchor = context.anchor
      }
      this.browser.setResultNode(a)
      this.call('addNode', {id: a.id, type: a.type, keyspace: a.keyspace, anchor})
    })
    this.dispatcher.on('resultTransactionForBrowser', ({result}) => {
      // historyPushState('resultTransaction', response)
      this.browser.setTransaction(result)
    })
    this.dispatcher.on('searchresult', ({context, result}) => {
      this.search.setResult(context, result)
    })
    this.dispatcher.on('selectNode', ([type, nodeId]) => {
      console.log('selectNode', type, nodeId)
      let o = this.store.get(type, nodeId[0])
      if (!o) {
        throw new Error(`selectNode: ${nodeId} of type ${type} not found in store`)
      }
      let node
      if (type === 'address') {
        this.browser.setAddress(o)
        node = this.graph.addressNodes.get(nodeId)
      } else if (type === 'cluster') {
        this.browser.setCluster(o)
        node = this.graph.clusterNodes.get(nodeId)
      }
      this.graph.selectNode(type, nodeId)
    })
    // user clicks address in transactions table
    this.dispatcher.on('clickAddress', ({address, keyspace}) => {
      this.browser.loading.add(address)
      this.graph.selectNodeWhenLoaded([address, 'address'])
      this.rest(keyspace).node({id: address, type: 'address'}).then(this.mapResult('resultNode'))
    })
    this.dispatcher.on('deselect', () => {
      this.browser.deselect()
      this.graph.deselect()
    })
    this.dispatcher.on('clickTransaction', ({txHash, keyspace}) => {
      this.browser.loading.add(txHash)
      this.rest(keyspace).transaction(txHash).then(this.mapResult('resultTransactionForBrowser'))
    })

    this.dispatcher.on('loadAddresses', ({keyspace, params, nextPage, request, drawCallback}) => {
      this.rest(keyspace).addresses({params, nextPage, pagesize: request.length})
        .then(this.mapResult('resultAddresses', {page: nextPage, request, drawCallback}))
    })
    this.dispatcher.on('resultAddresses', ({context, result}) => {
      this.browser.setResponse({...context, result})
    })
    this.dispatcher.on('loadTransactions', ({keyspace, params, nextPage, request, drawCallback}) => {
      this.rest(keyspace).transactions({params, nextPage, pagesize: request.length})
        .then(this.mapResult('resultTransactions', {page: nextPage, request, drawCallback}))
    })
    this.dispatcher.on('resultTransactions', ({context, result}) => {
      this.browser.setResponse({...context, result})
    })
    this.dispatcher.on('loadTags', ({keyspace, params, nextPage, request, drawCallback}) => {
      this.rest(keyspace).tags({params, nextPage, pagesize: request.length})
        .then(this.mapResult('resultTags', {page: nextPage, request, drawCallback}))
    })
    this.dispatcher.on('resultTags', ({context, result}) => {
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
    this.dispatcher.on('loadNeighbors', ({keyspace, params, nextPage, request, drawCallback}) => {
      let id = params[0]
      let type = params[1]
      let isOutgoing = params[2]
      this.rest(keyspace).neighbors(id, type, isOutgoing, request.length, nextPage)
        .then(this.mapResult('resultNeighbors', {page: nextPage, request, drawCallback}))
    })
    this.dispatcher.on('resultNeighbors', ({context, result}) => {
      this.browser.setResponse({...context, result})
    })
    this.dispatcher.on('selectNeighbor', (data) => {
      console.log('selectNeighbor', data)
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
        context['anchor'] = {nodeId: anchorNode.id, isOutgoing}
      }
      if (!o) {
        this.browser.loading.add(data.id)
        this.rest(data.keyspace).node({id: data.id, type: data.nodeType})
          .then(this.mapResult('resultNode', context))
      } else {
        this.call('resultNode', {context, result: o })
      }
    })
    this.dispatcher.on('selectAddress', (data) => {
      console.log('selectAdress', data)
      if (!data.address || !data.keyspace) return
      let a = this.store.add(data)
      this.rest(data.keyspace).node({id: data.address, type: 'address'})
        .then(this.mapResult('resultNode'))
      // historyPushState('selectAddress', data)
      this.graph.selectNodeWhenLoaded([data.address, 'address'])
      this.browser.setAddress(a)
    })
    this.dispatcher.on('addNode', ({id, type, keyspace, anchor}) => {
      this.graph.adding.add(id)
      this.call('addNodeCont', {context: {stage: 1, id, type, keyspace, anchor}, result: null})
    })
    this.dispatcher.on('addNodeCont', ({context, result}) => {
      let anchor = context.anchor
      let keyspace = context.keyspace
      if (context.stage === 1 && context.type && context.id) {
        let a = this.store.get(context.type, context.id)
        if (!a) {
          this.rest(keyspace).node({type: context.type, id: context.id})
            .then(this.mapResult('addNodeCont', {stage: 2, keyspace, anchor}))
        } else {
          this.call('addNodeCont', {context: {stage: 2, keyspace, anchor}, result: a})
        }
      } else if (context.stage === 2 && result) {
        let o = this.store.add(result)
        if (anchor && anchor.isOutgoing === false) {
          // incoming neighbor node
          this.store.linkOutgoing(o.id, anchor.nodeId[0])
        }
        if (!this.graph.adding.has(o.id)) return
        console.log('cluster', o.cluster)
        if (o.type === 'address' && !o.cluster) {
          this.rest(keyspace).clusterForAddress(o.id)
            .then(this.mapResult('addNodeCont', {stage: 3, addressId: o.id, keyspace, anchor}))
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
        }
        this.store.add({...resultCopy, forAddress: context.addressId})
        this.call('addNodeCont', {context: {stage: 4, id: context.addressId, type: 'address', keyspace, anchor}})
      } else if (context.stage === 4 && context.id && context.type) {
        let backCall = {msg: 'addNodeCont', data: {context: { ...context, stage: 5 }}}
        let o = this.store.get(context.type, context.id)
        if (context.type === 'cluster') {
          this.call('excourseLoadDegree', {context: {backCall, id: o.id, type: 'cluster', keyspace}})
        } else if (context.type === 'address') {
          this.call('excourseLoadDegree', {context: {backCall, id: o.cluster.id, type: 'cluster', keyspace}})
        }
      } else if (context.stage === 5 && context.id && context.type) {
        let o = this.store.get(context.type, context.id)
        if (!o.tags) {
          this.rest(keyspace).tags({id: o.id, type: o.type})
            .then(this.mapResult('resultTags', {id: o.id, type: o.type}))
        }
        this.graph.add(o, context.anchor)
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
        this.rest(keyspace).neighbors(o.id, o.type, false, degreeThreshold)
          .then(this.mapResult('excourseLoadDegree', { ...context, stage: 2 }))
      } else if (context.stage === 2) {
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
        this.rest(keyspace).neighbors(o.id, o.type, true, degreeThreshold)
          .then(this.mapResult('excourseLoadDegree', {...context, stage: 3}))
      } else if (context.stage === 3) {
        let o = this.store.get(context.type, context.id)
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
      o.tags = result.tags
      if (context.type === 'address' && this.graph.labelType['addressLabel'] === 'tag') {
        this.graph.addressNodes.each((node) => node.shouldUpdateLabel())
      }
      if (context.type === 'cluster' && this.graph.labelType['clusterLabel'] === 'tag') {
        this.graph.clusterNodes.each((node) => node.shouldUpdateLabel())
      }
    })
    this.dispatcher.on('loadEgonet', ({id, type, keyspace, isOutgoing, limit}) => {
      this.rest(keyspace).neighbors(id[0], type, isOutgoing, limit).then(this.mapResult('resultEgonet', {id, type, isOutgoing}))
    })
    this.dispatcher.on('resultEgonet', ({context, result}) => {
      let a = this.store.get(context.type, context.id[0])
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
      this.rest(keyspace).clusterAddresses(id[0], limit).then(this.mapResult('resultClusterAddresses', id))
    })
    this.dispatcher.on('resultClusterAddresses', ({context, result}) => {
      let id = context
      let addresses = []
      result.addresses.forEach((address) => {
        let copy = {...address, toCluster: id[0]}
        let a = this.store.add(copy)
        addresses.push(a)
        if (!a.tags) {
          let request = {id: a.id, type: 'address'}
          this.rest(a.keyspace).tags(request)
            .then(this.mapResult('resultTags', request))
        }
      })
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
    this.dispatcher.on('switchConfig', (type) => {
      this.config.switchConfig(type)
    })
    this.dispatcher.on('stats', () => {
      this.rest().stats().then(this.mapResult('receiveStats'))
    })
    this.dispatcher.on('receiveStats', ({context, result}) => {
      this.landingpage.setStats({...result})
    })
    this.dispatcher.on('contextmenu', ({x, y, node}) => {
      if (!node) {
        this.config.showGraphConfig(x, y)
      } else {
        this.config.showNodeConfig(x, y, node)
        this.call('selectNode', [node.data.type, node.id])
      }
    })
    this.dispatcher.on('hideContextmenu', () => {
      this.config.hideMenu()
    })
    window.onpopstate = (e) => {
      return
      if (!e.state) return
      this.dispatcher.call(e.state.message, null, e.state.data)
    }
  }
  mapResult (msg, context) {
    if (this.isReplaying) {
      return () => {}
    }
    return result => this.call(msg, {context, result})
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (this.showLandingpage) {
      return this.landingpage.render(this.root)
    }
    console.log('model render')
    console.log('graph', this.graph)
    console.log('store', this.store)
    console.log('browser', this.browser)
    return this.layout.render(this.root)
  }
  replay () {
    // console.log('disable rest')
    this.rest('btc').disable()
    this.rest('ltc').disable()
    console.log('replay')
    this.isReplaying = true
    this.dispatcher.replay()
    this.isReplaying = false
    this.rest('btc').enable()
    this.rest('ltc').enable()
  }
}
