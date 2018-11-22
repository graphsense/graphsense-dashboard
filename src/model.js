import Store from './store.js'
import Browser from './browser.js'
import Rest from './rest.js'
import Layout from './layout.js'
import NodeGraph from './nodeGraph.js'
import Config from './config.js'

const baseUrl = 'http://localhost:9000/btc'

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

let defaultLabelType =
      { clusterLabel: 'id',
        addressLabel: 'id'
      }

export default class Model {
  constructor (dispatcher) {
    this.dispatcher = dispatcher
    this.store = new Store()
    this.isReplaying = false

    this.call = (message, data) => {
      if (this.isReplaying) {
        console.log('omit calling while replaying', message, data)
        return
      }
      console.log('calling', message, data)
      this.dispatcher.call(message, null, data)
      this.render()
    }

    // VIEWS
    this.browser = new Browser(this.call)
    this.graph = new NodeGraph(this.call, defaultLabelType)
    this.config = new Config(this.call, defaultLabelType)
    this.rest = new Rest(baseUrl, prefixLength)
    this.layout = new Layout(this.call, this.browser, this.graph, this.config)

    this.dispatcher.on('initSearch', () => {
      this.browser.setSearch()
    })
    this.dispatcher.on('search', (term) => {
      if (this.browser.setSearchTermAndNeedsResults(term, searchlimit, prefixLength)) {
        this.rest.search(term, searchlimit).then(this.mapResult('searchresult', term))
      }
    })
    this.dispatcher.on('clickSearchResult', ({id, type}) => {
      this.browser.loading.add(id)
      this.rest.node({id, type}).then(this.mapResult('resultNode'))
    })
    this.dispatcher.on('resultNode', ({context, result}) => {
      let a = this.store.add(result)
      this.browser.setResultNode(a)
      this.graph.setResultNode(a)
    })
    this.dispatcher.on('resultTransactionForBrowser', ({result}) => {
      // historyPushState('resultTransaction', response)
      this.browser.setTransaction(result)
    })
    this.dispatcher.on('searchresult', ({context, result}) => {
      this.browser.setSearchresult(context, result)
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
      this.config.selectNode(node)
    })
    // user clicks address in transactions table
    this.dispatcher.on('clickAddress', ({address}) => {
      this.browser.loading.add(address)
      this.rest.node({id: address, type: 'address'}).then(this.mapResult('resultNodeForBrowser'))
    })
    // user clicks address in transactions table
    this.dispatcher.on('clickTransaction', ({txHash}) => {
      this.browser.loading.add(txHash)
      this.rest.transaction(txHash).then(this.mapResult('resultTransactionForBrowser'))
    })

    this.dispatcher.on('loadAddresses', ({params, nextPage, pagesize, drawCallback, draw}) => {
      this.rest.addresses({params, nextPage, pagesize})
        .then(this.mapResult('resultAddresses', {page: nextPage, draw, drawCallback}))
    })
    this.dispatcher.on('resultAddresses', ({context, result}) => {
      this.browser.setResponse({...context, result})
    })
    this.dispatcher.on('loadTransactions', ({params, nextPage, pagesize, drawCallback, draw}) => {
      this.rest.transactions({params, nextPage, pagesize})
        .then(this.mapResult('resultTransactions', {page: nextPage, draw, drawCallback}))
    })
    this.dispatcher.on('resultTransactions', ({context, result}) => {
      this.browser.setResponse({...context, result})
    })
    this.dispatcher.on('loadTags', ({params, nextPage, pagesize, drawCallback, draw}) => {
      this.rest.tags({params, nextPage, pagesize})
        .then(this.mapResult('resultTags', {page: nextPage, draw, drawCallback}))
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
    this.dispatcher.on('loadNeighbors', ({params, nextPage, pagesize, drawCallback, draw}) => {
      let id = params[0]
      let type = params[1]
      let isOutgoing = params[2]
      this.rest.neighbors(id, type, isOutgoing)
        .then(this.mapResult('resultNeighbors', {page: nextPage, draw, drawCallback}))
    })
    this.dispatcher.on('resultNeighbors', ({context, result}) => {
      this.browser.setResponse({...context, result})
    })
    this.dispatcher.on('selectNeighbor', (data) => {
      console.log('selectNeighbor', data)
      if (!data.id || !data.nodeType) return
      let o = this.store.get(data.nodeType, data.id)
      if (!o) {
        this.browser.loading.add(data.id)
        this.rest.node({id: data.id, type: data.nodeType}).then(this.mapResult('resultNode'))
        return
      }
      this.browser.setResultNode(o)
    })
    this.dispatcher.on('selectAddress', (data) => {
      console.log('selectAdress', data)
      if (!data.address) return
      let a = this.store.add('address', data)
      // historyPushState('selectAddress', data)
      this.browser.setAddress(a)
    })
    this.dispatcher.on('addNode', ({id, type, anchor}) => {
      this.graph.adding.add(id)
      this.call('addNodeCont', {context: {stage: 1, id, type, anchor}, result: null})
    })
    this.dispatcher.on('addNodeCont', ({context, result}) => {
      let anchor = context.anchor
      if (context.stage === 1 && context.type && context.id) {
        let a = this.store.get(context.type, context.id)
        if (!a) {
          this.rest.node({type: context.type, id: context.id})
            .then(this.mapResult('addNodeCont', {stage: 2, anchor}))
        } else {
          this.call('addNodeCont', {context: {stage: 2, anchor}, result: a})
        }
      } else if (context.stage === 2 && result) {
        let o = this.store.add(result)
        if (anchor && anchor.isOutgoing === false) {
          // incoming neighbor node
          o.outgoing.add(anchor.nodeId[0])
        }
        if (!this.graph.adding.has(o.id)) return
        console.log('cluster', o.cluster)
        if (o.type === 'address' && !o.cluster) {
          this.rest.clusterForAddress(o.id)
            .then(this.mapResult('addNodeCont', {stage: 3, addressId: o.id, anchor}))
        } else {
          this.call('addNodeCont', {context: {stage: 4, id: o.id, type: o.type, anchor}})
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
        this.call('addNodeCont', {context: {stage: 4, id: context.addressId, type: 'address', anchor}})
      } else if (context.stage === 4 && context.id && context.type) {
        let o = this.store.get(context.type, context.id)
        this.graph.add(o, context.anchor)
        if (!o.tags) {
          this.rest.tags({id: o.id, type: o.type})
            .then(this.mapResult('resultTags', {id: o.id, type: o.type}))
        }
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
    this.dispatcher.on('loadEgonet', ({id, type, isOutgoing, limit}) => {
      this.rest.egonet(type, id, isOutgoing, limit).then(this.mapResult('resultEgonet', {id, type, isOutgoing}))
    })
    this.dispatcher.on('resultEgonet', ({context, result}) => {
      let a = this.store.get(context.type, context.id[0])
      result.nodes.forEach((node) => {
        if (node.id === context.id[0] || node.nodeType !== context.type) return
        let anchor = {
          nodeId: context.id,
          nodeType: context.type,
          isOutgoing: context.isOutgoing
        }
        if (context.isOutgoing) {
          a.outgoing.add(node.id)
        }
        this.call('addNode', {id: node.id, type: node.nodeType, anchor})
      })
    })
    this.dispatcher.on('loadClusterAddresses', ({id, limit}) => {
      this.rest.clusterAddresses(id[0], limit).then(this.mapResult('resultClusterAddresses', id))
    })
    this.dispatcher.on('resultClusterAddresses', ({context, result}) => {
      let id = context
      let addresses = []
      result.addresses.forEach((address) => {
        let copy = {...address, toCluster: id[0]}
        let a = this.store.add(copy)
        addresses.push(a)
        if (!a.in_degree || !a.out_degree) {
          this.rest.node({id: a.id, type: 'address'})
            .then(this.mapResult('resultNode'))
        }
        if (!a.tags) {
          let request = {id: a.id, type: 'address'}
          this.rest.tags(request)
            .then(this.mapResult('resultTags', request))
        }
      })
      this.graph.setResultClusterAddresses(id, addresses)
    })
    this.dispatcher.on('changeClusterLabel', (labelType) => {
      this.graph.setClusterLabel(labelType)
    })
    this.dispatcher.on('changeAddressLabel', (labelType) => {
      this.graph.setAddressLabel(labelType)
    })
    this.dispatcher.on('removeNode', ([nodeType, nodeId]) => {
      this.graph.remove(nodeType, nodeId)
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
    console.log('model render')
    console.log('graph', this.graph)
    console.log('store', this.store)
    return this.layout.render(root)
  }
  replay () {
    // console.log('disable rest')
    this.rest.disable()
    console.log('replay')
    this.isReplaying = true
    this.dispatcher.replay()
    this.isReplaying = false
    this.rest.enable()
  }
}
