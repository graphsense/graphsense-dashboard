import Store from './store.js'
import Search from './search/search.js'
import Browser from './browser.js'
import Rest from './rest.js'
import Layout from './layout.js'
import NodeGraph from './nodeGraph.js'
import Config from './config.js'
import Landingpage from './landingpage.js'

const baseUrl = 'http://localhost:9000'

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
    this.graph = new NodeGraph(this.call, defaultLabelType, defaultCurrency)
    this.config = new Config(this.call, defaultLabelType, defaultCurrency)
    let btc = new Rest(baseUrl + '/btc', prefixLength)
    let ltc = new Rest(baseUrl + '/ltc', prefixLength)
    this.keyspace = 'btc'
    this.rest = (keyspace) => {
      if (!keyspace) keyspace = this.keyspace
      switch (keyspace) {
        case 'btc':
          return btc
        case 'ltc':
          return ltc
      }
    }
    this.search = new Search(this.call)
    this.layout = new Layout(this.call, this.browser, this.graph, this.config, this.search)
    this.landingpage = new Landingpage(this.call, this.search)

    this.dispatcher.on('search', (term) => {
      this.search.setSearchTerm(term, prefixLength)
      if (this.search.needsResults(searchlimit, prefixLength)) {
        this.rest().search(term, searchlimit).then(this.mapResult('searchresult', term))
      }
    })
    this.dispatcher.on('clickSearchResult', ({id, type}) => {
      this.browser.loading.add(id)
      if (this.showLandingpage) {
        this.showLandingpage = false
        this.layout.shouldUpdate(true)
      }
      this.search.clear()
      this.rest().node({id, type}).then(this.mapResult('resultNode'))
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
            this.store.linkOutgoing(f.id, a.id)
          } else if (context.focusNode.isOutgoing === false) {
            this.store.linkOutgoing(a.id, f.id)
          }
        }
      }
      this.browser.setResultNode(a)
      this.graph.selectNodeWhenLoaded([a.id, a.type])
      this.call('addNode', {id: a.id, type: a.type})
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
      this.config.selectNode(node)
    })
    // user clicks address in transactions table
    this.dispatcher.on('clickAddress', ({address}) => {
      this.browser.loading.add(address)
      this.rest().node({id: address, type: 'address'}).then(this.mapResult('resultNodeForBrowser'))
    })
    // user clicks address in transactions table
    this.dispatcher.on('clickTransaction', ({txHash}) => {
      this.browser.loading.add(txHash)
      this.rest().transaction(txHash).then(this.mapResult('resultTransactionForBrowser'))
    })

    this.dispatcher.on('loadAddresses', ({params, nextPage, request, drawCallback}) => {
      this.rest().addresses({params, nextPage, pagesize: request.length})
        .then(this.mapResult('resultAddresses', {page: nextPage, request, drawCallback}))
    })
    this.dispatcher.on('resultAddresses', ({context, result}) => {
      this.browser.setResponse({...context, result})
    })
    this.dispatcher.on('loadTransactions', ({params, nextPage, request, drawCallback}) => {
      this.rest().transactions({params, nextPage, pagesize: request.length})
        .then(this.mapResult('resultTransactions', {page: nextPage, request, drawCallback}))
    })
    this.dispatcher.on('resultTransactions', ({context, result}) => {
      this.browser.setResponse({...context, result})
    })
    this.dispatcher.on('loadTags', ({params, nextPage, request, drawCallback}) => {
      this.rest().tags({params, nextPage, pagesize: request.length})
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
    this.dispatcher.on('loadNeighbors', ({params, nextPage, request, drawCallback}) => {
      let id = params[0]
      let type = params[1]
      let isOutgoing = params[2]
      this.rest().neighbors(id, type, isOutgoing, request.length, nextPage)
        .then(this.mapResult('resultNeighbors', {page: nextPage, request, drawCallback}))
    })
    this.dispatcher.on('resultNeighbors', ({context, result}) => {
      this.browser.setResponse({...context, result})
    })
    this.dispatcher.on('selectNeighbor', (data) => {
      console.log('selectNeighbor', data)
      if (!data.id || !data.nodeType) return
      let focusNode = this.browser.getCurrentNode()
      let isOutgoing = this.browser.isShowingOutgoingNeighbors()
      let o = this.store.get(data.nodeType, data.id)
      if (!o) {
        this.browser.loading.add(data.id)
        this.rest().node({id: data.id, type: data.nodeType})
          .then(this.mapResult('resultNode', {focusNode: {id: focusNode.id, type: focusNode.type, isOutgoing: isOutgoing}}))
        return
      }
      console.log('focusNode', focusNode, isOutgoing)
      if (isOutgoing !== null && focusNode) {
        this.store.linkOutgoing(focusNode.id, o.id)
      }
      this.browser.setResultNode(o)
    })
    this.dispatcher.on('selectAddress', (data) => {
      console.log('selectAdress', data)
      if (!data.address) return
      let a = this.store.add(data)
      this.rest().node({id: data.address, type: 'address'})
        .then(this.mapResult('resultNode'))
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
          this.rest().node({type: context.type, id: context.id})
            .then(this.mapResult('addNodeCont', {stage: 2, anchor}))
        } else {
          this.call('addNodeCont', {context: {stage: 2, anchor}, result: a})
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
          this.rest().clusterForAddress(o.id)
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
        let backCall = {msg: 'addNodeCont', data: {context: { ...context, stage: 5 }}}
        let o = this.store.get(context.type, context.id)
        if (context.type === 'cluster') {
          this.call('excourseLoadDegree', {context: {backCall, id: o.id, type: 'cluster'}})
        } else if (context.type === 'address') {
          this.call('excourseLoadDegree', {context: {backCall, id: o.cluster.id, type: 'cluster'}})
        }
      } else if (context.stage === 5 && context.id && context.type) {
        let o = this.store.get(context.type, context.id)
        if (!o.tags) {
          this.rest().tags({id: o.id, type: o.type})
            .then(this.mapResult('resultTags', {id: o.id, type: o.type}))
        }
        this.graph.add(o, context.anchor)
      }
    })
    this.dispatcher.on('excourseLoadDegree', ({context, result}) => {
      if (!context.stage) {
        let o = this.store.get(context.type, context.id)
        if (o.in_degree >= degreeThreshold) {
          this.call('excourseLoadDegree', {context: { ...context, stage: 2 }})
          return
        }
        this.rest().neighbors(o.id, o.type, false, degreeThreshold)
          .then(this.mapResult('excourseLoadDegree', { ...context, stage: 2 }))
      } else if (context.stage === 2) {
        let o = this.store.get(context.type, context.id)
        if (result && result.neighbors) {
          // add the node in context to the outgoing set of incoming relations
          result.neighbors.forEach((neighbor) => {
            if (neighbor.nodeType !== o.type) return
            this.store.linkOutgoing(neighbor.id, o.id)
          })
        }
        if (o.out_degree >= degreeThreshold || o.out_degree === o.outgoing.size()) {
          this.call(context.backCall.msg, context.backCall.data)
          return
        }
        this.rest().neighbors(o.id, o.type, true, degreeThreshold)
          .then(this.mapResult('excourseLoadDegree', {...context, stage: 3}))
      } else if (context.stage === 3) {
        let o = this.store.get(context.type, context.id)
        if (result && result.neighbors) {
          // add outgoing relations to the node in context
          result.neighbors.forEach((neighbor) => {
            if (neighbor.nodeType !== o.type) return
            this.store.linkOutgoing(o.id, neighbor.id)
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
    this.dispatcher.on('loadEgonet', ({id, type, isOutgoing, limit}) => {
      this.rest().egonet(type, id, isOutgoing, limit).then(this.mapResult('resultEgonet', {id, type, isOutgoing}))
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
          this.store.linkOutgoing(a.id, node.id)
        }
        this.call('addNode', {id: node.id, type: node.nodeType, anchor})
      })
    })
    this.dispatcher.on('loadClusterAddresses', ({id, limit}) => {
      this.rest().clusterAddresses(id[0], limit).then(this.mapResult('resultClusterAddresses', id))
    })
    this.dispatcher.on('resultClusterAddresses', ({context, result}) => {
      let id = context
      let addresses = []
      result.addresses.forEach((address) => {
        let copy = {...address, toCluster: id[0]}
        let a = this.store.add(copy)
        addresses.push(a)
        if (!a.in_degree || !a.out_degree) {
          this.rest().node({id: a.id, type: 'address'})
            .then(this.mapResult('resultNode'))
        }
        if (!a.tags) {
          let request = {id: a.id, type: 'address'}
          this.rest().tags(request)
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
    this.dispatcher.on('changeCurrency', (currency) => {
      this.browser.setCurrency(currency)
      this.graph.setCurrency(currency)
      this.config.setCurrency(currency)
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
    this.dispatcher.on('stats', (keyspace) => {
      this.rest(keyspace).stats(keyspace).then(this.mapResult('receiveStats', keyspace))
    })
    this.dispatcher.on('receiveStats', ({context, result}) => {
      this.landingpage.setStats(context, {...result})
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
