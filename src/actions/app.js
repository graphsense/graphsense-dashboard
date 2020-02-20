import Logger from '../logger.js'
import numeral from 'numeral'
import moment from 'moment'
import {map} from 'd3-collection'
import NeighborsTable from '../browser/neighbors_table.js'
import TagsTable from '../browser/tags_table.js'
import TransactionsTable from '../browser/transactions_table.js'
import BlockTransactionsTable from '../browser/block_transactions_table.js'
import FileSaver from 'file-saver'
const logger = Logger.create('Actions') // eslint-disable-line no-unused-vars

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

const submitSearchResult = function ({term, context}) {
  if (context === 'tagpack') {
    this.menu.addSearchLabel(term)
    return
  }
  let first = (context === 'search' ? this.search : this.menu.search).getFirstResult()
  if (first) {
    clickSearchResult.call(this, {...first, context})
    return
  }
  term.split('\n').forEach((address) => {
    this.keyspaces.forEach(keyspace => {
      clickSearchResult.call(this, {id: address, type: 'address', keyspace, context: this.context})
    })
  })
}

const clickSearchResult = function ({id, type, keyspace, context}) {
  if (this.menu.search) {
    if (context === 'note' && type === 'address') {
      this.menu.addSearchAddress(id)
    } else if (context === 'tagpack' && type === 'label') {
      this.menu.addSearchLabel(id)
    }
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
}

const blurSearch = function (context) {
  let search = context === 'search' ? this.search : this.menu.search
  if (!search) return
  search.clear()
}

const removeLabel = function (label) {
  if (this.menu.getType() !== 'tagpack') return
  this.menu.removeSearchLabel(label)
}

const setLabels = function ({labels, id, keyspace}) {
  if (this.menu.getType() !== 'tagpack') return
  this.store.addTags(keyspace, id, labels)
  this.menu.hideMenu()
}

const resultNode = function ({context, result}) {
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
  if (!a.tags) {
    this.statusbar.addMsg('loadingTagsFor', a.type, a.id)
    this.mapResult(this.rest.tags(a.keyspace, {id: a.id, type: a.type}), 'resultTags', {id: a.id, type: a.type, keyspace: a.keyspace})
  }
  this.statusbar.removeLoading(a.id)
  this.statusbar.addMsg('loaded', a.type, a.id)
  addNode.call(this, {id: a.id, type: a.type, keyspace: a.keyspace, anchor})
}

const resultTransactionForBrowser = function ({result}) {
  this.browser.setTransaction(result)
  historyPushState(result.keyspace, 'transaction', result.tx_hash)
  this.statusbar.removeLoading(result.tx_hash)
  this.statusbar.addMsg('loaded', 'transaction', result.tx_hash)
}

const resultLabelForBrowser = function ({result, context}) {
  this.browser.setLabel(result)
  historyPushState(null, 'label', result.label)
  this.statusbar.removeLoading(context)
  this.statusbar.addMsg('loaded', 'label', result.label)
  initTagsTable.call(this, {id: result.label, type: 'label', index: 0})
}

const resultBlockForBrowser = function ({result}) {
  this.browser.setBlock(result)
  historyPushState(result.keyspace, 'block', result.height)
  this.statusbar.removeLoading(result.height)
  this.statusbar.addMsg('loaded', 'block', result.height)
}

const selectNode = function ([type, nodeId]) {
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
}

// user clicks address in a table
const clickAddress = function ({address, keyspace}) {
  if (this.keyspaces.indexOf(keyspace) === -1) return
  this.statusbar.addLoading(address)
  this.mapResult(this.rest.node(keyspace, {id: address, type: 'address'}), 'resultNode', address)
}

// user clicks label in a table
const clickLabel = function ({label, keyspace}) {
  this.statusbar.addLoading(label)
  this.mapResult(this.rest.label(label), 'resultLabelForBrowser', label)
}

const deselect = function () {
  this.browser.deselect()
  this.config.hide()
  this.graph.deselect()
}

const clickTransaction = function (data) {
  this.browser.loading.add(data.tx_hash)
  this.statusbar.addLoading(data.tx_hash)
  this.mapResult(this.rest.transaction(data.keyspace, data.tx_hash), 'resultTransactionForBrowser', data.tx_hash)
}

const clickBlock = function ({height, keyspace}) {
  this.browser.loading.add(height)
  this.statusbar.addLoading(height)
  this.mapResult(this.rest.block(keyspace, height), 'resultBlockForBrowser', height)
}

const loadAddresses = function ({keyspace, params, nextPage, request, drawCallback}) {
  this.statusbar.addMsg('loading', 'addresses')
  this.mapResult(this.rest.addresses(keyspace, {params, nextPage, pagesize: request.length}), 'resultAddresses', {page: nextPage, request, drawCallback})
}

const resultAddresses = function ({context, result}) {
  this.statusbar.addMsg('loaded', 'addresses')
  this.browser.setResponse({...context, result})
}

const loadTransactions = function ({keyspace, params, nextPage, request, drawCallback}) {
  this.statusbar.addMsg('loading', 'transactions')
  this.mapResult(this.rest.transactions(keyspace, {params, nextPage, pagesize: request.length}), 'resultTransactions', {page: nextPage, request, drawCallback})
}

const resultTransactions = function ({context, result}) {
  this.statusbar.addMsg('loaded', 'transactions')
  this.browser.setResponse({...context, result})
}

const loadTags = function ({keyspace, params, nextPage, request, drawCallback}) {
  this.statusbar.addMsg('loading', 'tags')
  this.mapResult(this.rest.tags(keyspace, {id: params[0], type: params[1], nextPage, pagesize: request.length}), 'resultTagsTable', {page: nextPage, request, drawCallback})
}

const resultTagsTable = function ({context, result}) {
  this.browser.setResponse({...context, result})
}

const initTransactionsTable = function (request) {
  this.browser.initTransactionsTable(request)
}

const initBlockTransactionsTable = function (request) {
  this.browser.initBlockTransactionsTable(request)
}

const initAddressesTable = function (request) {
  this.browser.initAddressesTable(request)
}

const initAddressesTableWithEntity = function ({id, keyspace}) {
  let entity = this.store.get(keyspace, 'entity', id)
  if (!entity) return
  this.browser.setEntity(entity)
  this.browser.initAddressesTable({index: 0, id, type: 'entity'})
}

const initTagsTable = function (request) {
  this.browser.initTagsTable(request)
}

const initIndegreeTable = function (request) {
  this.browser.initNeighborsTable(request, false)
}

const initOutdegreeTable = function (request) {
  this.browser.initNeighborsTable(request, true)
}

const initNeighborsTableWithNode = function ({id, keyspace, type, isOutgoing}) {
  let node = this.store.get(keyspace, type, id)
  if (!node) return
  if (type === 'address') {
    this.browser.setAddress(node)
  } else if (type === 'entity') {
    this.browser.setEntity(node)
  }
  this.browser.initNeighborsTable({id, keyspace, type, index: 0}, isOutgoing)
}

const initTxInputsTable = function (request) {
  this.browser.initTxAddressesTable(request, false)
}

const initTxOutputsTable = function (request) {
  this.browser.initTxAddressesTable(request, true)
}

const loadNeighbors = function ({keyspace, params, nextPage, request, drawCallback}) {
  let id = params[0]
  let type = params[1]
  let isOutgoing = params[2]
  this.mapResult(this.rest.neighbors(keyspace, id, type, isOutgoing, request.length, nextPage), 'resultNeighbors', {page: nextPage, request, drawCallback})
}

const resultNeighbors = function ({context, result}) {
  this.browser.setResponse({...context, result})
}

const selectNeighbor = function (data) {
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
    resultNode.call(this, { context, result: o })
  }
}

const selectAddress = function (data) {
  logger.debug('selectAdress', data)
  if (!data.address || !data.keyspace) return
  this.mapResult(this.rest.node(data.keyspace, {id: data.address, type: 'address'}), 'resultNode', data.address)
}

const addNode = function ({id, type, keyspace, anchor}) {
  this.graph.adding.add(id)
  this.statusbar.addLoading(id)
  addNodeCont.call(this, {context: {stage: 1, id, type, keyspace, anchor}, result: null})
}

const addNodeCont = function ({context, result}) {
  let anchor = context.anchor
  let keyspace = context.keyspace
  if (context.stage === 1 && context.type && context.id) {
    let a = this.store.get(context.keyspace, context.type, context.id)
    if (!a) {
      this.statusbar.addMsg('loading', context.type, context.id)
      this.mapResult(this.rest.node(keyspace, {type: context.type, id: context.id}), 'addNodeCont', {stage: 2, keyspace, anchor})
    } else {
      addNodeCont.call(this, {context: {stage: 2, keyspace, anchor}, result: a})
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
      addNodeCont.call(this, {context: {stage: 4, id: o.id, type: o.type, keyspace, anchor}})
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
    let e = this.store.add({...resultCopy, forAddresses: [context.addressId]})
    if (!e.tags) {
      this.statusbar.addMsg('loadingTagsFor', e.type, e.id)
      this.mapResult(this.rest.tags(keyspace, {id: e.id, type: e.type}), 'resultTags', {id: e.id, type: e.type, keyspace: e.keyspace})
    }
    addNodeCont.call(this, ({context: {stage: 4, id: context.addressId, type: 'address', keyspace, anchor}}))
  } else if (context.stage === 4 && context.id && context.type) {
    let backCall = {msg: 'addNodeCont', data: {context: { ...context, stage: 5 }}}
    let o = this.store.get(context.keyspace, context.type, context.id)
    if (context.type === 'entity') {
      excourseLoadDegree.call(this, {context: {backCall, id: o.id, type: 'entity', keyspace}})
    } else if (context.type === 'address') {
      if (o.entity && !o.entity.mockup) {
        excourseLoadDegree.call(this, {context: {backCall, id: o.entity.id, type: 'entity', keyspace}})
      } else {
        functions[backCall.msg].call(this, backCall.data)
      }
    }
  } else if (context.stage === 5 && context.id && context.type) {
    let o = this.store.get(context.keyspace, context.type, context.id)
    if (!o.tags) {
      this.statusbar.addMsg('loadingTagsFor', o.type, o.id)
      this.mapResult(this.rest.tags(keyspace, {id: o.id, type: o.type}), 'resultTags', {id: o.id, type: o.type, keyspace: o.keyspace})
    }
    this.graph.add(o, context.anchor)
    this.browser.setUpdate('tables_with_addresses')
    this.statusbar.removeLoading(o.id)
  }
}

const excourseLoadDegree = function ({context, result}) {
  let keyspace = context.keyspace
  if (!context.stage) {
    let o = this.store.get(context.keyspace, context.type, context.id)
    if (o.in_degree >= degreeThreshold) {
      excourseLoadDegree.call(this, {context: { ...context, stage: 2 }})
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
    if (o.out_degree >= degreeThreshold || o.out_degree === o.outgoing.size()) {
      functions[context.backCall.msg].call(this, context.backCall.data)
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
    functions[context.backCall.msg].call(this, context.backCall.data)
  }
}

const resultTags = function ({context, result}) {
  let o = this.store.get(context.keyspace, context.type, context.id)
  logger.debug('o', o)
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
}

const loadEgonet = function ({id, type, keyspace, isOutgoing, limit}) {
  this.statusbar.addLoading(`neighbors of ${type} ${id[0]}`)
  this.statusbar.addMsg('loadingNeighbors', id, type, isOutgoing)
  this.mapResult(this.rest.neighbors(keyspace, id[0], type, isOutgoing, limit), 'resultEgonet', {id, type, isOutgoing, keyspace})
}

const resultEgonet = function ({context, result}) {
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
    addNode.call(this, {id: node.id, type: node.nodeType, keyspace: node.keyspace, anchor})
  })
}

const loadEntityAddresses = function ({id, keyspace, limit}) {
  this.statusbar.addMsg('loadingEntityAddresses', id, limit)
  this.statusbar.addLoading('addresses of entity ' + id[0])
  this.mapResult(this.rest.entityAddresses(keyspace, id[0], limit), 'resultEntityAddresses', {id, keyspace})
}

const removeEntityAddresses = function (id) {
  this.graph.removeEntityAddresses(id)
  this.browser.setUpdate('tables_with_addresses')
}

const resultEntityAddresses = function ({context, result}) {
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
}

const changeEntityLabel = function (labelType) {
  this.config.setEntityLabel(labelType)
  this.graph.setEntityLabel(labelType)
}

const changeAddressLabel = function (labelType) {
  this.config.setAddressLabel(labelType)
  this.graph.setAddressLabel(labelType)
}

const changeCurrency = function (currency) {
  this.browser.setCurrency(currency)
  this.graph.setCurrency(currency)
  this.layout.setCurrency(currency)
}

const changeTxLabel = function (type) {
  this.graph.setTxLabel(type)
  this.config.setTxLabel(type)
}

const removeNode = function ([nodeType, nodeId]) {
  this.statusbar.addMsg('removeNode', nodeType, nodeId[0])
  this.graph.remove(nodeType, nodeId)
  this.browser.setUpdate('tables_with_addresses')
}

const inputNotes = function ({id, type, keyspace, note}) {
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
}

const toggleConfig = function () {
  this.config.toggleConfig()
}

const noteDialog = function ({x, y, nodeId, nodeType}) {
  let o = this.store.get(nodeId[2], nodeType, nodeId[0])
  this.menu.showNodeDialog(x, y, {dialog: nodeType === 'entity' ? 'note' : 'tagpack', data: o})
  selectNode.call(this, [nodeType, nodeId])
}

const searchNeighborsDialog = function ({x, y, id, type, isOutgoing}) {
  this.menu.showNodeDialog(x, y, {dialog: 'search', id, type, isOutgoing})
  selectNode.call(this, [type, id])
}

const changeSearchCriterion = function (criterion) {
  this.menu.setSearchCriterion(criterion)
}

const changeSearchCategory = function (category) {
  this.menu.setSearchCategory(category)
}

const hideContextmenu = function () {
  this.menu.hideMenu()
}

const blank = function () {
  if (this.isReplaying) return
  if (!this.promptUnsavedWork('start a new graph')) return
  this.createComponents()
  this.loadCategories()
}

const save = function (stage) {
  if (this.isReplaying) return
  if (!stage) {
    // update status bar before starting serializing
    this.statusbar.addMsg('saving')
    this.config.hide()
    save.call(this, true)
    return
  }
  let filename = moment().format('YYYY-MM-DD HH-mm-ss') + '.gs'
  this.statusbar.addMsg('saved', filename)
  this.download(filename, this.serialize())
}

const saveNotes = function (stage) {
  if (this.isReplaying) return
  if (!stage) {
    // update status bar before starting serializing
    this.statusbar.addMsg('saving')
    this.config.hide()
    saveNotes.call(this, true)
    return
  }
  let filename = moment().format('YYYY-MM-DD HH-mm-ss') + '.notes.gs'
  this.statusbar.addMsg('saved', filename)
  this.download(filename, this.serializeNotes())
}

const saveYAML = function (stage) {
  if (this.isReplaying) return
  if (!stage) {
    // update status bar before starting serializing
    this.statusbar.addMsg('saving')
    this.config.hide()
    saveYAML.call(this, true)
    return
  }
  let filename = moment().format('YYYY-MM-DD HH-mm-ss') + '.yaml'
  this.statusbar.addMsg('saved', filename)
  this.download(filename, this.generateTagpack())
}

const saveTagsJSON = function (stage) {
  if (this.isReplaying) return
  if (!stage) {
    // update status bar before starting serializing
    this.statusbar.addMsg('saving')
    this.config.hide()
    saveTagsJSON.call(this, true)
    return
  }
  let filename = moment().format('YYYY-MM-DD HH-mm-ss') + '.json'
  this.statusbar.addMsg('saved', filename)
  this.download(filename, this.generateTagsJSON())
}

const exportRestLogs = function () {
  if (this.isReplaying) return
  let csv = 'timestamp,url\n'
  this.rest.getLogs().forEach(row => {
    row[0] = moment(row[0]).format()
    csv += row.join(',') + '\n'
  })
  let filename = 'REST calls ' + moment().format('YYYY-MM-DD HH-mm-ss') + '.csv'
  let blob = new Blob([csv], {type: 'text/csv;charset=utf-8'}) // eslint-disable-line no-undef
  FileSaver.saveAs(blob, filename)
}

const exportSvg = function () {
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
}

const load = function () {
  if (this.isReplaying) return
  if (this.promptUnsavedWork('load another file')) {
    this.layout.triggerFileLoad('load')
  }
  this.config.hide()
}

const loadNotes = function () {
  if (this.isReplaying) return
  this.layout.triggerFileLoad('loadNotes')
  this.config.hide()
}

const loadYAML = function () {
  if (this.isReplaying) return
  this.layout.triggerFileLoad('loadYAML')
  this.config.hide()
}

const loadTagsJSON = function () {
  if (this.isReplaying) return
  this.layout.triggerFileLoad('loadTagsJSON')
  this.config.hide()
}

const loadFile = function (params) {
  let type = params[0]
  let data = params[1]
  let filename = params[2]
  let stage = params[3]
  if (!stage) {
    this.statusbar.addMsg('loadFile', filename)
    loadFile.call(this, [type, data, filename, true])
    return
  }
  this.statusbar.addMsg('loadedFile', filename)
  if (type === 'load') {
    this.deserialize(data)
  } else if (type === 'loadNotes') {
    this.deserializeNotes(data)
  } else if (type === 'loadYAML') {
    this.loadTagpack(data)
  } else if (type === 'loadTagsJSON') {
    this.loadTagsJSON(data)
  }
}

const showLogs = function () {
  this.statusbar.show()
}

const hideLogs = function () {
  this.statusbar.hide()
}

const moreLogs = function () {
  this.statusbar.moreLogs()
}

const toggleErrorLogs = function () {
  this.statusbar.toggleErrorLogs()
}

const gohome = function () {
  this.showLandingpage = true
  this.browser.destroyComponentsFrom(1)
  this.landingpage.setUpdate(true)
  this.layout.setUpdate(true)
}

const sortEntityAddresses = function ({entity, property}) {
  this.graph.sortEntityAddresses(entity, property)
}

const dragNode = function ({id, type, dx, dy}) {
  this.graph.dragNode(id, type, dx, dy)
}

const dragNodeEnd = function ({id, type}) {
  this.graph.dragNodeEnd(id, type)
}

const changeSearchDepth = function (value) {
  this.menu.setSearchDepth(value)
}

const changeSearchBreadth = function (value) {
  this.menu.setSearchBreadth(value)
}

const changeSkipNumAddresses = function (value) {
  this.menu.setSkipNumAddresses(value)
}

const searchNeighbors = function (params) {
  logger.debug('search params', params)
  this.statusbar.addSearching(params)
  this.mapResult(this.rest.searchNeighbors(params), 'resultSearchNeighbors', params)
  this.menu.hideMenu()
}

const resultSearchNeighbors = function ({result, context}) {
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
      excourseLoadDegree.call(this, {context: {backCall, id: node.id, type: context.type, keyspace: result.keyspace}})

      let parent = this.graph.add(node, anchor)
      // link addresses to entity and add them (if any returned due of 'addresses' search criterion)
      pathnode.matching_addresses.forEach(address => {
        address.entity = pathnode.node.entity
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
}

const redrawGraph = function () {
  this.graph.setUpdate('layers')
}

const createSnapshot = function () {
  this.graph.createSnapshot()
  this.layout.disableButton('undo', !this.graph.thereAreMorePreviousSnapshots())
  this.layout.disableButton('redo', !this.graph.thereAreMoreNextSnapshots())
}

const undo = function () {
  this.graph.loadPreviousSnapshot(this.store)
  this.browser.setUpdate('tables_with_addresses')
  this.layout.disableButton('undo', !this.graph.thereAreMorePreviousSnapshots())
  this.layout.disableButton('redo', !this.graph.thereAreMoreNextSnapshots())
}

const redo = function () {
  this.graph.loadNextSnapshot(this.store)
  this.browser.setUpdate('tables_with_addresses')
  this.layout.disableButton('undo', !this.graph.thereAreMorePreviousSnapshots())
  this.layout.disableButton('redo', !this.graph.thereAreMoreNextSnapshots())
}

const disableUndoRedo = function () {
  this.layout.disableButton('undo', true)
  this.layout.disableButton('redo', true)
}

const toggleSearchTable = function () {
  this.browser.toggleSearchTable()
}

const toggleLegend = function () {
  this.config.setCategoryColors(this.graph.getCategoryColors())
  this.config.toggleLegend()
}

const toggleExport = function () {
  this.config.toggleExport()
}

const toggleImport = function () {
  this.config.toggleImport()
}

const downloadTable = function () {
  if (this.isReplaying) return
  let table = this.browser.content[1]
  if (!table) return
  let url
  if (table instanceof NeighborsTable) {
    let params = table.getParams()
    url = this.rest.neighbors(params.keyspace, params.id, params.type, params.isOutgoing, 0, 0, true)
  } else if (table instanceof TagsTable) {
    let params = table.getParams()
    url = this.rest.tags(params.keyspace, params, true)
  } else if (table instanceof TransactionsTable || table instanceof BlockTransactionsTable) {
    let params = table.getParams()
    url = this.rest.transactions(params.keyspace, {params: [params.id, params.type]}, true)
  }
  if (url) {
    this.layout.triggerDownloadViaLink(url)
  }
}

const downloadTagsAsJSON = function () {
  if (this.isReplaying) return
  let table = this.browser.content[1]
  if (!table) return
  if (!(table instanceof TagsTable)) return
  let tags = table.data.map(this.tagToJSON)
  let blob = new Blob([JSON.stringify(tags)], {type: 'text/json;charset=utf-8'}) // eslint-disable-line no-undef
  let params = table.getParams()
  let filename = `tags of ${params.type} ${params.id}.json`
  FileSaver.saveAs(blob, filename)
}

const addAllToGraph = function () {
  let table = this.browser.content[1]
  if (!table) return
  table.data.forEach(row => {
    if (!row.keyspace) {
      if (row.currency) row.keyspace = row.currency.toLowerCase()
      else row.keyspace = table.keyspace
    }
    functions[table.selectMessage].call(this, row)
  })
}

const tooltip = function (type) {
  this.statusbar.showTooltip(type)
}

const hideTooltip = function (type) {
  this.statusbar.showTooltip('')
}

const changeLocale = function (locale) {
  moment.locale(locale)
  numeral.locale(locale)
  this.locale = locale
  this.config.setLocale(locale)
  this.browser.setUpdate('locale')
  this.graph.setUpdate('layers')
}

const receiveCategories = function ({result}) {
  if (!Array.isArray(result)) return
  result.sort((a, b) => a.id - b.id)
  this.store.setCategories(result)
  result = result.map(({category}) => category)
  this.graph.setCategories(result)
  this.menu.setCategories(result)
  this.config.setCategoryColors(this.graph.getCategoryColors())
}

const receiveCategoryColors = function ({result}) {
  this.graph.setCategoryColors(result)
  this.config.setCategoryColors(this.graph.getCategoryColors())
}

const functions = {
  submitSearchResult,
  clickSearchResult,
  blurSearch,
  removeLabel,
  setLabels,
  resultNode,
  resultTransactionForBrowser,
  resultLabelForBrowser,
  resultBlockForBrowser,
  selectNode,
  clickAddress,
  clickLabel,
  deselect,
  clickTransaction,
  clickBlock,
  loadAddresses,
  resultAddresses,
  loadTransactions,
  resultTransactions,
  loadTags,
  resultTagsTable,
  initTransactionsTable,
  initBlockTransactionsTable,
  initAddressesTable,
  initAddressesTableWithEntity,
  initTagsTable,
  initIndegreeTable,
  initOutdegreeTable,
  initNeighborsTableWithNode,
  initTxInputsTable,
  initTxOutputsTable,
  loadNeighbors,
  resultNeighbors,
  selectNeighbor,
  selectAddress,
  addNode,
  addNodeCont,
  excourseLoadDegree,
  resultTags,
  loadEgonet,
  resultEgonet,
  loadEntityAddresses,
  removeEntityAddresses,
  resultEntityAddresses,
  changeEntityLabel,
  changeAddressLabel,
  changeCurrency,
  changeTxLabel,
  removeNode,
  inputNotes,
  toggleConfig,
  noteDialog,
  searchNeighborsDialog,
  changeSearchCriterion,
  changeSearchCategory,
  hideContextmenu,
  blank,
  save,
  saveNotes,
  saveYAML,
  saveTagsJSON,
  exportRestLogs,
  load,
  loadNotes,
  loadYAML,
  loadTagsJSON,
  loadFile,
  showLogs,
  hideLogs,
  moreLogs,
  toggleErrorLogs,
  gohome,
  sortEntityAddresses,
  dragNode,
  dragNodeEnd,
  changeSearchDepth,
  changeSearchBreadth,
  changeSkipNumAddresses,
  searchNeighbors,
  resultSearchNeighbors,
  redrawGraph,
  createSnapshot,
  undo,
  redo,
  disableUndoRedo,
  toggleSearchTable,
  toggleLegend,
  toggleExport,
  toggleImport,
  downloadTable,
  downloadTagsAsJSON,
  addAllToGraph,
  tooltip,
  hideTooltip,
  changeLocale,
  receiveCategories,
  receiveCategoryColors,
  exportSvg
}

export default functions
