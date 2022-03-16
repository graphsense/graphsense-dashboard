import Logger from '../logger.js'
import moment from 'moment'
import { coinToSatoshi, coinToWei } from '../utils.js'
import { map } from 'd3-collection'
import Export from '../export/export.js'
import Table from '../browser/table.js'
import FileSaver from 'file-saver'
const logger = Logger.create('Actions') // eslint-disable-line no-unused-vars

const historyPushState = (keyspace, type, id, target) => {
  const s = window.history.state
  if (s && keyspace === s.keyspace && type === s.type && id == s.id && target == s.target) return // eslint-disable-line eqeqeq
  let url = '/'
  if (type && id) {
    const comps = [type, id]
    if (target) comps.push(target)
    url = '#!' + (keyspace ? keyspace + '/' : '') + comps.join('/')
  }
  window.history.replaceState({ keyspace, type, id, target }, null, url)
}

const submitSearchResult = function ({ term, context }) {
  logger.debug('this.menu.search', this.menu.search)
  if (context === 'search' || context === 'neighborsearch') {
    const first = (context === 'search' ? this.search : this.menu.search).getFirstResult()
    if (first) {
      clickSearchResult.call(this, { ...first, context })
      return
    }
  }
  if (context === 'tagpack') {
    this.menu.addSearchLabel(term)
    return
  }
  logger.debug('split', term)
  term.split('\n').forEach((address) => {
    this.keyspaces.forEach(keyspace => {
      clickSearchResult.call(this, { id: address, type: 'address', keyspace, context: context })
    })
  })
}

const clickSearchResult = function ({ id, type, keyspace, context }) {
  logger.debug('clickSerachResult', id, type, keyspace, context)
  if (this.menu.search) {
    if (context === 'neighborsearch' && type === 'address') {
      this.menu.addSearchAddress(id)
    } else if (context === 'tagpack' && (type === 'label' || type === 'userdefinedlabel')) {
      this.menu.addSearchLabel(id, true)
      if (type === 'label') {
        const level = this.menu.view.data.type
        this.mapResult(this.rest.label(this.keyspaces[0], {
          id,
          type,
          level
        }), 'resultLabelTagsForTag', id)
      } else {
        resultLabelTagsForTag.call(this, { result: this.store.getUserDefinedTagsForLabel(id), context: id })
      }
    }
    this.menu.search.clear()
    return
  }
  if (type !== 'label') {
    this.browser.loading.add(id)
    this.statusbar.addLoading(id)
  }
  if (this.showLandingpage) {
    this.showLandingpage = false
    this.layout.setUpdate(true)
  }
  this.search.clear()
  if (type === 'address' || type === 'entity') {
    this.graph.selectNodeWhenLoaded([id, type, keyspace])
    this.mapResult(this.rest.node(keyspace, { id, type }), 'resultNode', id)
  } else if (type === 'transaction') {
    this.mapResult(this.rest.transaction(keyspace, id), 'resultTransactionForBrowser', id)
  } else if (type === 'label') {
    labelForBrowser.call(this, id)
  } else if (type === 'block') {
    this.mapResult(this.rest.block(keyspace, id), 'resultBlockForBrowser', id)
  }
  if (type !== 'label') this.statusbar.addMsg('loading', type, id)
}

const blurSearch = function (context) {
  if (context === 'search') {
    this.search.clear()
  } else {
    this.menu.search.clearResults()
  }
}

const removeLabel = function (label) {
  if (this.menu.getType() !== 'tagpack') return
  this.menu.removeSearchLabel(label)
}

const setLabels = function ({ labels, input, id, keyspace }) {
  if (this.menu.getType() !== 'tagpack') return
  if (input.label && !labels[input.label]) {
    labels[input.label] = input
  }
  this.store.addTags(keyspace, id, labels)
  this.updateCategoriesByTags(Object.values(labels))
  this.graph.setUpdateNodes('address', id, true)
  this.menu.hideMenu()
}

const resultNode = function ({ context, result }) {
  const a = this.store.add(result)
  if (context && context.focusNode) {
    const f = this.store.get(context.focusNode.keyspace, context.focusNode.type, context.focusNode.id)
    if (f) {
      if (context.focusNode.isOutgoing === true) {
        this.store.linkOutgoing(f.id, a.id, f.keyspace, a.keyspace, context.focusNode.linkData)
      } else if (context.focusNode.isOutgoing === false) {
        this.store.linkOutgoing(a.id, f.id, a.keyspace, f.keyspace, context.focusNode.linkData)
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
    this.mapResult(this.rest.tags(a.keyspace, { id: a.id, type: a.type }), 'resultTags', { id: a.id, type: a.type, keyspace: a.keyspace })
  }
  this.statusbar.removeLoading(a.id)
  this.statusbar.addMsg('loaded', a.type, a.id)
  addNode.call(this, { id: a.id, type: a.type, keyspace: a.keyspace, anchor })
}

const resultTransactionForBrowser = function ({ result }) {
  this.browser.setTransaction(result)
  historyPushState(result.keyspace, 'transaction', result.tx_hash)
  this.statusbar.removeLoading(result.tx_hash)
  this.statusbar.addMsg('loaded', 'transaction', result.tx_hash)
}

const labelForBrowser = function (label) {
  this.browser.setLabel(label)
  historyPushState(null, 'label', label)
}

const resultLabelTagsForTag = function ({ result, context }) {
  this.menu.labelTagsData(result)
}

const resultBlockForBrowser = function ({ result }) {
  this.browser.setBlock(result)
  historyPushState(result.keyspace, 'block', result.height)
  this.statusbar.removeLoading(result.height)
  this.statusbar.addMsg('loaded', 'block', result.height)
}

const selectNode = function ([type, nodeId]) {
  if (this.graph.highlightMode) {
    this.graph.colorNode(type, nodeId)
    return
  }
  logger.debug('selectNode', type, nodeId, this.shiftPressed)
  const o = this.store.get(nodeId[2], type, nodeId[0])
  if (!o) {
    throw new Error(`selectNode: ${nodeId} of type ${type} not found in store`)
  }
  if (this.shiftPressed && this.graph.selectedNode) {
    if (this.graph.selectedNode.data.type !== type) return
  }
  historyPushState(o.keyspace, o.type, o.id)
  if (type === 'address') {
    this.browser.setAddress(o, this.shiftPressed)
  } else if (type === 'entity') {
    this.browser.setEntity(o, this.shiftPressed)
  }
  this.graph.selectNode(type, nodeId, this.shiftPressed)
}

// user clicks address/entity in a table
const clickAddress = function (data) {
  if (!Array.isArray(data)) data = [data]
  const type = data[0].address ? 'address' : 'entity'
  const found = new Set()
  data = data.filter(row => {
    if (found.has(row[type])) return false
    found.add(row[type])
    return true
  })
  data.forEach(data => {
    if (this.keyspaces.indexOf(data.keyspace) === -1) return
    this.statusbar.addLoading(data[type])
    this.mapResult(this.rest.node(data.keyspace, { id: data[type], type }), 'resultNode', data[type])
  })
}

// user clicks label in a table
const clickLabel = function ({ label, keyspace }) {
  this.statusbar.addLoading(label)
  labelForBrowser.call(this, label)
}

const deselect = function () {
  this.search.clear()
  this.browser.deselect()
  this.config.hide()
  this.graph.deselect()
  this.graph.deselectLink()
}

const clickTransaction = function (data) {
  this.browser.loading.add(data.tx_hash)
  this.statusbar.addLoading(data.tx_hash)
  this.mapResult(this.rest.transaction(data.keyspace, data.tx_hash), 'resultTransactionForBrowser', data.tx_hash)
}

const clickBlock = function ({ height, keyspace }) {
  this.browser.loading.add(height)
  this.statusbar.addLoading(height)
  this.mapResult(this.rest.block(keyspace, height), 'resultBlockForBrowser', height)
}

const loadAddresses = function ({ keyspace, params, nextPage, request, drawCallback }) {
  this.statusbar.addMsg('loading', 'addresses')
  this.mapResult(this.rest.addresses(keyspace, { params, nextPage, pagesize: request.length }), 'resultAddresses', { page: nextPage, request, drawCallback })
}

const resultAddresses = function ({ context, result }) {
  this.statusbar.addMsg('loaded', 'addresses')
  this.browser.setResponse({ ...context, result })
}

const loadTransactions = function ({ keyspace, params, nextPage, request, drawCallback }) {
  this.statusbar.addMsg('loading', 'transactions')
  this.mapResult(this.rest.transactions(keyspace, { params, nextPage, pagesize: request.length }), 'resultTransactions', { page: nextPage, request, drawCallback })
}

const loadLinkTransactions = function ({ keyspace, params, request, drawCallback }) {
  this.statusbar.addMsg('loadingLinkTransactions', request.source, request.target)
  this.mapResult(this.rest.linkTransactions(keyspace, params), 'resultTransactions', { request, drawCallback })
}

const resultTransactions = function ({ context, result }) {
  this.statusbar.addMsg('loaded', 'transactions')
  this.browser.setResponse({ ...context, result })
}

const loadTags = function ({ keyspace, params, nextPage, request, drawCallback }) {
  this.statusbar.addMsg('loading', 'tags')
  const type = params[1]
  if (type === 'label') {
    this.mapResult(this.rest.label(keyspace, {
      id: params[0],
      type,
      level: params[2],
      nextPage,
      pagesize: request.length
    }), 'resultTagsTable', { page: nextPage, request, drawCallback })
  } else {
    this.mapResult(this.rest.tags(keyspace, {
      id: params[0],
      type,
      level: params[2],
      nextPage,
      pagesize: request.length
    }), 'resultTagsTable', { page: nextPage, request, drawCallback })
  }
}

const resultTagsTable = function ({ context, result }) {
  this.browser.setResponse({ ...context, result })
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

const initAddressesTableWithEntity = function ({ id, keyspace }) {
  const entity = this.store.get(keyspace, 'entity', id)
  if (!entity) return
  this.browser.setEntity(entity)
  this.browser.initAddressesTable({ index: 0, id, type: 'entity' })
}

const initTagsTable = function (request) {
  this.browser.initTagsTable(request)
}

const initEntityTagsTable = function (request) {
  this.browser.initEntityTagsTable(request)
}

const initLinkTransactionsTable = function (request) {
  this.browser.initLinkTransactionsTable(request)
}
const initIndegreeTable = function (request) {
  this.browser.initNeighborsTable(request, false)
}

const initOutdegreeTable = function (request) {
  this.browser.initNeighborsTable(request, true)
}

const initNeighborsTableWithNode = function ({ id, type, isOutgoing }) {
  const keyspace = id[2]
  const nodeId = id
  id = id[0]
  selectNode.call(this, [type, nodeId])
  if (this.shiftPressed) return
  this.browser.initNeighborsTable({ id, keyspace, type, index: 0 }, isOutgoing)
}

const initTxInputsTable = function (request) {
  this.browser.initTxAddressesTable(request, false)
}

const initTxOutputsTable = function (request) {
  this.browser.initTxAddressesTable(request, true)
}

const loadNeighbors = function ({ keyspace, params, nextPage, request, drawCallback }) {
  const id = params[0]
  const type = params[1]
  const isOutgoing = params[2]
  this.mapResult(this.rest.neighbors({
    keyspace,
    id,
    type,
    isOutgoing,
    includeLabels: true,
    targets: null,
    pagesize: request.length,
    nextPage
  }), 'resultNeighbors', { page: nextPage, request, drawCallback })
}

const resultNeighbors = function ({ context, result }) {
  this.browser.setResponse({ ...context, result })
}

const selectNeighbor = function (data) {
  logger.debug('selectNeighbor', data)
  if (!Array.isArray(data)) data = [data]
  data.forEach(data => {
    if (!data.id || !data.nodeType || !data.keyspace) return
    const focusNode = this.browser.getCurrentNode()
    const anchorNode = this.graph.selectedNode
    const isOutgoing = this.browser.isShowingOutgoingNeighbors()
    const o = this.store.get(data.keyspace, data.nodeType, data.id)
    const context =
      {
        data,
        focusNode:
          {
            id: focusNode.id,
            type: focusNode.type,
            keyspace: data.keyspace,
            linkData: { ...data },
            isOutgoing: isOutgoing
          }
      }
    if (anchorNode) {
      context.anchorNode = { nodeId: anchorNode.id, isOutgoing }
    }
    if (!o) {
      this.statusbar.addLoading(data.id)
      this.mapResult(this.rest.node(data.keyspace, { id: data.id, type: data.nodeType }), 'resultNode', context)
    } else {
      resultNode.call(this, { context, result: o })
    }
  })
}

const selectAddress = function (data) {
  logger.debug('selectAdress', data)
  if (!Array.isArray(data)) data = [data]
  data.forEach(data => {
    if (!data.address || !data.keyspace) return
    this.mapResult(this.rest.node(data.keyspace, { id: data.address, type: 'address' }), 'resultNode', data.address)
  })
}

const addNode = function ({ id, type, keyspace, anchor }) {
  this.graph.adding.add(id)
  this.statusbar.addLoading(id)
  addNodeCont.call(this, { context: { stage: 1, id, type, keyspace, anchor }, result: null })
}

const addNodeCont = function ({ context, result }) {
  logger.debug('addNodeCont ', context, result)
  const anchor = context.anchor
  const keyspace = context.keyspace
  if (context.stage === 1 && context.type && context.id) {
    const a = this.store.get(context.keyspace, context.type, context.id)
    if (!a) {
      this.statusbar.addMsg('loading', context.type, context.id)
      this.mapResult(this.rest.node(keyspace, { type: context.type, id: context.id }), 'addNodeCont', { stage: 2, keyspace, anchor })
    } else {
      addNodeCont.call(this, { context: { stage: 2, keyspace, anchor }, result: a })
    }
  } else if (context.stage === 2 && result) {
    const o = this.store.add(result)
    this.statusbar.addMsg('loaded', o.type, o.id)
    if (anchor && anchor.isOutgoing === false) {
      // incoming neighbor node
      this.store.linkOutgoing(o.id, anchor.nodeId[0], o.keyspace, anchor.nodeId[2])
    }
    if (!this.graph.adding.has(o.id)) return
    logger.debug('entity', o.entity)
    if (o.type === 'address' && !o.entity) {
      this.statusbar.addMsg('loadingEntityFor', o.id)
      this.mapResult(this.rest.entityForAddress(keyspace, o.id), 'addNodeCont', { stage: 3, addressId: o.id, keyspace, anchor })
    } else {
      addNodeCont.call(this, { context: { stage: 4, id: o.id, type: o.type, keyspace, anchor } })
    }
  } else if (context.stage === 3 && context.addressId) {
    if (!this.graph.adding.has(context.addressId)) return
    const resultCopy = { ...result }
    // seems there exist addresses without entity ...
    // so mockup entity with the address id
    if (!resultCopy.entity) {
      resultCopy.entity = 'mockup' + context.addressId
      resultCopy.mockup = true
      this.statusbar.addMsg('noEntityFor', context.addressId)
    } else {
      this.statusbar.addMsg('loadedEntityFor', context.addressId)
    }
    const e = this.store.add({ ...resultCopy, forAddresses: [context.addressId] })
    if (anchor) {
      const a = this.store.get(keyspace, 'address', anchor.nodeId[0])
      if (a && a.entity) {
        const b = this.store.get(keyspace, 'address', context.addressId)
        if (b && b.entity) {
          if (anchor.isOutgoing === false) {
            this.store.linkOutgoing(b.entity.id, a.entity.id, keyspace, keyspace)
          } else {
            this.store.linkOutgoing(a.entity.id, b.entity.id, keyspace, keyspace)
          }
        }
      }
    }
    if (!e.tags) {
      this.statusbar.addMsg('loadingTagsFor', e.type, e.id)
      this.mapResult(this.rest.tags(keyspace, { id: e.id, type: e.type }), 'resultTags', { id: e.id, type: e.type, keyspace: e.keyspace })
    } else {
      this.updateCategoriesByTags(e.tags)
    }
    addNodeCont.call(this, ({ context: { stage: 4, id: context.addressId, type: 'address', keyspace, anchor } }))
  } else if (context.stage === 4 && context.id && context.type) {
    const backCall = { msg: 'addNodeCont', data: { context: { ...context, stage: 5 } } }
    const o = this.store.get(context.keyspace, context.type, context.id)
    if (context.type === 'entity') {
      excourseLoadDegree.call(this, { context: { backCall, id: o.id, type: 'entity', keyspace } })
    } else if (context.type === 'address') {
      if (o.entity && !o.entity.mockup) {
        excourseLoadDegree.call(this, { context: { backCall, id: o.entity.id, type: 'entity', keyspace } })
      } else {
        functions[backCall.msg].call(this, backCall.data)
      }
    }
  } else if (context.stage === 5 && context.id && context.type) {
    const o = this.store.get(context.keyspace, context.type, context.id)
    if (!o.tags) {
      this.statusbar.addMsg('loadingTagsFor', o.type, o.id)
      this.mapResult(this.rest.tags(keyspace, { id: o.id, type: o.type }), 'resultTags', { id: o.id, type: o.type, keyspace: o.keyspace })
    } else {
      this.updateCategoriesByTags(o.tags)
    }
    const node = this.graph.add(o, context.anchor)
    this.browser.setUpdate('tables_with_addresses')
    this.statusbar.removeLoading(o.id)
    if (context.keyspace === 'eth' && context.type === 'entity') {
      loadEntityAddresses.call(this, { id: node.id, keyspace: context.keyspace, limit: 1 })
    }
  }
}

const excourseLoadDegree = function ({ context, result }) {
  const keyspace = context.keyspace
  if (!context.stage) {
    const o = this.store.get(context.keyspace, context.type, context.id)
    this.statusbar.addMsg('loadingNeighbors', o.id, o.type, false)
    const targets = this.store.getEntityKeys(context.keyspace)
    this.mapResult(this.rest.neighbors({
      keyspace,
      id: o.id,
      type: o.type,
      isOutgoing: false,
      targets
    }), 'excourseLoadDegree', { ...context, stage: 2 })
  } else if (context.stage === 2) {
    this.statusbar.addMsg('loadedNeighbors', context.id, context.type, false)
    const o = this.store.get(context.keyspace, context.type, context.id)
    if (result && result.neighbors) {
      // add the node in context to the outgoing set of incoming relations
      result.neighbors.forEach((neighbor) => {
        if (neighbor.nodeType !== o.type) return
        this.store.linkOutgoing(neighbor.id, o.id, neighbor.keyspace, o.keyspace, neighbor)
      })
    }
    if (o.out_degree === o.outgoing.size()) {
      functions[context.backCall.msg].call(this, context.backCall.data)
      return
    }
    this.statusbar.addMsg('loadingNeighbors', o.id, o.type, true)
    const targets = this.store.getEntityKeys(context.keyspace)
    this.mapResult(this.rest.neighbors({
      keyspace,
      id: o.id,
      type: o.type,
      isOutgoing: true,
      targets
    }), 'excourseLoadDegree', { ...context, stage: 3 })
  } else if (context.stage === 3) {
    const o = this.store.get(context.keyspace, context.type, context.id)
    this.statusbar.addMsg('loadedNeighbors', context.id, context.type, true)
    if (result && result.neighbors) {
      // add outgoing relations to the node in context
      result.neighbors.forEach((neighbor) => {
        if (neighbor.nodeType !== o.type) return
        this.store.linkOutgoing(o.id, neighbor.id, o.keyspace, neighbor.keyspace, neighbor)
      })
      // this.storeRelations(result.neighbors, o, o.keyspace, true)
    }
    functions[context.backCall.msg].call(this, context.backCall.data)
  }
}

const resultTags = function ({ context, result }) {
  const o = this.store.get(context.keyspace, context.type, context.id)
  logger.debug('o', o)
  this.statusbar.addMsg('loadedTagsFor', o.type, o.id)
  o.tags = result || []
  this.graph.setUpdateNodes(context.type, context.id, true)

  this.updateCategoriesByTags(o.tags)
}

const loadEgonet = function ({ id, type, keyspace, isOutgoing, limit }) {
  this.statusbar.addMsg('loadingNeighbors', id, type, isOutgoing)
  this.mapResult(this.rest.neighbors({
    keyspace,
    id: id[0],
    type,
    isOutgoing,
    pagesize: limit
  }), 'resultEgonet', { id, type, isOutgoing, keyspace })
}

const resultEgonet = function ({ context, result }) {
  const a = this.store.get(context.keyspace, context.type, context.id[0])
  this.statusbar.addMsg('loadedNeighbors', context.id[0], context.type, context.isOutgoing)
  this.statusbar.removeLoading(`neighbors of ${context.type} ${context.id[0]}`)
  result.neighbors.forEach((node) => {
    if (node.id === context.id[0] || node.nodeType !== context.type) return
    const anchor = {
      nodeId: context.id,
      nodeType: context.type,
      isOutgoing: context.isOutgoing
    }
    if (context.isOutgoing === true) {
      this.store.linkOutgoing(a.id, node.id, a.keyspace, node.keyspace, node)
    } else if (context.isOutgoing === false) {
      this.store.linkOutgoing(node.id, a.id, node.keyspace, a.keyspace, node)
    }
    addNode.call(this, { id: node.id, type: node.nodeType, keyspace: node.keyspace, anchor })
  })
}

const loadEntityAddresses = function ({ id, keyspace, limit }) {
  this.statusbar.addMsg('loadingEntityAddresses', id, limit)
  this.mapResult(this.rest.entityAddresses(keyspace, id[0], limit), 'resultEntityAddresses', { id, keyspace })
}

const removeEntityAddresses = function (id) {
  this.graph.removeEntityAddresses(id)
  this.browser.setUpdate('tables_with_addresses')
}

const resultEntityAddresses = function ({ context, result }) {
  const id = context && context.id
  const keyspace = context && context.keyspace
  const addresses = []
  this.statusbar.removeLoading('addresses of entity ' + id[0])
  result.addresses.forEach((address) => {
    const copy = { ...address, toEntity: id[0] }
    const a = this.store.add(copy)
    addresses.push(a)
    if (!a.tags) {
      const request = { id: a.id, type: 'address', keyspace }
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

const toggleConfig = function () {
  this.config.toggleConfig()
}

const noteDialog = function ({ x, y, nodeId, nodeType }) {
  const o = this.store.get(nodeId[2], nodeType, nodeId[0])
  this.menu.showNodeDialog(x, y, { dialog: 'tagpack', data: o })
  selectNode.call(this, [nodeType, nodeId])
}

const searchNeighborsDialog = function ({ x, y, id, type, isOutgoing }) {
  this.menu.showNodeDialog(x, y, { dialog: 'neighborsearch', id, type, isOutgoing })
  selectNode.call(this, [type, id])
}

const changeSearchCriterion = function (criterion) {
  this.menu.setSearchCriterion(criterion)
}

const changeSearchCategory = function (category) {
  this.menu.setSearchCategory(category)
}

const changeUserDefinedTag = function ({ label, data }) {
  this.menu.setTagpack(label, data)
}

const hideContextmenu = function () {
  this.menu.hideMenu()
}

const blank = function () {
  if (this.isReplaying) return
  if (!this.promptUnsavedWork('start a new graph')) return
  this.createComponents()
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
  const filename = moment().format('YYYY-MM-DD HH-mm-ss') + '.gs'
  this.statusbar.addMsg('saved', filename)
  this.download(filename, this.serialize())
}

const exportYAML = function (nodeType) {
  const modal = new Export(this.call, { creator: this.meta.creator || this.meta.investigator }, 'tagpack', nodeType)
  this.layout.showModal(modal)
}

const exportYAMLentity = function () {
  exportYAML.call(this, 'entity')
}

const exportYAMLaddress = function () {
  exportYAML.call(this, 'address')
}

const saveYAML = function (stage, nodeType) {
  if (this.isReplaying) return
  if (!stage) {
    // update status bar before starting serializing
    this.statusbar.addMsg('saving')
    this.config.hide()
    saveYAML.call(this, true, nodeType)
    return
  }
  const filename = moment().format('YYYY-MM-DD HH-mm-ss') + '.yaml'
  this.statusbar.addMsg('saved', filename)
  this.download(filename, this.generateTagpack(nodeType))
  this.layout.hideModal()
}

const saveYAMLentity = function () {
  saveYAML.call(this, null, 'entity')
}

const saveYAMLaddress = function () {
  saveYAML.call(this, null, 'address')
}

const inputMetaData = function (meta) {
  this.meta = { ...this.meta, ...meta }
  this.omitUpdate()
}

const exportReport = function () {
  const meta = { ...this.meta }
  delete meta.creator
  const modal = new Export(this.call, meta, 'report')
  this.layout.showModal(modal)
}

const saveReport = function (stage) {
  if (this.isReplaying) return
  if (!stage) {
    // update status bar before starting serializing
    this.statusbar.addMsg('saving')
    this.config.hide()
    saveReport.call(this, true)
    return
  }
  const filename = moment().format('YYYY-MM-DD HH-mm-ss') + '.report.pdf'
  this.statusbar.addMsg('saved', filename)
  this.generateReportPDF().then(file => {
    logger.debug('otuput', file)
    this.download(filename, file)
    this.call('hideModal')
  })
}

const hideModal = function () {
  this.layout.hideModal()
}

const saveReportJSON = function (stage) {
  if (this.isReplaying) return
  if (!stage) {
    // update status bar before starting serializing
    this.statusbar.addMsg('saving')
    this.config.hide()
    saveReportJSON.call(this, true)
    return
  }
  const filename = moment().format('YYYY-MM-DD HH-mm-ss') + '.report.json'
  this.statusbar.addMsg('saved', filename)
  this.download(filename, JSON.stringify(this.generateReportJSON(), null, 2))
  this.layout.hideModal()
}

const exportRestLogs = function () {
  if (this.isReplaying) return
  let csv = 'timestamp,url\n'
  this.rest.getLogs().forEach(row => {
    row[0] = moment(row[0]).format()
    csv += row.join(',') + '\n'
  })
  const filename = 'REST calls ' + moment().format('YYYY-MM-DD HH-mm-ss') + '.csv'
  const blob = new Blob([csv], { type: 'text/csv;charset=utf-8' }) // eslint-disable-line no-undef
  FileSaver.saveAs(blob, filename)
}

const exportSvg = function () {
  if (this.isReplaying) return
  const classMap = map()
  let rules = ([...document.styleSheets]).filter(({ href }) => href)
    .filter(({ href }) => href.includes('main.css'))
  if (!rules) return
  rules = rules[0].rules
  for (let i = 0; i < rules.length; i++) {
    const selectorText = rules[i].selectorText
    const cssText = rules[i].cssText
    if (!selectorText || !selectorText.startsWith('svg')) continue
    const s = selectorText.replace('.', '').replace('svg', '').trim()
    classMap.set(s, cssText.split('{')[1].replace('}', ''))
  }
  classMap.set('linkPath', classMap.get('linkPath.hover'))
  classMap.set('linkText', classMap.get('linkText.hover'))
  let svg = this.graph.getSvg()
  // replace classes by inline styles
  svg = svg.replace(new RegExp('class="(.+?)"', 'g'), (_, classes) => {
    const repl = classes.split(' ')
      .map(cls => classMap.get(cls) || '')
      .join('')
    if (repl.trim() === '') return ''
    return 'style="' + repl.replace(/"/g, '\'').replace('"', '\'') + '"'
  })
  // replace double quotes and quot (which was created by innerHTML)
  svg = svg.replace(new RegExp('style="(.+?)"', 'g'), (_, style) => 'style="' + style.replace(/&quot;/g, '\'') + '"')
  // merge double style definitions
  svg = svg.replace(new RegExp('style="([^"]+?)"([^>]+?)style="([^"]+?)"', 'g'), 'style="$1$3" $2')
  const filename = moment().format('YYYY-MM-DD HH-mm-ss') + '.svg'
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

const loadYAML = function () {
  if (this.isReplaying) return
  this.layout.triggerFileLoad('loadYAML')
  this.config.hide()
}

const loadFile = function (params) {
  const type = params[0]
  const data = params[1]
  const filename = params[2]
  const stage = params[3]
  if (!stage) {
    this.statusbar.addMsg('loadFile', filename)
    loadFile.call(this, [type, data, filename, true])
    return
  }
  this.statusbar.addMsg('loadedFile', filename)
  if (type === 'load') {
    this.deserialize(data)
  } else if (type === 'loadYAML') {
    this.loadTagpack(data)
  }
  this.graph.dirty = true
  this.graph.createSnapshot()
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

const sortEntityAddresses = function ({ entity, property }) {
  this.graph.sortEntityAddresses(entity, property)
}

const dragNodeStart = function ({ id, type, x, y }) {
  this.graph.dragNodeStart(id, type, x, y)
}

const dragNode = function ({ x, y }) {
  this.graph.dragNode(x, y)
}

const dragNodeEnd = function () {
  this.graph.dragNodeEnd()
}

const changeMin = function (value) {
  this.menu.setMin(value)
}

const changeMax = function (value) {
  this.menu.setMax(value)
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
  this.statusbar.addSearching(params)
  params.params.currency = this.layout.currency
  const convert = params.id[2] === 'eth' ? coinToWei : coinToSatoshi
  if (this.layout.currency === 'value') {
    params.params.min = params.params.min ? convert(params.params.min) : null
    params.params.max = params.params.max ? convert(params.params.max) : null
  }
  this.mapResult(this.rest.searchNeighbors(params), 'resultSearchNeighbors', params)
  // make another call for local matching entity tags
  if (params.params.category) {
    let ids = this.store.getUserDefinedTags2()
    ids = ids
      .filter(tag => !!tag.entity && tag.category === params.params.category)
      .map(tag => tag.entity)
    ids = [...new Set(ids)]
    if (ids.length > 0) {
      delete params.params.category
      params.params.ids = ids
      this.mapResult(this.rest.searchNeighbors(params), 'resultSearchNeighbors', params)
    }
  }
  this.menu.hideMenu()
}

const resultSearchNeighbors = function ({ result, context }) {
  this.statusbar.removeSearching(context)
  let count = 0
  const add = (anchor, paths) => {
    if (!paths) {
      count++
      return
    }
    paths.forEach(pathnode => {
      pathnode.node.keyspace = result.keyspace
      pathnode.relation.keyspace = result.keyspace

      // store relations
      const node = this.store.add(pathnode.node)
      const src = context.isOutgoing ? anchor.nodeId[0] : node.id
      const dst = context.isOutgoing ? node.id : anchor.nodeId[0]
      this.store.linkOutgoing(src, dst, result.keyspace, result.keyspace, pathnode.relation)

      // fetch all relations
      const backCall = { msg: 'redrawGraph', data: null }
      excourseLoadDegree.call(this, { context: { backCall, id: node.id, type: context.type, keyspace: result.keyspace } })

      const parent = this.graph.add(node, anchor)
      // link addresses to entity and add them (if any returned due of 'addresses' search criterion)
      pathnode.matching_addresses.forEach(address => {
        address.entity = pathnode.node.entity
        const a = this.store.add(address)
        // anchor the address to its entity
        this.graph.add(a, { nodeId: parent.id, nodeType: 'entity' })
      })
      if (pathnode.node.tags) this.updateCategoriesByTags(pathnode.node.tags)
      add({ nodeId: parent.id, isOutgoing: context.isOutgoing }, pathnode.paths)
    })
  }
  add({ nodeId: context.id, isOutgoing: context.isOutgoing }, result.paths)
  this.statusbar.addMsg('searchResult', count)
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
  this.config.setCategoryColors(this.graph.getCategoryColors(), this.store.getCategories())
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
  let table = null
  for (let i = 0; i < this.browser.content.length; i++) {
    if (this.browser.content[i] instanceof Table) {
      table = this.browser.content[i]
      break
    }
  }
  if (!table) return

  table.table.buttons('.buttons-csv').trigger()
}

const addAllToGraph = function () {
  const table = this.browser.content[1]
  if (!table) return
  const rows = []
  table.data.forEach(row => {
    if (!row.keyspace) {
      if (row.currency) row.keyspace = row.currency.toLowerCase()
      else row.keyspace = table.keyspace
    }
    rows.push(row)
  })
  functions[table.selectMessage].call(this, rows)
}

const hoverLink = function () {
  this.statusbar.showTooltip('link')
}

const leaveLink = function () {
  this.statusbar.showTooltip('')
}

const hoverShadow = function () {
  this.statusbar.showTooltip('shadow')
}

const leaveShadow = function () {
  this.statusbar.showTooltip('')
}

const hoverNode = function ([type, id]) {
  this.debounce('hover' + id, () => {
    this.graph.hoverNode(id, type, true)
    this.statusbar.showTooltip(type)
  }, 1)
}

const leaveNode = function ([type, id]) {
  this.debounce('hover' + id, () => {
    this.graph.hoverNode(id, type, false)
    this.statusbar.showTooltip('')
  }, 1)
}

const receiveConcepts = function ({ result, context }) {
  if (!Array.isArray(result)) return
  result.sort((a, b) => a.id - b.id)
  this.browser.addConcepts(result)
  this.menu.setConcepts(result, context)
  this.config.setConcepts(result)
}

const receiveConceptsColors = function ({ result }) {
  this.graph.setCategoryColors(result)
}

const pressShift = function () {
  this.shiftPressed = true
}

const releaseShift = function () {
  this.shiftPressed = false
}

const clickLink = function ({ source, target }) {
  this.graph.selectLink(source, target)
  const t = this.store.getOutgoing(source.id[2], source.data.type, source.id[0], target.id[0])
  logger.debug('t', t)
  this.browser.setLink({
    keyspace: source.id[2],
    type: source.data.type,
    source: source.id[0],
    target: target.id[0],
    no_txs: t.no_txs,
    value: t.value
  })
  historyPushState(source.id[2], source.data.type + 'link', source.id[0], target.id[0])
  initLinkTransactionsTable.call(this, { source: source.id[0], target: target.id[0], type: source.data.type, index: 0 })
}

const removeLink = function ([source, target]) {
  this.graph.removeLink(source, target)
}

const removeShadow = function ([source, target]) {
  this.graph.removeShadow(source, target)
}

const receiveTaxonomies = function ({ result }) {
  if (!result) return
  result.forEach(({ taxonomy }) => this.mapResult(this.rest.concepts(taxonomy), 'receiveConcepts', taxonomy))
}

const sortCategories = function (ids) {
  this.store.setCategories(ids)
  this.config.setCategoryColors(this.graph.getCategoryColors(), ids)
  this.graph.setUpdate('layers')
}

const screenDragStart = function (coords) {
  this.graph.screenDragStart(coords)
}

const screenDragMove = function (coords) {
  this.graph.screenDragMove(coords)
}

const screenDragStop = function () {
  this.graph.screenDragStop()
}

const screenZoom = function (zoom) {
  this.graph.screenZoom(zoom)
}

const logout = function () {
  this.mapResult(this.rest.logout(), 'loggedout')
}

const loggedout = function () {
  window.history.go(0)
}

const toggleHighlight = function () {
  if (!this.config.toggleHighlight()) {
    this.graph.highlightModeOff()
  }
}

const addHighlight = function (color) {
  this.config.addHighlight(color)
  this.graph.highlightModeOn(color)
}

const pickHighlight = function (color) {
  this.config.pickHighlight(color)
  this.graph.highlightModeOn(color)
}

const inputHighlight = function ([color, title]) {
  this.config.inputHighlight(color, title)
}

const removeHighlight = function (color) {
  this.config.removeHighlight(color)
  this.graph.removeHighlight(color)
}

const editHighlight = function ([color, newColor]) {
  this.config.editHighlight(color, newColor)
  this.graph.editHighlight(color, newColor)
}

const colorNode = function ([type, id]) {
  this.graph.colorNode(type, id)
}

const clickSidebarMyEntityTags = function () {
  this.browser.initMyEntityTagsTable(this.store.getUserDefinedTags2().filter(tag => tag.entity))
}

const clickSidebarMyAddressTags = function () {
  this.browser.initMyAddressTagsTable(this.store.getUserDefinedTags2().filter(tag => tag.address))
}

const resize = function () {
  this.graph.resize()
}

const countdownRatelimitReset = function () {
  this.statusbar.setUpdate('ratelimit')
}

const functions = {
  submitSearchResult,
  clickSearchResult,
  blurSearch,
  removeLabel,
  setLabels,
  resultNode,
  resultTransactionForBrowser,
  labelForBrowser,
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
  loadLinkTransactions,
  resultTransactions,
  loadTags,
  resultTagsTable,
  initTransactionsTable,
  initBlockTransactionsTable,
  initAddressesTable,
  initAddressesTableWithEntity,
  initTagsTable,
  initEntityTagsTable,
  initLinkTransactionsTable,
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
  resultLabelTagsForTag,
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
  toggleConfig,
  noteDialog,
  searchNeighborsDialog,
  changeSearchCriterion,
  changeSearchCategory,
  changeUserDefinedTag,
  hideContextmenu,
  blank,
  save,
  exportReport,
  saveReport,
  saveReportJSON,
  saveYAML,
  saveYAMLentity,
  saveYAMLaddress,
  exportRestLogs,
  load,
  loadYAML,
  loadFile,
  showLogs,
  hideLogs,
  moreLogs,
  toggleErrorLogs,
  gohome,
  sortEntityAddresses,
  dragNodeStart,
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
  addAllToGraph,
  hoverNode,
  leaveNode,
  hoverLink,
  leaveLink,
  hoverShadow,
  leaveShadow,
  receiveConcepts,
  receiveConceptsColors,
  exportSvg,
  inputMetaData,
  pressShift,
  releaseShift,
  clickLink,
  hideModal,
  exportYAML,
  exportYAMLentity,
  exportYAMLaddress,
  changeMin,
  changeMax,
  receiveTaxonomies,
  sortCategories,
  screenDragStart,
  screenDragMove,
  screenDragStop,
  screenZoom,
  logout,
  loggedout,
  toggleHighlight,
  addHighlight,
  pickHighlight,
  inputHighlight,
  removeHighlight,
  editHighlight,
  colorNode,
  clickSidebarMyEntityTags,
  clickSidebarMyAddressTags,
  resize,
  removeLink,
  removeShadow,
  countdownRatelimitReset
}

export default functions
