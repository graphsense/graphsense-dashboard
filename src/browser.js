import { set } from 'd3-collection'
import layout from './browser/layout.html'
import Address from './browser/address.js'
import Entity from './browser/entity.js'
import Label from './browser/label.js'
import Transaction from './browser/transaction.js'
import Block from './browser/block.js'
import Link from './browser/link.js'
import Table from './browser/table.js'
import TransactionsTable from './browser/transactions_table.js'
import BlockTransactionsTable from './browser/block_transactions_table.js'
import AddressesTable from './browser/addresses_table.js'
import TagsTable from './browser/tags_table.js'
import LinkTransactionsTable from './browser/link_transactions_table.js'
import TransactionAddressesTable from './browser/transaction_addresses_table.js'
import NeighborsTable from './browser/neighbors_table.js'
import Component from './component.js'
import { addClass, removeClass } from './template_utils.js'
import Logger from './logger.js'
import { nodesIdentical } from './utils.js'
import { maxTransactionListSize } from './globals.js'

const logger = Logger.create('Browser') // eslint-disable-line no-unused-vars

export default class Browser extends Component {
  constructor (dispatcher, currency, supportedKeyspaces) {
    super()
    this.currency = currency
    this.supportedKeyspaces = supportedKeyspaces
    this.loading = set()
    this.dispatcher = dispatcher
    this.content = []
    this.visible = false
    this.categories = {}
  }

  setKeyspaces (keyspaces) {
    this.supportedKeyspaces = keyspaces
  }

  addConcepts (concepts) {
    concepts.forEach(({ label, uri }) => {
      this.categories[label] = uri
    })
    this.setUpdate('tagstable')
  }

  deselect () {
    this.visible = false
    this.destroyComponentsFrom(0)
    this.setUpdate('visibility')
  }

  destroyComponentsFrom (index) {
    this.content.forEach((content, i) => {
      if (i >= index) content.destroy()
    })
    this.content = this.content.slice(0, index)
  }

  toggleSearchTable () {
    if (!(this.content[1] instanceof Table)) return
    this.content[1].toggleSearch()
  }

  isShowingOutgoingNeighbors () {
    const last = this.content[this.content.length - 1]
    if (last instanceof NeighborsTable) {
      return last.isOutgoing
    }
    return null
  }

  getCurrentNode () {
    if (!(this.content[0] instanceof Address) && !(this.content[0] instanceof Entity)) return null
    if (this.content[0].data.length > 1) return null
    return this.content[0].data[0]
  }

  setNodeChecker (func) {
    this.nodeChecker = func
  }

  setCurrency (currency) {
    this.currency = currency
    this.content.forEach(comp => comp.setCurrency(currency))
  }

  setLabel (label, tags) {
    this.visible = true
    this.setUpdate('visibility')
    if (this.content[0] instanceof Label && this.content[0].data.label === label.label) return
    this.destroyComponentsFrom(0)
    this.content = [new Label(this.dispatcher, { label, tags }, 0, this.currency)]
    this.setUpdate('content')
  }

  setLink (data) {
    this.visible = true
    this.setUpdate('visibility')
    if (this.content[0] instanceof Link &&
        this.content[0].data.source === data.source &&
        this.content[0].data.target === data.target
    ) return
    this.destroyComponentsFrom(0)
    this.content = [new Link(this.dispatcher, data, 0, this.currency)]
    this.setUpdate('content')
  }

  setAddress (address, multi) {
    this.activeTab = 'address'
    this.visible = true
    this.setUpdate('visibility')
    const selected = this.content[0] instanceof Address && this.content[0].data
    logger.debug('selected', multi, selected)
    this.destroyComponentsFrom(0)
    let addresses = [address]
    if (selected && multi) {
      addresses = addresses.concat(selected)
    }
    if (selected && selected.filter(s => nodesIdentical(s, address)).length === 1 && multi) {
      addresses = addresses.filter(a => !nodesIdentical(a, address))
    }
    if (addresses.length === 0) {
      this.deselect()
      return
    }
    this.content = [new Address(this.dispatcher, addresses, 0, this.currency)]
    this.setUpdate('content')
  }

  setTransaction (tx) {
    this.activeTab = 'transactions'
    this.visible = true
    this.destroyComponentsFrom(0)
    this.content = [
      new Transaction(this.dispatcher, tx, 0, this.currency)
    ]
    this.setUpdate('content')
  }

  setBlock (block) {
    this.activeTab = 'block'
    this.visible = true
    this.destroyComponentsFrom(0)
    this.content = [
      new Block(this.dispatcher, block, 0, this.currency)
    ]
    this.setUpdate('content')
  }

  setEntity (entity, multi) {
    this.activeTab = 'address'
    this.visible = true
    this.setUpdate('visibility')
    const selected = this.content[0] instanceof Entity && this.content[0].data
    this.destroyComponentsFrom(0)
    let entities = [entity]
    if (selected && multi) {
      entities = entities.concat(selected)
    }
    if (selected && selected.filter(s => nodesIdentical(s, entity)).length === 1 && multi) {
      entities = entities.filter(a => !nodesIdentical(a, entity))
    }
    if (entities.length === 0) {
      this.deselect()
      return
    }
    this.content = [new Entity(this.dispatcher, entities, 0, this.currency)]
    this.setUpdate('content')
  }

  setResultNode (object) {
    this.visible = true
    this.loading.remove(object.id)
    this.destroyComponentsFrom(0)
    if (object.type === 'address') {
      this.content[0] = new Address(this.dispatcher, [object], 0, this.currency)
    } else if (object.type === 'entity') {
      this.content[0] = new Entity(this.dispatcher, [object], 0, this.currency)
    }
    this.setUpdate('content')
  }

  setResponse (response) {
    this.content.forEach((comp) => {
      if (!(comp instanceof Table)) return
      comp.setResponse(response)
    })
  }

  initTransactionsTable (request) {
    if (request.index !== 0 && !request.index) return
    const comp = this.content[request.index]
    if (!(comp instanceof Address)) return
    if (comp.data.length > 1) return
    const keyspace = comp.data[0].keyspace
    this.setUpdate('content')
    if (this.content[request.index + 1] instanceof TransactionsTable) {
      this.destroyComponentsFrom(request.index + 1)
      return
    }
    this.destroyComponentsFrom(request.index + 1)
    comp.setCurrentOption('initTransactionsTable')
    const total = comp.data[0].no_incoming_txs + comp.data[0].no_outgoing_txs
    this.content.push(new TransactionsTable(this.dispatcher, request.index + 1, total, request.id, request.type, this.currency, keyspace))
  }

  initBlockTransactionsTable (request) {
    if (request.index !== 0 && !request.index) return
    const comp = this.content[request.index]
    if (!(comp instanceof Block)) return
    const keyspace = comp.data.keyspace
    this.setUpdate('content')
    if (this.content[request.index + 1] instanceof BlockTransactionsTable) {
      this.destroyComponentsFrom(request.index + 1)
      return
    }
    this.destroyComponentsFrom(request.index + 1)
    comp.setCurrentOption('initBlockTransactionsTable')
    this.content.push(new BlockTransactionsTable(this.dispatcher, request.index + 1, comp.data.no_txs, request.id, this.currency, keyspace))
  }

  initAddressesTable (request) {
    if (request.index !== 0 && !request.index) return
    const last = this.content[request.index]
    if (!(last instanceof Entity)) return
    if (last.data.length > 1) return
    const keyspace = last.data[0].keyspace
    this.setUpdate('content')
    if (this.content[request.index + 1] instanceof AddressesTable) {
      this.destroyComponentsFrom(request.index + 1)
      return
    }
    this.destroyComponentsFrom(request.index + 1)
    last.setCurrentOption('initAddressesTable')
    const total = last.data[0].no_addresses
    this.content.push(new AddressesTable(this.dispatcher, request.index + 1, total, request.id, this.currency, keyspace, this.nodeChecker))
  }

  initTagsTable (request) {
    if (request.index !== 0 && !request.index) return
    const last = this.content[request.index]
    const fromLabel = (last instanceof Label)
    if (!(last instanceof Entity) && !(last instanceof Address) && !fromLabel) return
    let data = last.data
    if (!fromLabel) {
      if (last.data.length > 1) return
      data = last.data[0]
    }
    this.setUpdate('content')
    if (this.content[request.index + 1] instanceof TagsTable) {
      this.destroyComponentsFrom(request.index + 1)
      return
    }
    this.destroyComponentsFrom(request.index + 1)
    last.setCurrentOption('initTagsTable')
    const keyspace = data.keyspace
    const total = data.tags.length
    this.content.push(new TagsTable(this.dispatcher, request.index + 1, total, data.tags || [], request.id, request.type, this.currency, keyspace, this.nodeChecker, this.supportedKeyspaces, this.categories))
  }

  initLinkTransactionsTable (request) {
    if (request.index !== 0 && !request.index) return
    const last = this.content[request.index]
    if (!(last instanceof Link)) return
    this.setUpdate('content')
    if (this.content[request.index + 1] instanceof LinkTransactionsTable) {
      this.destroyComponentsFrom(request.index + 1)
      return
    }
    this.destroyComponentsFrom(request.index + 1)
    last.setCurrentOption('initLinkTransactionsTable')
    const keyspace = last.data.keyspace
    const total = Math.min(last.data.no_txs, maxTransactionListSize)
    this.content.push(new LinkTransactionsTable(this.dispatcher, request.index + 1, last.data.source, last.data.target, total, last.data.type, this.currency, keyspace))
  }

  initNeighborsTable (request, isOutgoing) {
    logger.debug('initNeighborsTable', request, isOutgoing)
    if (request.index !== 0 && !request.index) return
    const last = this.content[request.index]
    if (!(last instanceof Entity) && !(last instanceof Address)) return
    logger.debug('initNeighborsTable data.length', last.data.length)
    if (last.data.length > 1) return
    this.setUpdate('content')
    if (this.content[request.index + 1] instanceof NeighborsTable &&
        this.content[request.index + 1].isOutgoing == isOutgoing // eslint-disable-line eqeqeq
    ) {
      this.destroyComponentsFrom(request.index + 1)
      return
    }
    this.destroyComponentsFrom(request.index + 1)
    last.setCurrentOption(isOutgoing ? 'initOutdegreeTable' : 'initIndegreeTable')
    const keyspace = last.data[0].keyspace
    const total = isOutgoing ? last.data[0].out_degree : last.data[0].in_degree
    this.content.push(new NeighborsTable(this.dispatcher, request.index + 1, total, request.id, request.type, isOutgoing, this.currency, keyspace, this.nodeChecker))
  }

  initTxAddressesTable (request, isOutgoing) {
    if (request.index !== 0 && !request.index) return
    const last = this.content[request.index]
    if (!(last instanceof Transaction)) return

    this.setUpdate('content')
    if (this.content[request.index + 1] instanceof TransactionAddressesTable &&
        this.content[request.index + 1].isOutgoing == isOutgoing // eslint-disable-line eqeqeq
    ) {
      this.destroyComponentsFrom(request.index + 1)
      return
    }

    last.setCurrentOption(isOutgoing ? 'initTxOutputsTable' : 'initTxInputsTable')
    const keyspace = last.data.keyspace
    this.destroyComponentsFrom(request.index + 1)
    this.content.push(new TransactionAddressesTable(this.dispatcher, last.data, isOutgoing, request.index + 1, this.currency, keyspace, this.nodeChecker))
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    logger.debug('shouldupdate', this.update)
    if (this.shouldUpdate(true)) {
      this.root.innerHTML = layout
      this.renderVisibility()
      this.renderContent()
      super.render()
      return this.root
    }
    if (this.shouldUpdate('content')) {
      this.renderVisibility()
      this.renderContent()
      super.render()
      return this.root
    }
    if (this.shouldUpdate('visibility')) {
      this.renderVisibility()
      super.render()
      return this.root
    }
    if (this.shouldUpdate('tables_with_addresses')) {
      this.content.filter(comp =>
        comp instanceof AddressesTable ||
        comp instanceof TagsTable ||
        comp instanceof NeighborsTable ||
        comp instanceof TransactionAddressesTable
      ).map(comp => comp.setUpdate('page'))
    }
    if (this.shouldUpdate('tagstable')) {
      this.content.filter(comp =>
        comp instanceof TagsTable
      ).map(comp => comp.setUpdate('page'))
    }
    if (this.shouldUpdate('locale')) {
      this.content.forEach(comp => {
        comp.setUpdate(comp instanceof Table ? 'page' : true)
      })
    }
    this.content.forEach(comp => comp.render())
    super.render()
    return this.root
  }

  renderVisibility () {
    const frame = this.root
    logger.debug('visibility', this.visible)
    if (!this.visible) {
      removeClass(frame, 'show')
    } else {
      addClass(frame, 'show')
    }
  }

  renderContent () {
    const data = this.root.querySelector('#browser-data')
    data.innerHTML = ''
    let c = 0
    this.content.forEach((comp) => {
      c += 1
      const compEl = document.createElement('div')
      compEl.className = 'browser-component'
      data.appendChild(compEl)
      comp.render(compEl)
      const options = comp.renderOptions()
      if (!options) return
      const el = document.createElement('div')
      el.className = 'browser-options ' + (c < this.content.length ? 'browser-options-short' : '')
      el.appendChild(options)
      data.appendChild(el)
    })
  }
}
