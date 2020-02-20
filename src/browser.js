import {set} from 'd3-collection'
import layout from './browser/layout.html'
import Address from './browser/address.js'
import Entity from './browser/entity.js'
import Label from './browser/label.js'
import Transaction from './browser/transaction.js'
import Block from './browser/block.js'
import Table from './browser/table.js'
import TransactionsTable from './browser/transactions_table.js'
import BlockTransactionsTable from './browser/block_transactions_table.js'
import AddressesTable from './browser/addresses_table.js'
import TagsTable from './browser/tags_table.js'
import TransactionAddressesTable from './browser/transaction_addresses_table.js'
import NeighborsTable from './browser/neighbors_table.js'
import Component from './component.js'
import {addClass, removeClass} from './template_utils.js'
import Logger from './logger.js'

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
  }
  setKeyspaces (keyspaces) {
    this.supportedKeyspaces = keyspaces
  }
  deselect () {
    this.visible = false
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
    let last = this.content[this.content.length - 1]
    if (last instanceof NeighborsTable) {
      return last.isOutgoing
    }
    return null
  }
  getCurrentNode () {
    if (this.content[0] instanceof Address || this.content[0] instanceof Entity) {
      return this.content[0].data
    }
    return null
  }
  setNodeChecker (func) {
    this.nodeChecker = func
  }
  setCurrency (currency) {
    this.currency = currency
    this.content.forEach(comp => comp.setCurrency(currency))
  }
  setLabel (label) {
    this.visible = true
    this.setUpdate('visibility')
    if (this.content[0] instanceof Label && this.content[0].data.label === label.label) return
    this.destroyComponentsFrom(0)
    this.content = [ new Label(this.dispatcher, label, 0, this.currency) ]
    this.setUpdate('content')
  }
  setAddress (address) {
    this.activeTab = 'address'
    this.visible = true
    this.setUpdate('visibility')
    if (this.content[0] instanceof Address && this.content[0].data.id === address.id) return
    this.destroyComponentsFrom(0)
    this.content = [ new Address(this.dispatcher, address, 0, this.currency) ]
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
  setEntity (entity) {
    this.activeTab = 'address'
    this.visible = true
    this.setUpdate('visibility')
    if (this.content[0] instanceof Entity && this.content[0].data.id === entity.id) return
    this.destroyComponentsFrom(0)
    this.content = [ new Entity(this.dispatcher, entity, 0, this.currency) ]
    this.setUpdate('content')
  }
  setResultNode (object) {
    this.visible = true
    this.loading.remove(object.id)
    this.destroyComponentsFrom(0)
    if (object.type === 'address') {
      this.content[0] = new Address(this.dispatcher, object, 0, this.currency)
    } else if (object.type === 'entity') {
      this.content[0] = new Entity(this.dispatcher, object, 0, this.currency)
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
    let comp = this.content[request.index]
    if (!(comp instanceof Address)) return
    let keyspace = comp.data.keyspace
    this.setUpdate('content')
    if (this.content[request.index + 1] instanceof TransactionsTable) {
      this.destroyComponentsFrom(request.index + 1)
      return
    }
    this.destroyComponentsFrom(request.index + 1)
    comp.setCurrentOption('initTransactionsTable')
    let total = comp.data.no_incoming_txs + comp.data.no_outgoing_txs
    this.content.push(new TransactionsTable(this.dispatcher, request.index + 1, total, request.id, request.type, this.currency, keyspace))
  }
  initBlockTransactionsTable (request) {
    if (request.index !== 0 && !request.index) return
    let comp = this.content[request.index]
    if (!(comp instanceof Block)) return
    let keyspace = comp.data.keyspace
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
    let last = this.content[request.index]
    if (!(last instanceof Entity)) return
    let keyspace = last.data.keyspace
    this.setUpdate('content')
    if (this.content[request.index + 1] instanceof AddressesTable) {
      this.destroyComponentsFrom(request.index + 1)
      return
    }
    this.destroyComponentsFrom(request.index + 1)
    last.setCurrentOption('initAddressesTable')
    let total = last.data.no_addresses
    this.content.push(new AddressesTable(this.dispatcher, request.index + 1, total, request.id, this.currency, keyspace, this.nodeChecker))
  }
  initTagsTable (request) {
    if (request.index !== 0 && !request.index) return
    let last = this.content[request.index]
    let fromLabel = last instanceof Label
    if (!(last instanceof Entity) && !(last instanceof Address) && !(fromLabel)) return
    this.setUpdate('content')
    if (this.content[request.index + 1] instanceof TagsTable) {
      this.destroyComponentsFrom(request.index + 1)
      return
    }
    this.destroyComponentsFrom(request.index + 1)
    last.setCurrentOption('initTagsTable')
    let keyspace = last.data.keyspace
    let total = fromLabel ? last.data.address_count : last.data.tags.length
    this.content.push(new TagsTable(this.dispatcher, request.index + 1, total, last.data.tags || [], request.id, request.type, this.currency, keyspace, this.nodeChecker, this.supportedKeyspaces))
  }
  initNeighborsTable (request, isOutgoing) {
    if (request.index !== 0 && !request.index) return
    let last = this.content[request.index]
    if (!(last instanceof Entity) && !(last instanceof Address)) return
    this.setUpdate('content')
    if (this.content[request.index + 1] instanceof NeighborsTable &&
        this.content[request.index + 1].isOutgoing == isOutgoing // eslint-disable-line eqeqeq
    ) {
      this.destroyComponentsFrom(request.index + 1)
      return
    }
    this.destroyComponentsFrom(request.index + 1)
    last.setCurrentOption(isOutgoing ? 'initOutdegreeTable' : 'initIndegreeTable')
    let keyspace = last.data.keyspace
    let total = isOutgoing ? last.data.out_degree : last.data.in_degree
    this.content.push(new NeighborsTable(this.dispatcher, request.index + 1, total, request.id, request.type, isOutgoing, this.currency, keyspace, this.nodeChecker))
  }
  initTxAddressesTable (request, isOutgoing) {
    if (request.index !== 0 && !request.index) return
    let last = this.content[request.index]
    if (!(last instanceof Transaction)) return

    this.setUpdate('content')
    if (this.content[request.index + 1] instanceof TransactionAddressesTable &&
        this.content[request.index + 1].isOutgoing == isOutgoing // eslint-disable-line eqeqeq
    ) {
      this.destroyComponentsFrom(request.index + 1)
      return
    }

    last.setCurrentOption(isOutgoing ? 'initTxOutputsTable' : 'initTxInputsTable')
    let keyspace = last.data.keyspace
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
    let frame = this.root
    if (!this.visible) {
      removeClass(frame, 'show')
    } else {
      addClass(frame, 'show')
    }
  }
  renderContent () {
    let data = this.root.querySelector('#browser-data')
    data.innerHTML = ''
    let c = 0
    this.content.forEach((comp) => {
      c += 1
      let compEl = document.createElement('div')
      compEl.className = 'browser-component'
      data.appendChild(compEl)
      comp.render(compEl)
      let options = comp.renderOptions()
      if (!options) return
      let el = document.createElement('div')
      el.className = 'browser-options ' + (c < this.content.length ? 'browser-options-short' : '')
      el.appendChild(options)
      data.appendChild(el)
    })
  }
}
