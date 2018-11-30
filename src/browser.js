import {set} from 'd3-collection'
import layout from './browser/layout.html'
import Address from './browser/address.js'
import Cluster from './browser/cluster.js'
import Transaction from './browser/transaction.js'
import Table from './browser/table.js'
import TransactionsTable from './browser/transactions_table.js'
import AddressesTable from './browser/addresses_table.js'
import TagsTable from './browser/tags_table.js'
import TransactionAddressesTable from './browser/transaction_addresses_table.js'
import NeighborsTable from './browser/neighbors_table.js'
import Component from './component.js'

export default class Browser extends Component {
  constructor (dispatcher, currency) {
    super()
    this.currency = currency
    this.loading = set()
    this.dispatcher = dispatcher
    this.content = []
    this.visible = false
  }
  deselect () {
    this.visible = false
    this.shouldUpdate(true)
  }
  destroyComponentsFrom (index) {
    this.content.forEach((content, i) => {
      if (i >= index) content.destroy()
    })
    this.content = this.content.slice(0, index)
  }
  isShowingOutgoingNeighbors () {
    let last = this.content[this.content.length - 1]
    if (last instanceof NeighborsTable) {
      return last.isOutgoing
    }
    return null
  }
  getCurrentNode () {
    if (this.content[0] instanceof Address || this.content[0] instanceof Cluster) {
      return this.content[0].data
    }
    return null
  }
  setCurrency (currency) {
    this.currency = currency
    this.content.forEach(comp => comp.setCurrency(currency))
  }
  setAddress (address) {
    this.activeTab = 'address'
    this.visible = true
    this.destroyComponentsFrom(0)
    this.content = [ new Address(this.dispatcher, address, 0, this.currency) ]
    this.shouldUpdate(true)
  }
  setTransaction (tx) {
    this.activeTab = 'transactions'
    this.visible = true
    this.destroyComponentsFrom(0)
    this.content = [
      new Transaction(this.dispatcher, tx, 0, this.currency),
      new TransactionAddressesTable(this.dispatcher, tx.inputs, 'Inputs', 1, this.currency, tx.keyspace),
      new TransactionAddressesTable(this.dispatcher, tx.outputs, 'Outputs', 1, this.currency, tx.keyspace)
    ]
    this.shouldUpdate(true)
  }
  setCluster (cluster) {
    this.activeTab = 'address'
    this.visible = true
    this.destroyComponentsFrom(0)
    this.content = [ new Cluster(this.dispatcher, cluster, 0, this.currency) ]
    this.shouldUpdate(true)
  }
  setResultNode (object) {
    console.log('setResultNode', object)

    this.visible = true
    this.loading.remove(object.id)
    this.destroyComponentsFrom(0)
    if (object.type === 'address') {
      this.content[0] = new Address(this.dispatcher, object, 0, this.currency)
    } else if (object.type === 'cluster') {
      this.content[0] = new Cluster(this.dispatcher, object, 0, this.currency)
    }
    this.shouldUpdate(true)
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
    if (this.content[request.index + 1] instanceof TransactionsTable) return
    let total = comp.data.noIncomingTxs + comp.data.noOutgoingTxs
    this.destroyComponentsFrom(request.index + 1)
    this.content.push(new TransactionsTable(this.dispatcher, request.index + 1, total, request.id, request.type, this.currency, keyspace))
    this.shouldUpdate(true)
  }
  initAddressesTable (request) {
    if (request.index !== 0 && !request.index) return
    let last = this.content[request.index]
    if (!(last instanceof Cluster)) return
    let keyspace = last.data.keyspace
    if (this.content[request.index + 1] instanceof AddressesTable) return
    let total = last.data.noAddresses
    this.destroyComponentsFrom(request.index + 1)
    this.content.push(new AddressesTable(this.dispatcher, request.index + 1, total, request.id, this.currency, keyspace))
    this.shouldUpdate(true)
  }
  initTagsTable (request) {
    if (request.index !== 0 && !request.index) return
    let last = this.content[request.index]
    if (!(last instanceof Cluster) && !(last instanceof Address)) return
    if (this.content[request.index + 1] instanceof TagsTable) return
    this.destroyComponentsFrom(request.index + 1)
    let keyspace = last.data.keyspace
    this.content.push(new TagsTable(this.dispatcher, request.index + 1, last.data.tags, request.id, request.type, keyspace))

    this.shouldUpdate(true)
  }
  initNeighborsTable (request, isOutgoing) {
    if (request.index !== 0 && !request.index) return
    let last = this.content[request.index]
    if (!(last instanceof Cluster) && !(last instanceof Address)) return
    if (this.content[request.index + 1] instanceof NeighborsTable &&
        this.content[request.index + 1].isOutgoing == isOutgoing
    ) return

    let keyspace = last.data.keyspace
    let total = isOutgoing ? last.data.out_degree : last.data.in_degree
    this.destroyComponentsFrom(request.index + 1)
    this.content.push(new NeighborsTable(this.dispatcher, request.index + 1, total, request.id, request.type, isOutgoing, this.currency, keyspace))
    this.shouldUpdate(true)
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    console.log('browser', this.shouldUpdate())
    if (this.shouldUpdate() === true) {
      super.render()
      this.root.innerHTML = layout
      let data = this.root.querySelector('#browser-data')
      let c = 0
      if (!this.visible) {
        this.root.style.display = 'none'
      } else {
        this.root.style.display = 'block'
      }
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
      return this.root
    }
    this.content.forEach(comp => comp.render())
    super.render()
    return this.root
  }
}
