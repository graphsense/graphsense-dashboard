import {set} from 'd3-collection'
import layout from './browser/layout.html'
import Address from './browser/address.js'
import Cluster from './browser/cluster.js'
import Transaction from './browser/transaction.js'
import Search from './browser/search.js'
import TransactionsTable from './browser/transactions_table.js'
import AddressesTable from './browser/addresses_table.js'
import TagsTable from './browser/tags_table.js'
import TransactionAddressesTable from './browser/transaction_addresses_table.js'

const historyPushState = (msg, data) => {
  let newState = {message: msg, data: {fromHistory: true, ...data}}
  let mm = msg + '.browser'
  if (data.fromHistory) {
    history.replaceState(newState, mm)
  } else {
    history.pushState(newState, mm)
  }
}

export default class Browser {
  constructor (dispatcher, store) {
    this.store = store
    this.loading = set()
    this.dispatcher = dispatcher
    this.dispatcher.on('searchresult.browser', (result) => {
      this.searchresult(result)
    })
    this.dispatcher.on('selectNode.browser', ([type, nodeId]) => {
      if (type === 'address') {
        this.address(nodeId[0])
      } else if (type === 'cluster') {
        this.cluster(nodeId[0])
      }
      this.render()
    })
    this.dispatcher.on('resultNode.browser', (response) => {
      let isSearch = this.content[0] instanceof Search
      let isTransaction = this.content[0] instanceof Transaction
      if (!isSearch && !isTransaction && !response.fromHistory) return
      if (!response.fromHistory) {
        if (isSearch && !this.content[0].loading.has(response.result.address)) return
        if (isTransaction && !this.loading.has(response.result.address)) return
      }

      historyPushState('resultNode', response)
      this.loading.remove(response.result.address)
      let a = this.store.add(response.result)
      this.destroyComponentsFrom(0)
      this.content[0] = new Address(this.dispatcher, a, 0)
      this.render()
    })
    this.dispatcher.on('loadAddress.browser', ({address}) => {
      this.loading.add(address)
      this.dispatcher.call('loadNode', null, {id: address, type: 'address'})
    })

    this.dispatcher.on('initTransactionsTable.browser', (request) => {
      if (request.index !== 0 && !request.index) return
      let comp = this.content[request.index]
      if (!(comp instanceof Address)) return
      if (this.content[request.index + 1] instanceof TransactionsTable) return
      let total = comp.data.noIncomingTxs + comp.data.noOutgoingTxs
      this.destroyComponentsFrom(request.index + 1)
      this.content.push(new TransactionsTable(this.dispatcher, request.index + 1, total, request.id, request.type))
      this.render()
    })
    this.dispatcher.on('initAddressesTable.browser', (request) => {
      if (request.index !== 0 && !request.index) return
      let last = this.content[request.index]
      if (!(last instanceof Cluster)) return
      if (this.content[request.index + 1] instanceof AddressesTable) return
      let total = last.data.noAddresses
      this.destroyComponentsFrom(request.index + 1)
      this.content.push(new AddressesTable(this.dispatcher, request.index + 1, total, request.id))
      this.render()
    })
    this.dispatcher.on('initTagsTable.browser', (request) => {
      if (request.index !== 0 && !request.index) return
      let last = this.content[request.index]
      if (!(last instanceof Cluster) && !(last instanceof Address)) return
      if (this.content[request.index + 1] instanceof TagsTable) return
      this.destroyComponentsFrom(request.index + 1)
      this.content.push(new TagsTable(this.dispatcher, request.index + 1, request.id, request.type))
      this.render()
    })
    this.dispatcher.on('selectAddress.browser', (data) => {
      console.log('selectAdress', data)
      if (!data.address) return
      historyPushState('selectAddress', data)
      this.address(data)
      this.render()
    })
    this.dispatcher.on('resultTransaction.browser', (response) => {
      console.log('resultTransaction', response.result)
      historyPushState('resultTransaction', response)
      this.transaction(response.result)
      this.render()
    })

    window.onpopstate = (e) => {
      if (!e.state) return
      this.dispatcher.call(e.state.message, null, e.state.data)
    }

    this.root = document.createElement('div')
    this.root.className = 'h-full'
    this.search()
  }
  destroyComponentsFrom (index) {
    this.content.forEach((content, i) => {
      if (i >= index) content.destroy()
    })
    this.content = this.content.slice(0, index)
  }
  search () {
    this.activeTab = 'search'

    this.content = [ new Search(this.dispatcher, 0) ]
  }
  address (addrOrObj) {
    let address = typeof addrOrObj === 'string' ? this.store.get('address', addrOrObj) : addrOrObj
    if (!address) {
      console.error(`browser: ${addrOrObj} not found`)
      return
    }
    this.activeTab = 'address'
    this.destroyComponentsFrom(0)
    this.content = [ new Address(this.dispatcher, address, 0) ]
  }
  transaction (tx) {
    this.activeTab = 'transactions'
    this.destroyComponentsFrom(0)
    this.content = [
      new Transaction(this.dispatcher, tx, 0),
      new TransactionAddressesTable(this.dispatcher, tx.inputs, 'Inputs', 1),
      new TransactionAddressesTable(this.dispatcher, tx.outputs, 'Outputs', 1)
    ]
  }
  cluster (cluster) {
    cluster = this.store.get('cluster', cluster)
    if (!cluster) {
      console.error(`browser: ${cluster} not found`)
      return
    }
    this.activeTab = 'address'
    this.destroyComponentsFrom(0)
    this.content = [ new Cluster(this.dispatcher, cluster, 0) ]
  }
  searchresult (result) {
    if (!(this.content[0] instanceof Search)) return
    console.log('searchresult', result)
    // assume search being the first in content
    let search = this.content[0]
    search.setResult(result)
    search.render()
  }
  pickresult (result) {
    if (this.activeTab !== 'search') return
    // assume search being the first in content
    let content = [this.content[0]]
    let comp = null
    if (result.address) {
      this.store.add(result)

      comp = new Address(this.dispatcher, result)
    }
    if (comp === null) return
    content.push(comp)
    this.content = content
  }
  render () {
    this.root.innerHTML = layout
    this.root.querySelector('#browser-nav-search-button')
      .addEventListener('click', () => {
        this.search()
        this.render()
      })
    let data = this.root.querySelector('#browser-data')
    let c = 0
    this.content.forEach((comp) => {
      c += 1
      data.appendChild(comp.render())
      let options = comp.renderOptions()
      if (!options) return
      let el = document.createElement('div')
      el.className = 'browser-options ' + (c < this.content.length ? 'browser-options-short' : '')
      el.appendChild(options)
      data.appendChild(el)
    })
    return this.root
  }
}
