import {json} from 'd3-fetch'

export default class Rest {
  constructor (dispatcher, baseUrl) {
    this.dispatcher = dispatcher
    this.baseUrl = baseUrl
    this.dispatcher.on('search.rest', (term) => {
      this.search(term)
    })
    this.dispatcher.on('loadIncomingTxs.rest', (address) => {
      this.incomingTxs(address)
    })
    this.dispatcher.on('loadAddress', (address) => {
      this.address(address)
    })
    this.dispatcher.on('loadClusterForAddress', (address) => {
      this.clusterForAddress(address)
    })
  }
  search (term) {
    return json(this.baseUrl + '/address/' + term).then((result) => {
      console.log(result)
      this.dispatcher.call('searchresult', this, result)
    })
  }
  address (address) {
    return json(this.baseUrl + '/address/' + address).then((result) => {
      console.log(result)
      this.dispatcher.call('resultAddress', this, result)
    })
  }
  clusterForAddress (address) {
    return json(this.baseUrl + '/address/' + address + '/cluster').then((result) => {
      result.forAddress = address
      console.log(result)
      this.dispatcher.call('resultClusterForAddress', this, result)
    })
  }
  incomingTxs (address) {
    return json(this.baseUrl + '/address/' + address + '/transactions').then((result) => {
      console.log(result)
    })
  }
}
