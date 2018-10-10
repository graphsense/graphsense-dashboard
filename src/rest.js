import {json} from 'd3-fetch'

export default class Rest {
  constructor (dispatcher, baseUrl) {
    this.dispatcher = dispatcher
    this.baseUrl = baseUrl
    this.dispatcher.on('search.rest', (term) => {
      this.address(term)
    })
    this.dispatcher.on('loadIncomingTxs.rest', (address) => {
      this.incomingTxs(address)
    })
  }
  address (address) {
    return json(this.baseUrl + '/address/' + address).then((result) => {
      console.log(result)
      this.dispatcher.call('searchresult', this, result)
    })
  }
  incomingTxs (address) {
    return json(this.baseUrl + '/address/' + address + '/transactions').then((result) => {
      console.log(result)
    })
  }
}
