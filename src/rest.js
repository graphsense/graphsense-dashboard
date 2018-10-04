import {json} from 'd3-fetch'

export default class Rest {
  constructor (dispatcher, baseUrl) {
    this.dispatcher = dispatcher
    this.baseUrl = baseUrl
    this.dispatcher.on('search.rest', (term) => {
      this.address(term)
    })
  }
  address (address) {
    return json(this.baseUrl + '/address/' + address).then((result) => {
      console.log(result)
      this.dispatcher.call('searchresult', this, result)
    })
  }
}
