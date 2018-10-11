import {json} from 'd3-fetch'

export default class Rest {
  constructor (dispatcher, baseUrl) {
    this.dispatcher = dispatcher
    this.baseUrl = baseUrl
    this.dispatcher.on('search.rest', (term) => {
      this.search(term)
    })
    this.dispatcher.on('loadIncomingTxs.rest', (request) => {
      this.incomingTxs(request)
    })
    this.dispatcher.on('loadAddress.rest', (request) => {
      this.address(request)
    })
    this.dispatcher.on('loadClusterForAddress.rest', (request) => {
      this.clusterForAddress(request)
    })
    this.dispatcher.on('applyAddressFilters.rest', ([addressId, isOutgoing, filters]) => {
      if (!filters.has('limit')) return
      this.egonet(addressId, isOutgoing, filters.get('limit'))
    })
  }
  search (term) {
    return json(this.baseUrl + '/address/' + term).then((result) => {
      this.dispatcher.call('searchresult', null, result)
    })
  }
  address (request) {
    return json(this.baseUrl + '/address/' + request.address).then((result) => {
      this.dispatcher.call('resultAddress', null, {request, result})
    })
  }
  clusterForAddress (request) {
    return json(this.baseUrl + '/address/' + request.address + '/cluster').then((result) => {
      if (!result.cluster) {
        // seems there exist addresses without cluster ...
        // so mockup cluster with the address id
        result.cluster = request.address
        result.mockup = true
      }
      this.dispatcher.call('resultClusterForAddress', null, {request, result})
    })
  }
  incomingTxs (address) {
    return json(this.baseUrl + '/address/' + address + '/transactions').then((result) => {
      console.log(result)
    })
  }
  egonet (addressId, isOutgoing, limit) {
    let dir = isOutgoing ? 'out' : 'in'
    return json(`${this.baseUrl}/address/${addressId[0]}/egonet?limit=${limit}&direction=${dir}`).then((result) => {
      console.log(result)
      this.dispatcher.call('resultEgonet', null, {addressId, isOutgoing, result})
    })
  }
}
