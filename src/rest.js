import {json} from 'd3-fetch'
import {firstToUpper} from './util.js'

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
    this.dispatcher.on('loadNode.rest', (request) => {
      this.node(request)
    })
    this.dispatcher.on('loadClusterForAddress.rest', (request) => {
      this.clusterForAddress(request)
    })
    this.dispatcher.on('applyTxFilters.rest', ([id, isOutgoing, type, filters]) => {
      if (!filters.has('limit')) return
      this.egonet(type, id, isOutgoing, filters.get('limit'))
    })
    this.dispatcher.on('applyAddressFilters.rest', ([id, filters]) => {
      if (!filters.has('limit')) return
      this.clusterAddresses(id, filters.get('limit'))
    })
  }
  search (term) {
    return json(this.baseUrl + '/address/' + term).then((result) => {
      this.dispatcher.call('searchresult', null, result)
    })
  }
  node (request) {
    return json(`${this.baseUrl}/${request.type}/${request.id}`).then((result) => {
      this.dispatcher.call('resultNode', null, {request, result})
    })
  }
  clusterForAddress (request) {
    return json(this.baseUrl + '/address/' + request.id + '/cluster').then((result) => {
      if (!result.cluster) {
        // seems there exist addresses without cluster ...
        // so mockup cluster with the address id
        result.cluster = request.id
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
  egonet (type, id, isOutgoing, limit) {
    let dir = isOutgoing ? 'out' : 'in'
    return json(`${this.baseUrl}/${type}/${id[0]}/egonet?limit=${limit}&direction=${dir}`).then((result) => {
      console.log(result)
      this.dispatcher.call('resultEgonet', null, {type, id, isOutgoing, result})
    })
  }
}
