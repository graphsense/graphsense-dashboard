import {json} from 'd3-fetch'
import {firstToUpper} from './util.js'

export default class Rest {
  constructor (dispatcher, baseUrl) {
    this.dispatcher = dispatcher
    this.baseUrl = baseUrl
    this.dispatcher.on('search.rest', ([str, limit]) => {
      this.search(str, limit)
    })
    this.dispatcher.on('loadTransactions.rest', (request) => {
      this.transactions(request)
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
  search (str, limit) {
    return json(this.baseUrl + '/search?q=' + encodeURIComponent(str) + '&limit=' + limit).then((result) => {
      this.dispatcher.call('searchresult', null, [result, str])
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
  transactions (request) {
    let url =
      this.baseUrl + '/' + request.params[1] + '/' + request.params[0] + '/transactions?' +
      (request.nextPage ? 'page=' + request.nextPage : '') +
      (request.pagesize ? '&pagesize=' + request.pagesize : '')
    return json(url).then((result) => {
      this.dispatcher.call('resultTransactions', null, {page: request.nextPage, result})
    })
  }
  egonet (type, id, isOutgoing, limit) {
    let dir = isOutgoing ? 'out' : 'in'
    return json(`${this.baseUrl}/${type}/${id[0]}/egonet?limit=${limit}&direction=${dir}`).then((result) => {
      console.log(result)
      this.dispatcher.call('resultEgonet', null, {type, id, isOutgoing, result})
    })
  }
  clusterAddresses (id, limit) {
    return json(`${this.baseUrl}/cluster/${id[0]}/addresses?limit=${limit}`).then((result) => {
      console.log(result)
      this.dispatcher.call('resultClusterAddresses', null, {id, result})
    })
  }
}
