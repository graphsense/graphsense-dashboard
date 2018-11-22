import {json} from 'd3-fetch'

export default class Rest {
  constructor (baseUrl, prefixLength) {
    this.baseUrl = baseUrl
    this.prefixLength = prefixLength
    this.json = json
  }
  disable () {
    this.json = (url) => {
      return Promise.resolve()
    }
  }
  enable () {
    this.json = json
  }
  search (str, limit) {
    if (str.length < this.prefixLength) {
      return Promise.resolve({addresses: []})
    }
    return this.json(this.baseUrl + '/search?q=' + encodeURIComponent(str) + '&limit=' + limit)
  }
  node (request) {
    return this.json(`${this.baseUrl}/${request.type}_with_tags/${request.id}`)
  }
  clusterForAddress (id) {
    console.log('rest clusterForAddress', id)
    return this.json(this.baseUrl + '/address/' + id + '/cluster_with_tags')
  }
  transactions (request) {
    let url =
      this.baseUrl + '/' + request.params[1] + '/' + request.params[0] + '/transactions?' +
      (request.nextPage ? 'page=' + request.nextPage : '') +
      (request.pagesize ? '&pagesize=' + request.pagesize : '')
    return this.json(url)
  }
  addresses (request) {
    let url =
      this.baseUrl + '/cluster/' + request.params + '/addresses?' +
      (request.nextPage ? 'page=' + request.nextPage : '') +
      (request.pagesize ? '&pagesize=' + request.pagesize : '')
    return this.json(url)
  }
  tags ({id, type}) {
    let url = this.baseUrl + '/' + type + '/' + id + '/tags'
    return this.json(url)
  }
  egonet (type, id, isOutgoing, limit) {
    let dir = isOutgoing ? 'out' : 'in'
    return this.json(`${this.baseUrl}/${type}/${id[0]}/egonet?limit=${limit}&direction=${dir}`)
  }
  clusterAddresses (id, limit) {
    return this.json(`${this.baseUrl}/cluster/${id}/addresses?pagesize=${limit}`)
  }
  transaction (txHash) {
    return this.json(`${this.baseUrl}/tx/${txHash}`)
  }
  neighbors (id, type, isOutgoing) {
    let dir = isOutgoing ? 'out' : 'in'
    return this.json(`${this.baseUrl}/${type}/${id}/neighbors?direction=${dir}`)
  }
}
