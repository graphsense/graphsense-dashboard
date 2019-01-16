import {json} from 'd3-fetch'
// import {json} from './mockup.js'
import Logger from './logger.js'

const logger = Logger.create('Rest') // eslint-disable-line

const options = {} // { credentials: 'include' }

export default class Rest {
  constructor (baseUrl, keyspace, prefixLength) {
    this.keyspace = keyspace
    this.baseUrl = baseUrl + (keyspace ? '/' + keyspace : '')
    this.prefixLength = prefixLength
    this.json = this.remoteJson
  }
  remoteJson (url, field) {
    return json(url, options).then(result => {
      if (field) {
        // result is an array
        if (!result[field] || !result[field].length) {
          logger.warn(`${field} is not in result, calling ${url}`)
        } else {
          result[field].forEach(item => item.keyspace = this.keyspace)
        }
      } else {
        result.keyspace = this.keyspace
      }
      return Promise.resolve(result)
    }, error => {
      error.keyspace = this.keyspace
      error.requestURL = url
      return Promise.reject(error)
    })
  }
  disable () {
    this.json = (url) => {
      return Promise.resolve()
    }
  }
  enable () {
    this.json = this.remoteJson
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
    logger.debug('rest clusterForAddress', id)
    return this.json(this.baseUrl + '/address/' + id + '/cluster_with_tags')
  }
  transactions (request) {
    let url =
      this.baseUrl + '/' + request.params[1] + '/' + request.params[0] + '/transactions?' +
      (request.nextPage ? 'page=' + request.nextPage : '') +
      (request.pagesize ? '&pagesize=' + request.pagesize : '')
    return this.json(url, 'transactions')
  }
  addresses (request) {
    let url =
      this.baseUrl + '/cluster/' + request.params + '/addresses?' +
      (request.nextPage ? 'page=' + request.nextPage : '') +
      (request.pagesize ? '&pagesize=' + request.pagesize : '')
    return this.json(url, 'addresses')
  }
  tags ({id, type}) {
    let url = this.baseUrl + '/' + type + '/' + id + '/tags'
    return this.json(url, 'tags')
  }
  egonet (type, id, isOutgoing, limit) {
    let dir = isOutgoing ? 'out' : 'in'
    return this.json(`${this.baseUrl}/${type}/${id}/egonet?limit=${limit}&direction=${dir}`, 'nodes')
  }
  clusterAddresses (id, limit) {
    return this.json(`${this.baseUrl}/cluster/${id}/addresses?pagesize=${limit}`, 'addresses')
  }
  transaction (txHash) {
    return this.json(`${this.baseUrl}/tx/${txHash}`)
  }
  neighbors (id, type, isOutgoing, pagesize, nextPage) {
    let dir = isOutgoing ? 'out' : 'in'
    let url =
      `${this.baseUrl}/${type}/${id}/neighbors?direction=${dir}&` +
      (nextPage ? 'page=' + nextPage : '') +
      (pagesize ? '&pagesize=' + pagesize : '')
    return this.json(url, 'neighbors')
  }
  stats () {
    return this.json(`${this.baseUrl}`)
  }
}
