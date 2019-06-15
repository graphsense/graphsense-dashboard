import {json} from 'd3-fetch'
import Logger from './logger.js'

const logger = Logger.create('Rest')

const options = {
  credentials: 'include',
  headers: {
    'Accept': 'application/json',
    'Content-Type': 'application/json',
    // never expiring token
    'Authorization': 'Bearer ' + JWT_TOKEN // eslint-disable-line no-undef
  }
}

export default class Rest {
  constructor (baseUrl, prefixLength) {
    this.baseUrl = baseUrl
    this.prefixLength = prefixLength
    this.json = this.remoteJson
  }
  remoteJson (keyspace, url, field) {
    url = this.baseUrl + (keyspace ? '/' + keyspace : '') + '/' + url
    return json(url, options)
      .catch(err => {
        logger.debug('err', err)
      })
      .then(result => {
        if (field) {
        // result is an array
          if (!Array.isArray(result[field])) {
            logger.warn(`${field} is not in result or not an array, calling ${url}`)
          } else {
            result[field].forEach(item => { item.keyspace = keyspace })
          }
        } else {
          result.keyspace = keyspace
        }
        return Promise.resolve(result)
      }, error => {
        error.keyspace = keyspace
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
  search (keyspace, str, limit) {
    if (str.length < this.prefixLength) {
      return Promise.resolve({addresses: []})
    }
    return this.json(keyspace, '/search?q=' + encodeURIComponent(str) + '&limit=' + limit)
  }
  node (keyspace, request) {
    return this.json(keyspace, `/${request.type}_with_tags/${request.id}`)
  }
  clusterForAddress (keyspace, id) {
    logger.debug('rest clusterForAddress', id)
    return this.json(keyspace, '/address/' + id + '/cluster_with_tags')
  }
  transactions (keyspace, request) {
    let url =
       '/' + request.params[1] + '/' + request.params[0] + '/transactions?' +
      (request.nextPage ? 'page=' + request.nextPage : '') +
      (request.pagesize ? '&pagesize=' + request.pagesize : '')
    return this.json(keyspace, url, request.params[1] === 'block' ? 'txs' : 'transactions')
  }
  addresses (keyspace, request) {
    let url =
       '/cluster/' + request.params + '/addresses?' +
      (request.nextPage ? 'page=' + request.nextPage : '') +
      (request.pagesize ? '&pagesize=' + request.pagesize : '')
    return this.json(keyspace, url, 'addresses')
  }
  tags (keyspace, {id, type}) {
    let url = '/' + type + '/' + id + '/tags'
    return this.json(keyspace, url, 'tags')
  }
  egonet (keyspace, type, id, isOutgoing, limit) {
    let dir = isOutgoing ? 'out' : 'in'
    return this.json(keyspace, `/${type}/${id}/egonet?limit=${limit}&direction=${dir}`, 'nodes')
  }
  clusterAddresses (keyspace, id, limit) {
    return this.json(keyspace, `/cluster/${id}/addresses?pagesize=${limit}`, 'addresses')
  }
  transaction (keyspace, txHash) {
    return this.json(keyspace, `/tx/${txHash}`)
  }
  block (keyspace, height) {
    return this.json(keyspace, `/block/${height}`)
  }
  neighbors (keyspace, id, type, isOutgoing, pagesize, nextPage) {
    let dir = isOutgoing ? 'out' : 'in'
    let url =
      `/${type}/${id}/neighbors?direction=${dir}&` +
      (nextPage ? 'page=' + nextPage : '') +
      (pagesize ? '&pagesize=' + pagesize : '')
    return this.json(keyspace, url, 'neighbors')
  }
  stats () {
    return this.json(null, '')
  }
  searchNeighbors (keyspace, id, type, isOutgoing, params, searchDepth, searchBreadth) {
    let dir = isOutgoing ? 'out' : 'in'
    let url =
      `/${type}/${id}/search/${dir}/${params.category}?depth=${searchDepth}&breadth=${searchBreadth}`
    return this.json(keyspace, url)
  }
}
