import {json} from 'd3-fetch'
import Logger from './logger.js'

const logger = Logger.create('Rest')

const options = () => ({
  credentials: 'include',
  headers: {
    'Accept': 'application/json',
    'Content-Type': 'application/json'
  }
})

const translateClusterToEntity = msg => node => {
  switch (msg) {
    case 'entityForAddress':
    case 'cluster':
      node.entity = node.cluster
      delete node.cluster
      break
    case 'address':
      break
    default:
      console.warn('no translation from cluster to entity applied for msg ' + msg)
  }
  return node
}

const normalizeTag = keyspace => tag => {
  tag.currency = keyspace.toUpperCase()
  tag.keyspace = keyspace
  return tag
}

const normalizeNodeTags = keyspace => node => {
  if (!node.tags || !Array.isArray(node.tags)) return node
  node.tags.forEach(normalizeTag(keyspace))
  return node
}

export default class Rest {
  constructor (baseUrl, prefixLength) {
    this.baseUrl = baseUrl
    this.prefixLength = prefixLength
    this.json = this.remoteJson
  }
  refreshToken () {
    logger.debug('refreshToken')
    if (this.refreshing) return Promise.reject(new Error('refresh in progress'))
    logger.debug('refreshing Token')
    this.refreshing = true
    return json(this.baseUrl + '/token_refresh', options())
      .then(result => {
        if (!result.refreshed) return Promise.reject(new Error('refreshed is false'))
        logger.debug('refresh successful')
        return result
      })
      .finally(() => { logger.debug('refresh cleanup'); this.refreshing = false })
  }
  remoteJson (keyspace, url, field) {
    let newurl = this.keyspaceUrl(keyspace) + (url.startsWith('/') ? '' : '/') + url
    return json(newurl, options())
      .then(result => {
        if (field) {
        // result is an array
          if (!Array.isArray(result[field])) {
            logger.warn(`${field} is not in result or not an array, calling ${url}`)
          } else {
            result[field].forEach(item => { item.keyspace = keyspace })
          }
        } else if (!Array.isArray(result)) {
          result.keyspace = keyspace
        }
        return Promise.resolve(result)
      }, error => {
        if (error.message && error.message.startsWith('401')) {
          return this.refreshToken()
            .then(() => this.remoteJson(keyspace, url, field))
        }
        error.keyspace = keyspace
        error.requestURL = url
        // normalize message
        if (!error.message && error.msg) error.message = error.msg
        return Promise.reject(error)
      })
  }
  csv (keyspace, url) {
    url = this.keyspaceUrl(keyspace) + (url.startsWith('/') ? '' : '/') + url
    if (url.indexOf('?') !== -1) {
      url = url.replace('?', '.csv?')
    } else {
      url += '.csv'
    }
    return fetch(url, options()) // eslint-disable-line no-undef
      .then(resp => resp.blob())
  }
  keyspaceUrl (keyspace) {
    return this.baseUrl + (keyspace ? '/' + keyspace : '')
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
    return this.json(keyspace, '/search?q=' + encodeURIComponent(str) + '&limit=' + limit)
  }
  searchLabels (str, limit) {
    return this.json(null, '/labelsearch?q=' + encodeURIComponent(str) + '&limit=' + limit)
  }
  node (keyspace, {type, id}) {
    if (type === 'entity') type = 'cluster'
    return this.json(keyspace, `/${type}_with_tags/${id}`)
      .then(translateClusterToEntity(type))
      .then(normalizeNodeTags(keyspace))
  }
  entityForAddress (keyspace, id) {
    logger.debug('rest entityForAddress', id)
    return this.json(keyspace, '/address/' + id + '/cluster_with_tags')
      .then(translateClusterToEntity('entityForAddress'))
      .then(normalizeNodeTags(keyspace))
  }
  transactions (keyspace, request, csv) {
    let url =
       '/' + request.params[1] + '/' + request.params[0] + '/transactions'
    if (csv) return this.csv(keyspace, url)
    url += '?' +
      (request.nextPage ? 'page=' + request.nextPage : '') +
      (request.pagesize ? '&pagesize=' + request.pagesize : '')
    return this.json(keyspace, url, request.params[1] === 'block' ? 'txs' : 'transactions')
  }
  addresses (keyspace, request, csv) {
    let url = '/cluster/' + request.params + '/addresses'
    if (csv) return this.csv(keyspace, url)
    url += '?' +
      (request.nextPage ? 'page=' + request.nextPage : '') +
      (request.pagesize ? '&pagesize=' + request.pagesize : '')
    return this.json(keyspace, url, 'addresses')
  }
  tags (keyspace, {id, type}, csv) {
    logger.debug('fetch tags', keyspace)
    if (type === 'entity') type = 'cluster'
    let url = '/' + type + '/' + id + '/tags'
    if (csv) return this.csv(keyspace, url)
    return this.json(keyspace, url).then(tags => tags.map(tag => normalizeTag(tag.currency.toLowerCase())(tag)))
  }
  egonet (keyspace, type, id, isOutgoing, limit) {
    let dir = isOutgoing ? 'out' : 'in'
    if (type === 'entity') type = 'cluster'
    return this.json(keyspace, `/${type}/${id}/egonet?limit=${limit}&direction=${dir}`, 'nodes')
  }
  entityAddresses (keyspace, id, limit) {
    return this.json(keyspace, `/cluster/${id}/addresses?pagesize=${limit}`, 'addresses')
  }
  transaction (keyspace, txHash) {
    return this.json(keyspace, `/tx/${txHash}`)
  }
  block (keyspace, height) {
    return this.json(keyspace, `/block/${height}`)
  }
  label (id) {
    return this.json(null, `/label/${id}`)
  }
  neighbors (keyspace, id, type, isOutgoing, pagesize, nextPage, csv) {
    let dir = isOutgoing ? 'out' : 'in'
    if (type === 'entity') type = 'cluster'
    let url = `/${type}/${id}/neighbors?direction=${dir}`
    if (csv) return this.csv(keyspace, url)
    url += '&' +
      (nextPage ? 'page=' + nextPage : '') +
      (pagesize ? '&pagesize=' + pagesize : '')
    return this.json(keyspace, url, 'neighbors')
  }
  stats () {
    return json(this.baseUrl + '/stats')
  }
  searchNeighbors ({id, type, isOutgoing, depth, breadth, skipNumAddresses, params}) {
    let dir = isOutgoing ? 'out' : 'in'
    if (type === 'entity') type = 'cluster'
    let keyspace = id[2]
    id = id[0]
    let searchCrit = ''
    if (params.category) {
      searchCrit = `category=${params.category}`
    } else if (params.addresses) {
      searchCrit = 'addresses=' + params.addresses.join(',')
    }
    let url =
      `/${type}/${id}/search?direction=${dir}&${searchCrit}&depth=${depth}&breadth=${breadth}&skipNumAddresses=${skipNumAddresses}`
    let addKeyspace = (node) => {
      if (!node.paths) { return node }
      (node.paths || []).forEach(path => {
        path.node.keyspace = keyspace
        path.matchingAddresses.forEach(address => { address.keyspace = keyspace })
        addKeyspace(path)
      })
      return node
    }
    return this.json(keyspace, url).then(addKeyspace)
  }
  login (username, password) {
    this.username = username
    let opt = options()
    opt.headers['Authorization'] = 'Basic ' + btoa(username + ':' + password) // eslint-disable-line no-undef
    // using d3 json directly to pass options
    return json(this.baseUrl + '/login', opt)
      .catch(error => {
        // normalize message
        if (!error.message && error.msg) error.message = error.msg
        return Promise.reject(error)
      })
  }
}
