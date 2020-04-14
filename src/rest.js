import { json } from 'd3-fetch'
import Logger from './logger.js'

const logger = Logger.create('Rest')

const options = () => ({
  credentials: 'include',
  headers: {
    Accept: 'application/json',
    'Content-Type': 'application/json'
  }
})

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

const normalizeNode = node => {
  node.nodeType = node.node_type
  delete node.node_type
  return node
}

const typeToEndpoint = type => {
  if (type === 'address') return 'addresses'
  if (type === 'entity') return 'entities'
  if (type === 'label') return 'labels'
  if (type === 'block') return 'blocks'
  return type
}

export default class Rest {
  constructor (baseUrl, prefixLength) {
    this.baseUrl = baseUrl
    this.prefixLength = prefixLength
    this.json = this.remoteJson
    this.logs = []
  }

  refreshToken () {
    logger.debug('refreshToken')
    if (this.refreshing) return Promise.reject(new Error('refresh in progress'))
    logger.debug('refreshing Token')
    this.refreshing = true
    return json(this.baseUrl + '/refresh', options())
      .then(result => {
        if (result.status !== 'success') return Promise.reject(new Error('refreshed is false'))
        logger.debug('refresh successful')
        return result
      })
      .finally(() => { logger.debug('refresh cleanup'); this.refreshing = false })
  }

  remoteJson (keyspace, url, field, abortController) {
    const newurl = this.keyspaceUrl(keyspace) + (url.startsWith('/') ? '' : '/') + url
    const opts = options()
    if (abortController) {
      opts.signal = abortController.signal
    }
    return json(newurl, opts)
      .then(result => {
        this.logs.push([+new Date(), newurl])
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
        error.requestURL = newurl
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
    return url
  }

  keyspaceUrl (keyspace) {
    return this.baseUrl + (keyspace ? '/' + keyspace : '')
  }

  disable () {
    this.json = (keyspace, url, field) => {
      logger.debug('DISABLED calling ', keyspace, url, field)
      return Promise.resolve()
    }
  }

  enable () {
    this.json = this.remoteJson
  }

  search (str, limit) {
    logger.debug('calling search')
    const ac = new window.AbortController()
    return [ac,
      this.json(null, '/search/' + encodeURIComponent(str) + (limit ? `?limit=${limit}` : ''), null, ac)]
  }

  node (keyspace, { type, id }) {
    type = typeToEndpoint(type)

    return this.json(keyspace, `/${type}/${id}`)
      .then(normalizeNode)
      .then(normalizeNodeTags(keyspace))
  }

  entityForAddress (keyspace, id) {
    logger.debug('rest entityForAddress', id)
    return this.json(keyspace, '/addresses/' + id + '/entity')
      .then(normalizeNode)
      .then(normalizeNodeTags(keyspace))
  }

  transactions (keyspace, request, csv) {
    const type = typeToEndpoint(request.params[1])
    let url =
       '/' + type + '/' + request.params[0] + '/txs'
    if (csv) return this.csv(keyspace, url)
    url += '?' +
      (request.nextPage ? 'page=' + request.nextPage : '') +
      (request.pagesize ? '&pagesize=' + request.pagesize : '')
    return this.json(keyspace, url, request.params[1] === 'block' ? 'txs' : 'address_txs')
  }

  linkTransactions (keyspace, params, csv) {
    const url =
       '/addresses/' + params.source + '/links?neighbor=' + params.target
    if (csv) return this.csv(keyspace, url)
    return this.json(keyspace, url, 'links')
  }

  addresses (keyspace, request, csv) {
    let url = '/entities/' + request.params + '/addresses'
    if (csv) return this.csv(keyspace, url)
    url += '?' +
      (request.nextPage ? 'page=' + request.nextPage : '') +
      (request.pagesize ? '&pagesize=' + request.pagesize : '')
    return this.json(keyspace, url, 'addresses')
      .then(result => { result.addresses.forEach(normalizeNode); return result })
  }

  tags (keyspace, { id, type }, csv) {
    type = typeToEndpoint(type)
    logger.debug('fetch tags', keyspace)
    const url = '/' + type + '/' + id + '/tags'
    if (csv) return this.csv(keyspace, url)
    return this.json(keyspace, url).then(tags => tags.map(tag => normalizeTag(tag.currency.toLowerCase())(tag)))
  }

  entityAddresses (keyspace, id, limit) {
    return this.json(keyspace, `/entities/${id}/addresses?pagesize=${limit}`, 'addresses')
      .then(result => { result.addresses.forEach(normalizeNode); return result })
  }

  transaction (keyspace, txHash) {
    return this.json(keyspace, `/txs/${txHash}`)
  }

  block (keyspace, height) {
    return this.json(keyspace, `/blocks/${height}`)
  }

  label (id) {
    return this.json(null, `/labels/${id}`)
  }

  neighbors (keyspace, id, type, isOutgoing, pagesize, nextPage, csv) {
    const dir = isOutgoing ? 'out' : 'in'
    type = typeToEndpoint(type)
    let url = `/${type}/${id}/neighbors?direction=${dir}`
    if (csv) return this.csv(keyspace, url)
    url += '&' +
      (nextPage ? 'page=' + nextPage : '') +
      (pagesize ? '&pagesize=' + pagesize : '')
    return this.json(keyspace, url, 'neighbors')
      .then(result => { result.neighbors.forEach(normalizeNode); return result })
  }

  stats () {
    return this.json(null, '/stats')
  }

  searchNeighbors ({ id, type, isOutgoing, depth, breadth, skipNumAddresses, params }) {
    type = typeToEndpoint(type)
    const keyspace = id[2]
    id = id[0]
    const dir = isOutgoing ? 'out' : 'in'
    let searchCrit = ''
    if (params.category) {
      searchCrit = `category=${params.category}`
    } else if (params.addresses) {
      searchCrit = 'addresses=' + params.addresses.join(',')
    }
    const url =
      `/entities/${id}/search?direction=${dir}&${searchCrit}&depth=${depth}&breadth=${breadth}&skipNumAddresses=${skipNumAddresses}`
    const addKeyspace = (node) => {
      if (!node.paths) { return node }
      (node.paths || []).forEach(path => {
        path.node.keyspace = keyspace
        path.node.nodeType = 'entity';
        (path.node.tags || []).forEach(tag => {
          tag.keyspace = keyspace
          tag.currency = keyspace.toUpperCase()
        })
        path.matching_addresses.forEach(address => {
          address.keyspace = keyspace
          address.nodeType = 'address'
        })
        addKeyspace(path)
      })
      return node
    }
    return this.json(keyspace, url).then(addKeyspace)
  }

  login (username, password) {
    this.username = username
    const opt = options()
    opt.method = 'post'
    opt.body = JSON.stringify({ username, password }) // eslint-disable-line no-undef

    // using d3 json directly to pass options
    return json(this.baseUrl + '/login', opt)
      .catch(error => {
        // normalize message
        if (!error.message && error.msg) error.message = error.msg
        return Promise.reject(error)
      })
  }

  getLogs () {
    return this.logs
  }

  categories () {
    return this.json(null, '/labels/categories')
  }

  abuses () {
    return this.json(null, '/labels/abuses')
  }
}
