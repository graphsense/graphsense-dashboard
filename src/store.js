import {map} from 'd3-collection'
import Logger from './logger.js'

const logger = Logger.create('Store') // eslint-disable-line no-unused-vars

const sep = '|'

const prefix = (keyspace, id) => {
  return keyspace + sep + id
}
const unprefix = (idPrefixed) => {
  let pos = idPrefixed.indexOf(sep)
  if (pos === -1) return [null, idPrefixed]
  return [idPrefixed.substring(0, pos), idPrefixed.substring(pos + 1)]
}

export default class Store {
  constructor () {
    this.addresses = map()
    this.clusters = map()
    this.outgoingLinks = map()
  }
  /**
   * Adds an object to store if it does not exist
   */
  add (object) {
    let empty = {}
    let idPrefixed = null
    let id = null
    let type = null
    if (object.address || object.type === 'address') {
      id = object.address ? object.address : object.id
      type = 'address'
    } else if (object.cluster || object.type === 'cluster') {
      id = object.cluster ? object.cluster : object.id
      type = 'cluster'
    } else {
      logger.error('invalid object ' + object)
      return
    }
    if (object.keyspace && id) idPrefixed = prefix(object.keyspace, id)
    if (idPrefixed && type === 'address') {
      let a = this.addresses.get(idPrefixed)
      if (!a) {
        a = empty
        a.id = id
        a.type = 'address'
        let outgoing = this.initOutgoing(id, object.keyspace)
        a.outgoing = outgoing
        this.addresses.set(idPrefixed, a)
      }
      // merge new object into existing one
      Object.keys(object).forEach(key => { a[key] = object[key] })
      // remove unneeded address field (is now id)
      delete a.address
      if (typeof object.cluster === 'string' || typeof object.cluster === 'number' ) object.toCluster = object.cluster
      if (object.toCluster) {
        let cidPrefixed = prefix(object.keyspace, object.toCluster)
        let c = this.clusters.get(cidPrefixed)
        if (!c) {
          c = { addresses: map(), id: object.toCluster, type: 'cluster', ...empty }
          let outgoing = this.initOutgoing(id, object.keyspace)
          c.outgoing = outgoing
          this.clusters.set(cidPrefixed, c)
        }
        c.addresses.set(a.id, a)
        a.cluster = c
      }
      return a
    } else if (idPrefixed && type === 'cluster') {
      let c = this.clusters.get(idPrefixed)
      if (!c) {
        c = { addresses: map(), ...empty }
        c.id = id
        c.type = 'cluster'
        let outgoing = this.initOutgoing(id, object.keyspace)
        c.outgoing = outgoing
        this.clusters.set(idPrefixed, c)
      }
      // merge new object into existing one
      Object.keys(object).forEach(key => { c[key] = object[key] })
      // remove unneeded cluster field (is now id)
      delete c.cluster
      let addresses = object.forAddresses || []
      addresses.forEach(address => {
        let a = this.addresses.get(prefix(object.keyspace, address))
        logger.debug('forAddress', address, a)
        if (a) {
          c.addresses.set(address, a)
          a.cluster = c
        }
      })
      return c
    }
  }
  get (keyspace, type, key) {
    switch (type) {
      case 'address':
        return this.addresses.get(prefix(keyspace, key))
      case 'cluster':
        return this.clusters.get(prefix(keyspace, key))
    }
  }
  initOutgoing (id, keyspace) {
    if (typeof id !== 'string' && typeof id !== 'number') {
      throw new Error('id is not string')
    }
    let outgoing = this.outgoingLinks.get(prefix(keyspace, id))
    if (!outgoing) {
      outgoing = map()
      this.outgoingLinks.set(prefix(keyspace, id), outgoing)
    }
    return outgoing
  }
  linkOutgoing (source, target, keyspace, data) {
    logger.debug('linkOutgoing', source, target, keyspace, data)
    let outgoing = this.initOutgoing(source, keyspace)
    let n = outgoing.get(target)
    if (!n && (!data || !data.noTransactions || !data.estimatedValue)) {
      outgoing.set(target, null)
      return
    }
    if (!data) return
    outgoing.set(target, {
      noTransactions: data.noTransactions,
      estimatedValue: data.estimatedValue
    })
  }
  serialize () {
    let addresses = []
    this.addresses.each(address => {
      let s = {...address}
      s.cluster = s.cluster.id
      delete s.outgoing
      addresses.push(s)
    })
    let clusters = []
    this.clusters.each(cluster => {
      let s = {...cluster}
      s.addresses = s.addresses.keys()
      delete s.outgoing
      clusters.push(s)
    })
    let alllinks = []
    this.outgoingLinks.each((links, id) => {
      alllinks.push([id, links.entries()])
    })
    return [addresses, clusters, alllinks]
  }
  deserialize ([addresses, clusters, alllinks]) {
    alllinks.forEach(([id, links]) => {
      links.forEach(({key, value}) => {
        let sp = unprefix(id)
        this.linkOutgoing(sp[1], key, sp[0], value)
      })
    })
    clusters.forEach(cluster => {
      cluster.forAddresses = cluster.addresses
      delete cluster.addresses
      this.add(cluster)
    })
    addresses.forEach(address => {
      this.add(address)
    })
  }
}
