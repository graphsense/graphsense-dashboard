import {map} from 'd3-collection'
import Logger from './logger.js'

const logger = Logger.create('Store') // eslint-disable-line no-unused-vars

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
    if (object.address || (object.id && object.type === 'address')) {
      let a = this.addresses.get(object.address || object.id)
      if (!a) {
        a = empty
        a.id = object.address || object.id
        a.type = 'address'
        let outgoing = this.initOutgoing(a.id)
        a.outgoing = outgoing
        this.addresses.set(a.id, a)
      }
      // merge new object into existing one
      Object.keys(object).forEach(key => { a[key] = object[key] })
      // remove unneeded address field (is now id)
      delete a.address
      if (typeof object.cluster === 'string') object.toCluster = object.cluster
      if (object.toCluster) {
        let c = this.clusters.get(object.toCluster)
        if (!c) {
          c = { addresses: map(), id: object.toCluster, type: 'cluster', ...empty }
          this.clusters.set(object.toCluster, c)
        }
        c.addresses.set(a.id, a)
        a.cluster = c
      }
      return a
    } else if (object.cluster || (object.id && object.type === 'cluster')) {
      let c = this.clusters.get(object.cluster || object.id)
      if (!c) {
        c = { addresses: map(), ...empty }
        c.id = object.cluster || object.id
        c.type = 'cluster'
        let outgoing = this.initOutgoing(c.id)
        c.outgoing = outgoing
        this.clusters.set(c.id, c)
      }
      // merge new object into existing one
      Object.keys(object).forEach(key => { c[key] = object[key] })
      // remove unneeded cluster field (is now id)
      delete c.cluster
      let addresses = object.forAddresses || []
      addresses.forEach(address => {
        let a = this.addresses.get(address)
        logger.debug('forAddress', address, a)
        if (a) {
          c.addresses.set(address, a)
          a.cluster = c
        }
      })
      return c
    }
  }
  get (type, key) {
    switch (type) {
      case 'address':
        return this.addresses.get(key)
      case 'cluster':
        return this.clusters.get(key)
    }
  }
  initOutgoing (id) {
    logger.debug('id', id)
    if (typeof id !== 'string' && typeof id !== 'number') {
      throw new Error('id is not string')
    }
    let outgoing = this.outgoingLinks.get(id)
    if (!outgoing) {
      outgoing = map()
      this.outgoingLinks.set(id, outgoing)
    }
    return outgoing
  }
  linkOutgoing (source, target, data) {
    logger.debug('linkOutgoing', source, target, data)
    let outgoing = this.initOutgoing(source)
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
    addresses.forEach(address => {
      this.add(address)
    })
    clusters.forEach(cluster => {
      cluster.forAddresses = cluster.addresses
      delete cluster.addresses
      this.add(cluster)
    })
    alllinks.forEach(([id, links]) => {
      links.forEach(({key, value}) => {
        this.linkOutgoing(id, key, value)
      })
    })
  }
}
