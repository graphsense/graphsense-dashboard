import {map, set} from 'd3-collection'

export default class Store {
  constructor () {
    this.addresses = map()
    this.clusters = map()
  }
  /**
   * Adds an object to store if it does not exist
   */
  add (object) {
    let empty = {outgoing: set(), incoming: set()}
    if (object.address || (object.id && object.type === 'address')) {
      let a = this.addresses.get(object.address || object.id)
      if (!a) {
        a = empty
        a.id = object.address
        a.type = 'address'
        this.addresses.set(a.id, a)
      }
      // merge new object into existing one
      Object.keys(object).forEach(key => { a[key] = object[key] })
      // remove unneeded address field (is now id)
      delete a.address
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
        c.id = object.cluster
        c.type = 'cluster'
        this.clusters.set(c.id, c)
      }
      // merge new object into existing one
      Object.keys(object).forEach(key => { c[key] = object[key] })
      // remove unneeded cluster field (is now id)
      delete c.cluster
      if (object.forAddress) {
        let a = this.addresses.get(object.forAddress)
        console.log('forAddress', object.forAddress, a)
        if (a) {
          c.addresses.set(object.forAddress, a)
          a.cluster = c
        }
      }
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
}
