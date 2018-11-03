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
    if (object.address) {
      let a = this.addresses.get(object.address)
      if (!a) {
        a = empty
        this.addresses.set(object.address, a)
      }
      // merge new object into existing one
      Object.keys(object).forEach(key => { a[key] = object[key] })
      if (object.cluster) {
        let c = this.clusters.get(object.cluster)
        if (c) {
          c.addresses.add(object.address)
        }
      }
      return a
    } else if (object.cluster) {
      let c = this.clusters.get(object.cluster)
      if (!c) {
        c = { addresses: set(), ...empty }
        this.clusters.set(object.cluster, c)
      }
      // merge new object into existing one
      Object.keys(object).forEach(key => { c[key] = object[key] })
      if (object.forAddress) {
        c.addresses.add(object.forAddress)
        if (this.addresses.has(object.forAddress)) {
          let a = this.addresses.get(object.forAddress)
          a.cluster = object.cluster
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
