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
    if (object.address) {
      if (this.addresses.has(object.address)) return
      this.addresses.set(object.address, object)
      console.log(this.addresses)
      return object
    }
    if (object.cluster) {
      let c = this.clusters.get(object.cluster) || { addresses: set() }
      if (object.forAddress) {
        c.addresses.add(object.forAddress)
        object = {...object, ...c}
        delete object.forAddress
        if (this.addresses.has(object.forAddress)) {
          let a = this.addresses.get(object.forAddress)
          a.cluster = object.cluster
          this.addresses.set(a.address, a)
        }
      }
      this.clusters.set(object.cluster, object)
      return object
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
