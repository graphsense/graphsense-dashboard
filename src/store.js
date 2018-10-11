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
      let a = this.addresses.get(object.address) || empty
      this.addresses.set(object.address, {...a, ...object})
      return object
    }
    if (object.cluster) {
      let c = this.clusters.get(object.cluster) || { addresses: set(), ...empty }
      object = {...object, ...c}
      if (object.forAddress) {
        object.addresses.add(object.forAddress)
        if (this.addresses.has(object.forAddress)) {
          let a = this.addresses.get(object.forAddress)
          a.cluster = object.cluster
          this.addresses.set(a.address, a)
        }
      }
      delete object.forAddress
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
