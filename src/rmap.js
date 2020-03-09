import { map } from 'd3-collection'

export default class RMap {
  constructor () {
    this.map = map()
  }

  set (k, v) {
    return this.map.set(k, v)
  }

  get (k) {
    const v = this.map.get(k)
    if (v && !v.removed) return v
  }

  getRemoved (k) {
    return this.map.get(k)
  }

  has (k) {
    return !!this.get(k)
  }

  hasRemoved (k) {
    return this.map.has(k)
  }

  values () {
    return this.map.values().filter(node => !node.removed)
  }

  valuesWithRemoved () {
    return this.map.values()
  }

  each (fun) {
    this.map.each(function (v) {
      if (v.removed) return
      fun.apply(this, arguments)
    })
  }

  size () {
    let c = 0
    this.map.each((v) => {
      if (v.removed) return
      c++
    })
    return c
  }

  delete (id) {
    return this.map.delete(id)
  }
}
