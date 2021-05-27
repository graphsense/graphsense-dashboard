import { map } from 'd3-collection'
import Logger from './logger.js'
import moment from 'moment'
import Tag from './tag.js'

const logger = Logger.create('Store') // eslint-disable-line no-unused-vars

const sep = '|'

const prefix = (keyspace, id) => {
  return keyspace + sep + id
}
const unprefix = (idPrefixed) => {
  const pos = idPrefixed.indexOf(sep)
  if (pos === -1) return [null, idPrefixed]
  return [idPrefixed.substring(0, pos), idPrefixed.substring(pos + 1)]
}

export default class Store {
  constructor () {
    this.addresses = map()
    this.entities = map()
    this.outgoingLinks = map()
    this.tagsStore = map()
    this.userDefinedLabels = map()
    this.categories = []
  }

  /**
   * Adds an object to store if it does not exist
   */
  add (object) {
    const empty = {}
    let idPrefixed = null
    let id = null
    let type = null
    if (object.address || object.type === 'address') {
      id = object.address ? object.address : object.id
      type = 'address'
    } else if (object.entity || object.type === 'entity') {
      id = object.entity ? object.entity : object.id
      type = 'entity'
    } else {
      logger.error('invalid object, cannot determine type', object)
      return
    }
    if (!object.keyspace) {
      logger.error('invalid object, no keyspace/currency', object)
      return
    }
    idPrefixed = prefix(object.keyspace, id)
    if (idPrefixed && type === 'address') {
      let a = this.addresses.get(idPrefixed)
      if (!a) {
        a = empty
        a.id = id
        a.type = 'address'
        const outgoing = this.initOutgoing(id, object.keyspace)
        a.outgoing = outgoing
        this.addresses.set(idPrefixed, a)
      }
      // merge new object into existing one
      Object.keys(object).forEach(key => { a[key] = object[key] })

      // add existing tags eventually
      a.tags = (a.tags || []).concat(this.tagsStore.get(idPrefixed) || [])
      this.tagsStore.remove(idPrefixed)
      this.addTags(a.keyspace, a.id, a.tags.filter(t => t.isUserDefined))

      // remove unneeded address field (is now id)
      delete a.address
      if (typeof object.entity === 'string' || typeof object.entity === 'number') object.toEntity = object.entity
      if (object.toEntity) {
        const cidPrefixed = prefix(object.keyspace, object.toEntity)
        let c = this.entities.get(cidPrefixed)
        if (!c) {
          c = { addresses: map(), id: object.toEntity, type: 'entity', ...empty }
          const outgoing = this.initOutgoing(id, object.keyspace)
          c.outgoing = outgoing
          this.entities.set(cidPrefixed, c)
        }
        c.addresses.set(a.id, a)
        a.entity = c
      }
      this.calcMainCategory(a)
      return a
    } else if (idPrefixed && type === 'entity') {
      let c = this.entities.get(idPrefixed)
      let tags = null
      if (!c) {
        c = { addresses: map(), ...empty }
        c.id = id
        c.type = 'entity'
        const outgoing = this.initOutgoing(id, object.keyspace)
        c.outgoing = outgoing
        this.entities.set(idPrefixed, c)
      } else {
        tags = { ...c.tags }
      }
      // merge new object into existing one (save tags)
      Object.keys(object).forEach(key => { c[key] = object[key] })
      if (tags) c.tags = tags
      this.addTags(c.keyspace, c.id, c.tags.entity_tags.filter(t => t.isUserDefined))

      // remove unneeded entity field (is now id)
      delete c.entity
      const addresses = object.forAddresses || []
      addresses.forEach(address => {
        const a = this.addresses.get(prefix(object.keyspace, address))
        if (a) {
          c.addresses.set(address, a)
          a.entity = c
        }
      })
      this.calcMainCategory(c)
      return c
    }
  }

  calcMainCategory (node) {
    const cats = {}
    this.categories.forEach((category, index) => {
      // add 1 to avoid 0
      cats[category] = index + 1
    })

    let tags = []
    if (node.type === 'entity') {
      const t = (node.tags || {})
      tags = t.entity_tags || t.address_tags
    } else {
      tags = node.tags
    }

    const sorted = tags
      .map(tag => tag.category)
      // filter nulls and duplicates
      .filter((value, index, self) => value && self.indexOf(value) === index)
      .sort((a, b) => (cats[a] || Infinity) - (cats[b] || Infinity))
    node.mainCategory = sorted[0]
  }

  get (keyspace, type, key) {
    let store = null
    if (type === 'address') {
      store = this.addresses
    } else if (type === 'entity') {
      store = this.entities
    }
    if (!store) {
      logger.error('unknown type ' + type)
      return
    }
    return store.get(prefix(keyspace, key))
  }

  find (key, type) {
    let found = null
    const findIt = node => {
      if (!found && node.id == key) found = node // eslint-disable-line eqeqeq
    }
    if (type === 'address') {
      this.addresses.each(findIt)
    } else if (type === 'entity') {
      this.entities.each(findIt)
    } else {
      this.addresses.each(findIt)
      if (!found) this.entities.each(findIt)
    }
    return found
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

  linkOutgoing (source, target, keyspace, keyspaceTarget, data) {
    if (keyspace !== keyspaceTarget) return
    const outgoing = this.initOutgoing(source, keyspace)
    const n = outgoing.get(target)
    if (!n && !data) {
      outgoing.set(target, null)
      return
    }
    if (!data) return
    outgoing.set(target, { ...data })
  }

  getOutgoing (keyspace, type, source, target) {
    const s = this.get(keyspace, type, source)
    if (!s) return
    return s.outgoing.get(target)
  }

  serialize () {
    const addresses = []
    this.addresses.each(address => {
      const s = { ...address }
      s.entity = s.entity.id
      delete s.outgoing
      addresses.push(s)
    })
    const entities = []
    this.entities.each(entity => {
      const s = { ...entity }
      s.addresses = s.addresses.keys()
      delete s.outgoing
      entities.push(s)
    })
    const alllinks = []
    this.outgoingLinks.each((links, id) => {
      alllinks.push([id, links.entries()])
    })
    return [addresses, entities, alllinks, this.categories]
  }

  getUserDefinedTags () {
    const tags = []
    this.addresses.each(address => {
      address.tags
        .filter(tag => tag.isUserDefined)
        .map(tag => ({ ...tag }))
        .forEach(tag => {
          delete tag.isUserDefined
          delete tag.keyspace
          tags.push(tag)
        })
    })
    return tags
  }

  addTagpack (keyspaces, data) {
    const overwritable = ['address', 'label', 'source', 'currency', 'source', 'category', 'lastmod', 'abuse']
    const addressTags = map()
    data.tags.forEach(tag => {
      overwritable.forEach(key => {
        if (!tag[key]) tag[key] = data[key] || tag[key]
      })
      tag.lastmod = moment(tag.lastmod).unix()
      let tags = [tag]
      if (!tag.currency) {
        // if no currency given, assume all available keyspaces
        tags = keyspaces.map(keyspace => ({
          ...tag,
          currency: keyspace.toUpperCase()
        }))
      }
      tags.forEach(tag => {
        for (const key in tag) {
          if (tag[key] === undefined) tag[key] = null
        }
        tag.active = true
        tag.keyspace = tag.currency.toLowerCase()
        const p = prefix(tag.keyspace, tag.address)
        const t = addressTags.get(p) || []
        t.push(tag)
        addressTags.set(p, t)
      })
    })
    addressTags.each((tags, p) => {
      const a = this.addresses.get(p)
      if (a) {
        a.tags = a.tags || []
        a.tags = a.tags.concat(tags)
        this.calcMainCategory(a)
      } else {
        let t = this.tagsStore.get(p) || []
        t = t.concat(tags)
        this.tagsStore.set(p, t)
      }
    })
  }

  deserialize (version, [addresses, entities, alllinks, categories]) {
    entities.forEach(entity => {
      entity.forAddresses = entity.addresses
      delete entity.addresses
      if (version <= '0.4.5') {
        entity.tags = {
          address_tags: entity.tags,
          entity_tags: []
        }
        this.add(entity)
        if (entity.notes) {
          const tag = {}
          tag[entity.notes] = new Tag(entity.keyspace, entity.notes, entity.id, 'entity').data
          this.addTags(entity.keyspace, entity.id, tag)
        }
      } else {
        this.add(entity)
      }
    })
    addresses.forEach(address => {
      this.add(address)
    })
    alllinks.forEach(([id, links]) => {
      let sp = []
      if (version === '0.4.0') {
        const found = this.find(id)
        if (!found) return
        sp[0] = found.keyspace
        sp[1] = id
      } else {
        sp = unprefix(id)
      }
      links.forEach(({ key, value }) => {
        this.linkOutgoing(sp[1], key, sp[0], sp[0], value)
      })
    })
    this.categories = categories || []
  }

  allAddressTags () {
    let tags = []
    this.addresses.each((address) => {
      if (!address.tags) return
      tags = tags.concat(address.tags)
    })
    return tags
  }

  setCategories (cats) {
    cats = cats.map(c => typeof c === 'string' ? c : c.label)
    this.categories = cats
    this.addresses.each((a) => this.calcMainCategory(a))
    this.entities.each((a) => this.calcMainCategory(a))
  }

  addCategories (cats) {
    cats.forEach(cat => {
      if (this.categories.indexOf(cat) === -1) this.categories.push(cat)
    })
  }

  getCategories () {
    return this.categories
  }

  addTags (keyspace, id, labels) {
    let o = this.get(keyspace, 'address', id)
    if (!o) {
      o = this.get(keyspace, 'entity', id)
      if (!o) {
        console.error(`${keyspace} address/entity ${id} not found for tagging`)
        return
      }
    }

    const tagsWithoutUserDefined = []
    const userDefinedTags = []
    let tags = o.type === 'entity' ? o.tags.entity_tags : o.tags
    tags.forEach(tag => {
      if (tag.isUserDefined) {
        if (labels[tag.label]) {
          for (const prop in labels[tag.label]) {
            tag[prop] = labels[tag.label][prop]
          }
          userDefinedTags.push(tag)
          delete labels[tag.label]
        }
      } else {
        tagsWithoutUserDefined.push(tag)
      }
    })
    const newTags = []
    for (const l in labels) {
      const label = labels[l]
      const newTag = new Tag(keyspace, label.label, o.id, o.type)
      newTags.push(newTag.data)
    }
    newTags.forEach(tag => {
      let tags = this.userDefinedLabels.get(tag.label)
      if (!tags) {
        tags = [tag]
        this.userDefinedLabels.set(tag.label, tags)
      } else {
        tags.push(tag)
      }
    })
    tags = [...tagsWithoutUserDefined, ...userDefinedTags, ...newTags]
    if (o.type === 'entity') {
      o.tags.entity_tags = tags
    } else {
      o.tags = tags
    }

    this.calcMainCategory(o)
  }

  searchUserDefinedLabels (term) {
    const result = []
    for (const key of this.userDefinedLabels.keys()) {
      if (key.toLowerCase().startsWith(term.toLowerCase())) {
        result.push(key)
      }
    }
    return result
  }

  getUserDefinedTagsForLabel (label) {
    return this.userDefinedLabels.get(label)
  }

  getUserDefinedTags2 () {
    let all = []
    this.userDefinedLabels.each((tags, label) => {
      const t = tags.map(tt => ({ ...tt }))
      all = all.concat(t)
    })
    return all
  }

  getEntityKeys (keyspace) {
    return this.entities.keys()
      .filter(key => unprefix(key)[0] === keyspace)
      .map(key => unprefix(key)[1])
  }
}
