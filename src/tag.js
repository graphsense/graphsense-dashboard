export default class Tag {
  constructor (keyspace, label, id, type) {
    this.data = {
      isUserDefined: true,
      label: label,
      source: null,
      tagpack_uri: null,
      currency: keyspace.toUpperCase(),
      lastmod: +new Date() / 1000,
      category: null,
      abuse: null,
      keyspace: keyspace,
      active: true
    }
    this.data[type] = id
  }

  isUserDefined (isUserDefined) {
    this.isUserDefined = isUserDefined
    return this
  }

  label (label) {
    this.label = label
    return this
  }

  source (source) {
    this.source = source
    return this
  }

  tagpack_uri (tagpack_uri) { // eslint-disable-line
    this.tagpack_uri = tagpack_uri // eslint-disable-line
    return this
  }

  currency (currency) {
    this.currency = currency
    return this
  }

  lastmod (lastmod) {
    this.lastmod = lastmod
    return this
  }

  category (category) {
    this.category = category
    return this
  }

  abuse (abuse) {
    this.abuse = abuse
    return this
  }

  keyspace (keyspace) {
    this.keyspace = keyspace
    return this
  }

  active (active) {
    this.active = active
    return this
  }
}
