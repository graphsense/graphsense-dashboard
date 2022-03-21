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
      active: true,
      address: id
    }
  }

  isClusterDefiner (isClusterDefiner) {
    this.data.is_cluster_definer = isClusterDefiner
    return this
  }

  isUserDefined (isUserDefined) {
    this.data.isUserDefined = isUserDefined
    return this
  }

  label (label) {
    this.data.label = label
    return this
  }

  source (source) {
    this.data.source = source
    return this
  }

  tagpack_uri (tagpack_uri) { // eslint-disable-line
    this.data.tagpack_uri = tagpack_uri // eslint-disable-line
    return this
  }

  currency (currency) {
    this.data.currency = currency
    return this
  }

  lastmod (lastmod) {
    this.data.lastmod = lastmod
    return this
  }

  category (category) {
    this.data.category = category
    return this
  }

  abuse (abuse) {
    this.data.abuse = abuse
    return this
  }

  keyspace (keyspace) {
    this.data.keyspace = keyspace
    return this
  }

  active (active) {
    this.data.active = active
    return this
  }
}
