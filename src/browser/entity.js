import entity from './entity.html'
import Address from './address.js'
import { t } from '../lang.js'
import numeral from 'numeral'

export default class Entity extends Address {
  constructor (dispatcher, data, index, currency, categories) {
    super(dispatcher, data, index, currency, categories)
    this.template = entity
    this.options =
      [
        { inline: 'row-transactions', optionText: 'Transactions', message: 'initTransactionsTable' },
        { inline: 'row-incoming', optionText: 'Sending entities', message: 'initIndegreeTable' },
        { inline: 'row-outgoing', optionText: 'Receiving entities', message: 'initOutdegreeTable' },
        { inline: 'row-addresses', optionText: 'Addresses', message: 'initAddressesTable' },
        { inline: 'row-tags', optionText: 'Address Tags', message: 'initTagsTable', params: ['address', this.data[0].keyspace] },
        { inline: 'row-entity-tags', optionText: 'Entity Tags', message: 'initTagsTable', params: ['entity', this.data[0].keyspace] }
      ]
  }

  flattenData () {
    const flat = super.flattenData()
    flat.no_addresses = this.data.reduce((sum, v) => sum + v.no_addresses, 0)
    flat.no_entity_tags = this.data.reduce((sum, v) => sum + ((v.tags || {}).entity_tags || []).length, 0)
    flat.tagCoherence = this.data.length === 1 && (this.data[0].tags || {}).tag_coherence !== null
      ? numeral(this.data[0].tags.tag_coherence).format('0.[00]%')
      : t('unknown')
    flat.label = ''
    if (this.data.length === 1 && this.data[0].tags.entity_tags.length === 1) {
      flat.label = this.data[0].tags.entity_tags[0].label
      flat.id = this.data[0].id
    }
    return flat
  }

  flattenTags () {
    return this.data.reduce((tags, d) => tags.concat(d.tags.address_tags || []), [])
  }
}
