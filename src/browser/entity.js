import entity from './entity.html'
import Address from './address.js'
import numeral from 'numeral'
import Logger from '../logger.js'

const logger = Logger.create('Entity') // eslint-disable-line no-unused-vars

export default class Entity extends Address {
  constructor (dispatcher, data, index, currency, categories, colors) {
    super(dispatcher, data, index, currency, categories, colors)
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
    const num = n => numeral(n).format('1,000')
    const flat = super.flattenData()
    flat.no_addresses = num(this.data.reduce((sum, v) => sum + v.no_addresses, 0))
    flat.no_entity_tags = num(this.data.reduce((sum, v) => sum + ((v.tags || {}).entity_tags || []).length, 0))
    flat.label = ''
    flat.root_address = '<div>' + this.data.map(d => d.root_address).join('</div><div>') + '</div>'
    if (this.data.length === 1 && this.data[0].tags.entity_tags.length === 1) {
      flat.label = this.data[0].tags.entity_tags[0].label
      flat.id = this.data[0].id
      flat.style = `style="background-color: ${this.colors[this.data[0].tags.entity_tags[0].category] || 'unset'}"`
    }
    return flat
  }

  flattenTags () {
    return this.data.reduce((tags, d) => tags.concat(d.tags.address_tags || []), [])
  }
}
