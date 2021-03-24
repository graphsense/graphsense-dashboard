import entity from './entity.html'
import Address from './address.js'

export default class Entity extends Address {
  constructor (dispatcher, data, index, currency, categories) {
    super(dispatcher, data, index, currency, categories)
    this.template = entity
    this.options =
      [
        { inline: 'row-incoming', optionText: 'Sending entities', message: 'initIndegreeTable' },
        { inline: 'row-outgoing', optionText: 'Receiving entities', message: 'initOutdegreeTable' },
        { inline: 'row-addresses', optionText: 'Addresses', message: 'initAddressesTable' },
        { inline: 'row-tags', optionText: 'Tags', message: 'initTagsTable' }
      ]
  }

  flattenData () {
    const flat = super.flattenData()
    flat.no_addresses = this.data.reduce((sum, v) => sum + v.no_addresses, 0)

    return flat
  }
}
