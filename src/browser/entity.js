import entity from './entity.html'
import Address from './address.js'
import incomingNeighbors from '../icons/incomingNeighbors.html'
import outgoingNeighbors from '../icons/outgoingNeighbors.html'

export default class Entity extends Address {
  constructor (dispatcher, data, index, currency) {
    super(dispatcher, data, index, currency)
    this.template = entity
    this.options =
      [
        { html: incomingNeighbors, optionText: 'Incoming neighbors', message: 'initIndegreeTable' },
        { html: outgoingNeighbors, optionText: 'Outgoing neighbors', message: 'initOutdegreeTable' },
        { icon: 'at', optionText: 'Addresses', message: 'initAddressesTable' },
        { icon: 'tags', optionText: 'Tags', message: 'initTagsTable' }
      ]
  }

  flattenData () {
    const flat = super.flattenData()
    flat.no_addresses = this.data.reduce((sum, v) => sum + v.no_addresses, 0)

    return flat
  }
}
