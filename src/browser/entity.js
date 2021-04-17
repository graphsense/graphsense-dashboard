import entity from './entity.html'
import Address from './address.js'
import { t } from '../lang.js'

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
    const esc = s => s.replace(' ', '&nbsp;')
    flat.no_addresses = this.data.reduce((sum, v) => sum + v.no_addresses, 0)
    flat.label_receiving_entities = esc(t('Receiving entities'))
    flat.label_sending_entities = esc(t('Sending entities'))
    flat.label_tag_coherence = esc(t('Tag coherence'))

    return flat
  }
}
