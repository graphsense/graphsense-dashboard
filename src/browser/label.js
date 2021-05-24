import { tt } from '../lang.js'
import label from './label.html'
import { replace } from '../template_utils'
import BrowserComponent from './component.js'

export default class Label extends BrowserComponent {
  constructor (dispatcher, data, index, currency) {
    super(dispatcher, index, currency)
    this.data = data
    this.template = label
    this.options =
      [
        { inline: 'row-addresses', optionText: 'Address tags', message: 'initTagsTable' },
        { inline: 'row-entities', optionText: 'Entity tags', message: 'initEntityTagsTable' }
      ]
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    super.render()
    const flat =
      {
        label: this.data.label,
        no_address_tags: this.data.tags.address_tags.length,
        no_entity_tags: this.data.tags.entity_tags.length
      }
    this.root.innerHTML = replace(tt(this.template), flat)
    this.renderInlineOptions()
    return this.root
  }

  requestData () {
    return { ...super.requestData(), id: this.data.label, type: 'label' }
  }
}
