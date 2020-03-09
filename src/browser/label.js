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
        { icon: 'tags', optionText: 'Tags', message: 'initTagsTable' }
      ]
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    super.render()
    this.root.innerHTML = replace(this.template, this.data)
    return this.root
  }

  requestData () {
    return { ...super.requestData(), id: this.data.label_norm, type: 'label' }
  }
}
