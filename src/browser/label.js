import { tt } from '../lang.js'
import { replace } from '../template_utils'
import { firstToUpper } from '../utils'
import BrowserComponent from './component.js'

export default class Label extends BrowserComponent {
  constructor (dispatcher, data, index, currency, currencies) {
    super(dispatcher, index, currency)
    this.data = data
    if (!this.data.tags) this.data.tags = {}
    this.options = []
    this.makeTemplate(currencies)
    this.currencies = currencies
  }

  makeOptionId (nodeType, currency) {
    return `row-${nodeType}-${currency}`
  }

  makeTemplate (currencies) {
    this.template =
      '<div class="prop-table">' +
      ' <div class="prop-row">' +
      '   <span class="prop-key">{{te:Label}}</span>' +
      '   <span class="prop-value">{{label}}</span>' +
      ' </div>'
    currencies.forEach(curr => {
      (['address', 'entity']).forEach(nodeType => {
        const inline = this.makeOptionId(nodeType, curr)
        const plural = nodeType === 'address' ? 'addresses' : 'entities'
        this.template +=
        `<div class="prop-row" id="${inline}">` +
        ` <span class="prop-key">${curr.toUpperCase()} {{te:${plural}}}</span>` +
        ` <span class="prop-value">{{no_${curr}_${nodeType}_tags}}</span>` +
        '</div>'
        this.options.push({
          inline,
          optionText: firstToUpper(nodeType) + ' tags',
          message: 'initTagsTable',
          params: [nodeType, curr]
        })
      })
    })
    this.template += '</div>'
  }

  getData (currency, nodeType) {
    if (!this.data[currency]) return
    return this.data[currency][nodeType]
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    super.render()
    const flat =
      {
        label: this.data.label
      };
    (['address', 'entity']).forEach(nodeType => {
      this.currencies.forEach(curr => {
        let c = ''
        if (this.data.tags[curr]) {
          if (this.data.tags[curr][nodeType]) {
            c = this.data.tags[curr][nodeType].length
          }
        }
        flat['no_' + curr + '_' + nodeType + '_tags'] = c
      })
    })
    this.root.innerHTML = replace(tt(this.template), flat)
    this.renderInlineOptions()
    return this.root
  }

  requestData () {
    return { ...super.requestData(), id: this.data.label, type: 'label' }
  }
}
