import {replace} from '../template_utils.js'
import option from './option.html'
import {formatCurrency} from '../utils.js'
import moment from 'moment'
import Component from '../component.js'

export default class BrowserComponent extends Component {
  constructor (dispatcher, index, currency) {
    super()
    this.index = index
    this.dispatcher = dispatcher
    this.currency = currency
    this.currentOption = null
  }
  setCurrentOption (option) {
    this.currentOption = option
    this.shouldUpdate(true)
  }
  renderOptions () {
    let ul = document.createElement('ul')
    ul.className = 'list-reset'
    this.options.forEach((optionData) => {
      let li = document.createElement('li')
      li.className = 'cursor-pointer py-1 ' +
        (this.currentOption === optionData.message ? 'option-active' : '')
      li.innerHTML = replace(option, optionData)
      li.addEventListener('click', () => {
        this.dispatcher(optionData.message,
          { id: this.data.id,
            type: this.data.type,
            keyspace: this.data.keyspace,
            index: this.index
          })
      })
      ul.appendChild(li)
    })
    return ul
  }
  destroy () {
  }
  formatCurrency (value, keyspace) {
    return formatCurrency(value, this.currency, {keyspace})
  }
  formatTimestamp (timestamp) {
    return moment.unix(timestamp).format('DD.MM.YYYY HH:mm:ss')
  }
  setCurrency (currency) {
    this.currency = currency
    this.shouldUpdate(true)
  }
}
