import {replace} from '../template_utils.js'
import option from './option.html'
import {formatCurrency} from '../utils.js'
import moment from 'moment'
import Component from '../component.js'

export default class BrowserComponent extends Component {
  constructor (dispatcher, index) {
    super()
    this.index = index
    this.dispatcher = dispatcher
    this.currency = 'btc'
  }
  renderOptions () {
    let ul = document.createElement('ul')
    ul.className = 'list-reset'
    this.options.forEach((optionData) => {
      let li = document.createElement('li')
      li.className = 'cursor-pointer py-1'
      li.innerHTML = replace(option, optionData)
      li.addEventListener('click', () => {
        this.dispatcher(optionData.message, {id: this.data.id, type: this.data.type, index: this.index})
      })
      ul.appendChild(li)
    })
    return ul
  }
  destroy () {
  }
  formatCurrency (value) {
    return formatCurrency(value, this.currency)
  }
  formatTimestamp (timestamp) {
    return moment.unix(timestamp).format('DD.MM.YYYY HH:mm:ss')
  }
}
