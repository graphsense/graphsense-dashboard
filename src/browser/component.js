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
    this.options = []
  }
  setCurrentOption (option) {
    this.currentOption = option
    this.setUpdate(true)
  }
  addOption (option) {
    this.options.push(option)
  }
  renderOptions () {
    if (!this.options || this.options.length === 0) return
    let ul = document.createElement('ul')
    ul.className = 'list-reset'
    this.options.forEach((optionData) => {
      let li = document.createElement('li')
      li.className = 'cursor-pointer py-1 ' +
        (this.currentOption === optionData.message ? 'option-active' : '')
      let optionHtml = option
      if (optionData.html) {
        optionHtml = optionData.html
      }
      li.innerHTML = replace(optionHtml, optionData)
      li.addEventListener('click', () => {
        this.dispatcher(optionData.message, this.requestData())
      })
      ul.appendChild(li)
    })
    return ul
  }
  destroy () {
  }
  formatCurrency (value, keyspace, colorful) {
    let c = formatCurrency(value, this.currency, {keyspace})
    if (!colorful) return c
    let cl = value < 0 ? 'text-gs-red' : (value > 0 ? 'text-gs-base' : '')
    return `<span class="${cl}">${c}</span>`
  }
  formatTimestamp (timestamp) {
    let t = moment.unix(timestamp)
    return t.format('L') + ' ' + t.format('LTS')
  }
  formatTimestampWithAgo (timestamp) {
    return this.formatTimestamp(timestamp) + ' <span class="text-grey-dark">(' + this.formatAgo(timestamp) + ')</span>'
  }
  formatAgo (timestamp) {
    return moment.unix(timestamp).fromNow()
  }
  setCurrency (currency) {
    this.currency = currency
    this.setUpdate(true)
  }
  requestData () {
    return {index: this.index}
  }
  formatDuration (duration) {
    duration = moment.duration(duration)
    let years = duration.years()
    let months = duration.months()
    let days = duration.days()
    let hours = duration.hours()
    let minutes = duration.minutes()
    let seconds = duration.seconds()
    let format = (num, str, strs) => {
      if (num === 1) return `${num} ${str}`
      else if (num > 1) return `${num} ${strs}`
      return ''
    }
    if (years > 0) {
      return `${format(years, 'year', 'years')} ` +
        `${format(months, 'month', 'months')} ` +
        `${format(days, 'day', 'days')} `
    }
    if (months > 0) {
      return `${format(months, 'month', 'months')} ` +
        `${format(days, 'day', 'days')} `
    }
    if (days > 0) {
      return `${format(days, 'day', 'days')} ` +
        `${format(hours, 'hour', 'hours')} `
    }
    if (hours > 0) {
      return `${format(hours, 'hour', 'hours')} ` +
        `${format(minutes, 'minute', 'minutes')} `
    }

    if (minutes > 0) {
      return `${format(minutes, 'minute', 'minutes')} ` +
        `${format(seconds, 'second', 'seconds')} `
    }

    return `${format(seconds, 'second', 'seconds')} `
  }
}
