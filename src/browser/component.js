import { t } from '../lang.js'
import { replace, addClass } from '../template_utils.js'
import option from './option.html'
import { formatCurrency } from '../utils.js'
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

  currentOptionMatches (option) {
    if (!this.currentOption) return false
    if (typeof (option) === 'string') return this.currentOption === option
    if (!option.params) return this.currentOption === option.message
    return this.currentOption.message === option.message && this.currentOption.params[0] === option.params[0] && this.currentOption.params[1] === option.params[1]
  }

  addOption (option) {
    this.options.push(option)
  }

  optionEl (parent, optionData) {
    parent.className = 'cursor-pointer ' +
      (this.currentOptionMatches(optionData) ||
        !optionData.inline ? 'option-active' : '')
    let optionHtml = option
    if (optionData.html) {
      optionHtml = optionData.html
    }
    optionData.optionText = t(optionData.optionText)
    parent.innerHTML = replace(optionHtml, optionData)
    parent.addEventListener('click', () => {
      const r = this.requestData()
      r.optionParams = optionData.params
      this.dispatcher(optionData.message, r)
    })
    return parent
  }

  renderOuterOptions () {
    if (!this.options || this.options.length === 0) return

    const ul = document.createElement('ul')
    ul.className = 'list-reset'
    this.options.forEach((optionData) => {
      if (optionData.inline) return
      const el = this.optionEl(document.createElement('li'), optionData)
      addClass(el, 'py-1')
      ul.appendChild(el)
    })
    return ul
  }

  renderInlineOptions () {
    if (!this.options || this.options.length === 0) return

    this.options.forEach((optionData) => {
      if (!optionData.inline) return
      const row = this.root.querySelector('#' + optionData.inline)
      if (!row) return
      const icon = { icon: 'ellipsis-h' }
      const iconDiv = this.optionEl(document.createElement('div'), { ...optionData, ...icon })
      const div = document.createElement('div')
      div.className = 'option-inline-wrapper'
      div.appendChild(iconDiv)
      const arrowDiv = document.createElementNS('http://www.w3.org/2000/svg', 'svg')
      arrowDiv.setAttributeNS(null, 'viewBox', '0 0 10 20')
      arrowDiv.setAttributeNS(null, 'class', 'option-arrow')
      const path = document.createElementNS('http://www.w3.org/2000/svg', 'path')
      path.setAttributeNS(null, 'd', 'M10 0 0 10 10 20')
      if (this.currentOptionMatches(optionData)) div.appendChild(arrowDiv)
      row.appendChild(div)
      arrowDiv.appendChild(path)
    })
  }

  destroy () {
  }

  formatCurrency (value, keyspace, colorful) {
    const c = formatCurrency(value, this.currency, { keyspace })
    if (!colorful) return c
    const cl = value < 0 ? 'text-gs-red' : (value > 0 ? 'text-gs-base' : '')
    return `<span class="${cl}">${c}</span>`
  }

  formatTimestamp (timestamp) {
    const t = moment.unix(timestamp)
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
    return { index: this.index }
  }

  formatDuration (duration) {
    duration = moment.duration(duration)
    const years = duration.years()
    const months = duration.months()
    const days = duration.days()
    const hours = duration.hours()
    const minutes = duration.minutes()
    const seconds = duration.seconds()
    const format = (num, str, strs) => {
      if (num === 1) return `${num} ${str}`
      else if (num > 1) return `${num} ${strs}`
      return ''
    }
    if (years > 0) {
      return `${format(years, t('year'), t('years'))} ` +
        `${format(months, t('month'), t('months'))} ` +
        `${format(days, t('day'), t('days'))} `
    }
    if (months > 0) {
      return `${format(months, t('month'), t('months'))} ` +
        `${format(days, t('day'), t('days'))} `
    }
    if (days > 0) {
      return `${format(days, t('day'), t('days'))} ` +
        `${format(hours, t('hour'), t('hours'))} `
    }
    if (hours > 0) {
      return `${format(hours, t('hour'), t('hours'))} ` +
        `${format(minutes, t('minute'), t('minutes'))} `
    }

    if (minutes > 0) {
      return `${format(minutes, t('minute'), t('minutes'))} ` +
        `${format(seconds, t('second'), t('seconds'))} `
    }

    return `${format(seconds, t('second'), t('seconds'))} `
  }
}
