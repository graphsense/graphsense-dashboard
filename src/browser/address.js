import address from './address.html'
import {replace} from '../template_utils'
import moment from 'moment'
import option from './option.html'

const options =
  [
    {icon: 'sign-in-alt', optionText: 'Incoming transactions', message: 'loadIncomingTxs'},
    {icon: 'sign-out-alt', optionText: 'Outgoing transactions', message: 'loadOutgoingTxs'},
    {icon: 'tags', optionText: 'Tags', message: 'loadTags'},
    {icon: 'plus', optionText: 'Add to graph', message: 'addAddress'}
  ]

export default class Address {
  constructor (dispatcher, data) {
    this.dispatcher = dispatcher
    this.root = document.createElement('div')
    this.data = data
  }
  render () {
    let first = this.data.firstTx.timestamp
    let last = this.data.lastTx.timestamp
    let duration = (last - first) * 1000
    let flat = {
      firstUsage: moment.unix(first).fromNow(),
      lastUsage: moment.unix(last).fromNow(),
      activityPeriod: moment.duration(duration).humanize(),
      totalReceived: this.data.totalReceived.satoshi,
      finalBalance: this.data.totalReceived.satoshi - this.data.totalSpent.satoshi
    }
    this.root.innerHTML = replace(address, {...this.data, ...flat})
    return this.root
  }
  renderOptions () {
    let ul = document.createElement('ul')
    ul.className = 'list-reset'
    options.forEach((optionData) => {
      let li = document.createElement('li')
      li.className = 'cursor-pointer py-1'
      li.innerHTML = replace(option, optionData)
      li.addEventListener('click', () => {
        this.dispatcher.call(optionData.message, null, this.data.address)
      })
      ul.appendChild(li)
    })
    return ul
  }
}
