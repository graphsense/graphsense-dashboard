import address from './address.html'
import moment from 'moment'
import {replace} from '../template_utils'
import BrowserComponent from './component.js'

export default class Address extends BrowserComponent {
  constructor (dispatcher, data, index) {
    super(dispatcher, index)
    this.data = data
    this.template = address
    this.options =
      [
        {icon: 'exchange-alt', optionText: 'Transactions', message: 'initTransactionsTable'},
        {icon: 'tags', optionText: 'Tags', message: 'initTagsTable'},
        {icon: 'plus', optionText: 'Add to graph', message: 'addNode'}
      ]
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
    this.root.innerHTML = replace(this.template, {...this.data, ...flat})
    return this.root
  }
  requestData () {
    return {id: this.data.address, type: 'address', index: this.index}
  }
}
