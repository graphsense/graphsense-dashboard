import address from './address.html'
import moment from 'moment'
import {replace} from '../template_utils'
import BrowserComponent from './component.js'

export default class Address extends BrowserComponent {
  constructor (dispatcher, data, index, currency) {
    super(dispatcher, index, currency)
    this.data = data
    this.template = address
    this.options =
      [
        {icon: 'sign-in-alt', optionText: 'Incoming neighbors', message: 'initIndegreeTable'},
        {icon: 'sign-out-alt', optionText: 'Outgoing neighbors', message: 'initOutdegreeTable'},
        {icon: 'exchange-alt', optionText: 'Transactions', message: 'initTransactionsTable'},
        {icon: 'tags', optionText: 'Tags', message: 'initTagsTable'}
      ]
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    super.render()
    let first = this.data.firstTx.timestamp
    let last = this.data.lastTx.timestamp
    let duration = (last - first) * 1000
    let flat = {
      firstUsage: this.formatTimestampWithAgo(first),
      lastUsage: this.formatTimestampWithAgo(last),
      activityPeriod: moment.duration(duration).humanize(),
      totalReceived: this.formatCurrency(this.data.totalReceived[this.currency], this.data.keyspace),
      finalBalance: this.formatCurrency(this.data.balance[this.currency], this.data.keyspace),
      keyspace: this.data.keyspace.toUpperCase()
    }
    this.root.innerHTML = replace(this.template, {...this.data, ...flat})
    return this.root
  }
  requestData () {
    return {...super.requestData(), id: this.data.id, type: this.data.type}
  }
}
