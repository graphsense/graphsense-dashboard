import address from './address.html'
import {replace} from '../template_utils'
import moment from 'moment'

export default class Address {
  constructor (data) {
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
}
