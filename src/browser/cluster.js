import cluster from './cluster.html'
import Address from './address.js'

export default class Cluster extends Address {
  constructor (dispatcher, data, index, currency) {
    super(dispatcher, data, index, currency)
    this.template = cluster
    this.options =
      [
        {icon: 'sign-in-alt', optionText: 'Incoming neighbors', message: 'initIndegreeTable'},
        {icon: 'sign-out-alt', optionText: 'Outgoing neighbors', message: 'initOutdegreeTable'},
        {icon: 'at', optionText: 'Addresses', message: 'initAddressesTable'},
        {icon: 'tags', optionText: 'Tags', message: 'initTagsTable'}
      ]
  }
  requestData () {
    return {id: this.data.id, type: 'cluster', index: this.index}
  }
}
