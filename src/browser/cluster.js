import cluster from './cluster.html'
import Address from './address.js'

export default class Cluster extends Address {
  constructor (dispatcher, data) {
    super(dispatcher, data)
    this.template = cluster
    this.options =
      [
        {icon: 'sign-in-alt', optionText: 'Incoming transactions', message: 'loadIncomingTxs'},
        {icon: 'sign-out-alt', optionText: 'Outgoing transactions', message: 'loadOutgoingTxs'},
        {icon: 'at', optionText: 'Addresses', message: 'loadAddresses'},
        {icon: 'tags', optionText: 'Tags', message: 'loadClusterTags'},
        {icon: 'plus', optionText: 'Add to graph', message: 'addNode'}
      ]
  }
  requestData () {
    return {id: this.data.cluster, type: 'cluster'}
  }
}
