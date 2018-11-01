import cluster from './cluster.html'
import Address from './address.js'

export default class Cluster extends Address {
  constructor (dispatcher, data, index) {
    super(dispatcher, data, index)
    this.template = cluster
    this.options =
      [
        {icon: 'at', optionText: 'Addresses', message: 'initAddressesTable'},
        {icon: 'tags', optionText: 'Tags', message: 'initTagsTable'},
        {icon: 'plus', optionText: 'Add to graph', message: 'addNode'}
      ]
  }
  requestData () {
    return {id: this.data.cluster, type: 'cluster', index: this.index}
  }
}
