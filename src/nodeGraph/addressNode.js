import {map} from 'd3-collection'

const padding = 10
const addressLabelHeight = 25

export default class AddressNode {
  constructor (address, cluster, height, width) {
    this.address = address
    this.clusterNode = cluster
    this.height = height
    this.width = width
    this.id = [address.address, cluster.layer.id]
    this.outgoingTxsFilters = map()
    this.incomingTxsFilters = map()
  }
  render (root, x, y) {
    this.x = x
    this.y = y
    this.root = root
    this.root.classed('addressNode', true)
    this.root.append('rect')
      .attr('x', x)
      .attr('y', y)
      .attr('width', this.width)
      .attr('height', this.height)
      .attr('rx', 10)
      .attr('ry', 10)
      .on('click', () => {
        this.clusterNode.layer.graph.dispatcher.call('selectAddress', null, this.id)
      })
    this.root.append('text')
      .attr('x', x + padding)
      .attr('y', y + this.height / 2 + addressLabelHeight / 3)
      .style('font-size', addressLabelHeight + 'px')
      .text(this.address.address.substring(0, 8))
    if (this.clusterNode.layer.graph.selectedNode === this) {
      this.select()
    }
  }
  select () {
    this.root.classed('selected', true)
  }
  deselect () {
    this.root.classed('selected', false)
  }
}
