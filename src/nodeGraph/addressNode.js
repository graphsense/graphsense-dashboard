import {map} from 'd3-collection'

const padding = 10
const addressLabelHeight = 25

export default class AddressNode {
  constructor (address, layerId, graph) {
    this.address = address
    this.graph = graph
    this.id = [address.address, layerId]
    this.outgoingTxsFilters = map()
    this.incomingTxsFilters = map()
  }
  render (root, x, y, height, width) {
    this.x = x
    this.y = y
    this.root = root
    this.root.classed('addressNode', true)
    this.root.append('text')
      .attr('x', x + padding)
      .attr('y', y + height / 2 + addressLabelHeight / 3)
      .style('font-size', addressLabelHeight + 'px')
      .text(this.id[0].substring(0, 8))
    this.root.append('rect')
      .attr('x', x)
      .attr('y', y)
      .attr('width', width)
      .attr('height', height)
      .attr('rx', 10)

      .attr('ry', 10)
      .on('click', () => {
        this.graph.dispatcher.call('selectAddress', null, this.id)
      })
    if (this.graph.selectedNode === this) {
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
