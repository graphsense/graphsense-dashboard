import {map} from 'd3-collection'
import GraphNode from './graphNode.js'

const padding = 10
const addressLabelHeight = 25

export default class AddressNode extends GraphNode {
  constructor (address, layerId, labelType, graph) {
    super(labelType, graph)
    this.address = address
    this.id = [address.address, layerId]
    this.outgoingTxsFilters = map()
    this.incomingTxsFilters = map()
    // absolute coords for linking, not meant for rendering of the node itself
    this.x = 0
    this.y = 0
  }
  render (root, x, y, height, width) {
    this.x = x
    this.y = y
    this.width = width
    this.height = height
    this.root = root
    this.root.classed('addressNode', true)
      .on('click', () => {
        this.graph.dispatcher.call('selectNode', null, ['address', this.id])
      })
    this.root.append('rect')
      .attr('x', x)
      .attr('y', y)
      .attr('width', width)
      .attr('height', height)
      .attr('rx', 10)
      .attr('ry', 10)

    this.root.append('text')
      .attr('x', x + padding)
      .attr('y', y + height / 2 + addressLabelHeight / 3)
      .style('font-size', addressLabelHeight + 'px')
      .text(this.getLabel())
    if (this.graph.selectedNode === this) {
      this.select()
    }
  }
  getLabel () {
    switch (this.labelType) {
      case 'id':
        return this.address.address.substring(0, 8)
      case 'balance':
        return this.address.totalReceived.satoshi - this.address.totalSpent.satoshi
      case 'tag':
        return this.address.getTag()
    }
  }
}
