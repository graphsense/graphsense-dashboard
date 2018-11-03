import {map} from 'd3-collection'
import GraphNode from './graphNode.js'

const padding = 10
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

    let h = this.y + this.height / 2 + this.labelHeight / 3
    let label = this.root.append('g')
      .attr('transform', `translate(${this.x + padding}, ${h})`)

    this.renderLabel(label)
    if (this.graph.selectedNode === this) {
      this.select()
    }
  }
  getLabel () {
    switch (this.labelType) {
      case 'id':
        return (this.address.address + '').substring(0, 8)
      case 'balance':
        return this.address.totalReceived.satoshi - this.address.totalSpent.satoshi + ''
      case 'tag':
        return this.getTag(this.address) + ''
      case 'actorCategory':
        return this.getActorCategory(this.address) + ''
    }
  }
  select () {
    this.root.classed('selected', true)
  }
  deselect () {
    this.root.classed('selected', false)
  }
}
