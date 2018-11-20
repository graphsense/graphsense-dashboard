import {GraphNode, addressHeight, addressWidth} from './graphNode.js'

const padding = 10
export default class AddressNode extends GraphNode {
  constructor (address, layerId, labelType, graph) {
    super(labelType, graph)
    this.address = address
    this.id = [address.address, layerId]
    // absolute coords for linking, not meant for rendering of the node itself
    this.x = 0
    this.y = 0
    this.type = 'address'
  }
  render (root, x, y) {
    this.x = x
    this.y = y
    x = this.x
    this.root = root
    let g = this.root
      .append('g')
      .classed('addressNode', true)
      .on('click', () => {
        this.graph.dispatcher.call('selectNode', null, ['address', this.id])
      })
    g.append('rect')
      .attr('x', x)
      .attr('y', y)
      .attr('width', addressWidth)
      .attr('height', addressHeight)

    let h = this.y + addressHeight / 2 + this.labelHeight / 3
    let label = g.append('g')
      .attr('transform', `translate(${x + padding}, ${h})`)

    this.renderLabel(label)
    let eg = this.root.append('g').classed('expandHandles', true)
      .attr('transform', `translate(${this.x}, ${this.y})`)
    this.renderExpand(eg, true)
    this.renderExpand(eg, false)
    if (this.graph.selectedNode === this) {
      this.select()
    }
  }
  getLabel () {
    switch (this.labelType) {
      case 'id':
        return (this.address.address + '').substring(0, 8)
      case 'balance':
        return this.formatCurrency(this.address.totalReceived.satoshi - this.address.totalSpent.satoshi)
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
  getHeight () {
    return addressHeight
  }
  getWidth () {
    return addressWidth
  }
  getOutDegree () {
    return this.address.out_degree
  }
  getInDegree () {
    return this.address.in_degree
  }
  getId () {
    return this.address.address
  }
}
