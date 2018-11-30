import {event} from 'd3-selection'
import {GraphNode, addressHeight, addressWidth} from './graphNode.js'

const padding = 10
export default class AddressNode extends GraphNode {
  constructor (dispatcher, address, layerId, labelType, colors, currency) {
    super(dispatcher, labelType, address, layerId, colors, currency)
    // absolute coords for linking, not meant for rendering of the node itself
    this.x = 0
    this.y = 0
    this.type = 'address'
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (this.shouldUpdate() === true) {
      this.root.node().innerHTML = ''
      let x = 0
      let y = 0
      let g = this.root
        .append('g')
        .classed('addressNode', true)
        .on('click', () => {
          this.dispatcher('selectNode', ['address', this.id])
        })
        .on('contextmenu', () => {
          event.stopPropagation()
          event.preventDefault()
          this.dispatcher('contextmenu', {x: event.x, y: event.y, node: this})
        })
      g.append('rect')
        .classed('rect', true)
        .attr('x', x)
        .attr('y', y)
        .attr('width', addressWidth)
        .attr('height', addressHeight)

      let h = y + addressHeight / 2 + this.labelHeight / 3
      let label = g.append('g')
        .classed('label', true)
        .attr('transform', `translate(${x + padding}, ${h})`)

      this.renderLabel(label)
      let eg = g.append('g').classed('expandHandles', true)
      this.renderRemove(g)
      this.renderExpand(eg, true)
      this.renderExpand(eg, false)
      this.coloring()
      this.renderSelected()
    } else {
      if (this.shouldUpdate() === 'label' || this.shouldUpdate() === 'select+label') {
        let label = this.root.select('g.label')
        this.renderLabel(label)
        this.coloring()
      }
      if (this.shouldUpdate() === 'select' || this.shouldUpdate() === 'select+label') {
        this.renderSelected()
      }
    }
    super.render()
  }
  getHeight () {
    return addressHeight
  }
  getWidth () {
    return addressWidth
  }
  getOutDegree () {
    return this.data.out_degree
  }
  getInDegree () {
    return this.data.in_degree
  }
  getId () {
    return this.data.address
  }
}
