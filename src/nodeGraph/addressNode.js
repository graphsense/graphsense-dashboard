import { event } from 'd3-selection'
import { GraphNode, addressHeight, addressWidth } from './graphNode.js'
import contextMenu from 'd3-context-menu'

const iconAbuse = 'M296 160H180.6l42.6-129.8C227.2 15 215.7 0 200 0H56C44 0 33.8 8.9 32.2 20.8l-32 240C-1.7 275.2 9.5 288 24 288h118.7L96.6 482.5c-3.6 15.2 8 29.5 23.3 29.5 8.4 0 16.4-4.4 20.8-12l176-304c9.3-15.9-2.2-36-20.7-36z'

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
    if (this.shouldUpdate(true)) {
      this.root.node().innerHTML = ''
      const x = 0
      const y = 0
      const g = this.root
        .append('g')
        .classed('addressNode', true)
        .on('click', () => {
          event.stopPropagation()
          this.dispatcher('selectNode', ['address', this.id])
        })
        .on('contextmenu', contextMenu(this.menu()))
        .on('mouseover', () => this.dispatcher('tooltip', 'address'))
        .on('mouseout', () => this.dispatcher('hideTooltip'))
      g.append('rect')
        .classed('addressNodeRect', true)
        .attr('x', x)
        .attr('y', y)
        .attr('width', addressWidth)
        .attr('height', addressHeight)

      const h = y + addressHeight / 2 + this.labelHeight / 3
      const label = g.append('g')
        .classed('label', true)
        .attr('transform', `translate(${x + padding}, ${h})`)

      const flags = g.append('g')
        .attr('transform', `translate(${x + this.getWidth() - padding / 2}, ${y + padding / 2})`)
        .attr('class', 'abuse')

      this.renderLabel(label)
      this.renderFlags(flags)
      const eg = g.append('g')
      this.renderExpand(eg, true)
      this.renderExpand(eg, false)
      this.coloring()
      this.renderSelected()
    } else {
      if (this.shouldUpdate('label')) {
        const label = this.root.select('g.label')
        this.renderLabel(label)
        this.coloring()
      }
      if (this.shouldUpdate('select')) {
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

  getId () {
    return this.data.address
  }

  renderFlags (root) {
    const abuse = (this.data.tags || []).filter(({ abuse }) => !!abuse)
    if (!abuse) return
    root.append('path')
      .attr('transform', 'translate(-7, 0) scale(0.025) ')
      .attr('d', iconAbuse)
  }
}
