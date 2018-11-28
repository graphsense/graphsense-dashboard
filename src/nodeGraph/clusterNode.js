import {map} from 'd3-collection'
import {GraphNode, addressWidth, addressHeight, clusterWidth, padding, expandHandleWidth} from './graphNode.js'

const gap = padding
const noAddressesLabelHeight = 16
const paddingBottom = 7

export default class ClusterNode extends GraphNode {
  constructor (dispatcher, cluster, layerId, labelType, colors) {
    super(dispatcher, labelType, cluster, layerId, colors)
    this.nodes = map()
    this.addressFilters = map()
    this.addressFilters.set('limit', 10)
    this.expandLimit = 10
    this.type = 'cluster'
    this.numLetters = 11
  }
  add (node) {
    if (!node.id) throw new Error('not a node', node)
    this.nodes.set(node.id, node)
  }
  has (address) {
    this.nodes.has([address, this.id[1]])
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (this.shouldUpdate() === true) {
      this.root.node().innerHTML = ''
      if (!this.data.mockup) {
        let height = this.getHeight()
        let g = this.root.append('g')
          .classed('clusterNode', true)
          .on('click', () => {
            this.dispatcher('selectNode', ['cluster', this.id])
          })
        g.append('rect')
          .classed('rect', true)
          .attr('x', 0)
          .attr('y', 0)
          .attr('width', clusterWidth)
          .attr('height', height)
        let label = g.append('g')
          .classed('label', true)
          .attr('transform', `translate(${padding}, ${padding / 2 + this.labelHeight})`)
        this.renderLabel(label)
        let eg = g.append('g').classed('expandHandles', true)
        this.renderRemove(g)
        this.renderExpand(eg, true)
        this.renderExpand(eg, false)
        this.coloring()
        this.renderSelected()
      }
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
  renderAddresses (root) {
    if (!this.shouldUpdate()) {
      this.nodes.each(addressNode => addressNode.render())
      return
    }
    root.node().innerHTML = ''
    let cumY = 2 * padding + this.labelHeight
    this.nodes.each((addressNode) => {
      let g = root.append('g')
      addressNode.shouldUpdate(true)
      // reset absolute coords
      addressNode.x = 0
      addressNode.y = 0
      let x = padding + expandHandleWidth
      let y = cumY
      addressNode.render(g)
      addressNode.translate(x, y)
      g.attr('transform', `translate(${x}, ${y})`)
      cumY += addressNode.getHeight()
    })
    if (this.data.mockup) return
    cumY += this.nodes.size() > 0 ? gap : 0
    let button = root.append('g')
      .classed('addressExpand', true)
      .on('click', (e) => {
        this.dispatcher('loadClusterAddresses', {id: this.id, limit: this.addressFilters.get('limit')})
      })
    let h = this.getHeight()
    let w = this.getWidth()
    let lineY = h - paddingBottom - noAddressesLabelHeight
    button.append('text')
      .attr('text-anchor', 'middle')
      .attr('x', w / 2)
      .attr('y', h - paddingBottom)
      .attr('font-size', noAddressesLabelHeight)
      .text(this.data.noAddresses + ' addresses')
    super.render()
  }
  translate (x, y) {
    super.translate(x, y)
    this.nodes.each((node) => {
      node.translate(x, y)
    })
  }
  getHeight () {
    return this.nodes.size() * addressHeight +
      2 * padding +
      (this.data.mockup ? 0 : this.labelHeight + noAddressesLabelHeight) +
      (this.nodes.size() > 0 ? 2 * gap : gap)
  }
  getWidth () {
    return clusterWidth
  }
  getOutDegree () {
    return this.data.out_degree
  }
  getInDegree () {
    return this.data.in_degree
  }
  getId () {
    return this.data.cluster
  }
}
