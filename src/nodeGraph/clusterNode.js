import {map} from 'd3-collection'
import RMap from '../rmap.js'
import {GraphNode, addressWidth, addressHeight, clusterWidth, padding, expandHandleWidth} from './graphNode.js'

const gap = padding
const buttonHeight = 25
const buttonLabelHeight = 20

export default class ClusterNode extends GraphNode {
  constructor (dispatcher, cluster, layerId, labelType) {
    super(dispatcher, labelType, cluster, layerId)
    this.nodes = new RMap()
    this.addressFilters = map()
    this.addressFilters.set('limit', 10)
    this.expandLimit = 10
    this.type = 'cluster'
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
          .attr('x', 0)
          .attr('y', 0)
          .attr('width', clusterWidth)
          .attr('height', height)
        let label = g.append('g')
          .classed('label', true)
          .attr('transform', `translate(${padding}, ${padding / 2 + this.labelHeight})`)
        this.renderLabel(label)
        let eg = this.root.append('g').classed('expandHandles', true)
        this.renderRemove(g)
        this.renderExpand(eg, true)
        this.renderExpand(eg, false)
      }
    } else {
      if (this.shouldUpdate() === 'label' || this.shouldUpdate() === 'select+label') {
        let label = this.root.select('g.label')
        this.renderLabel(label)
      }
      if (this.shouldUpdate() === 'select' || this.shouldUpdate() === 'select+label') {
        this.root.select('g').classed('selected', this.selected)
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
    button.append('rect')
      .attr('x', padding)
      .attr('y', cumY)
      .attr('width', addressWidth)
      .attr('height', buttonHeight)
      .attr('rx', 5)
      .attr('ry', 5)
    button.append('text')
      .attr('x', padding * 2)
      .attr('y', cumY + buttonHeight / 2 + buttonLabelHeight / 3)
      .text(`+ Addresses`)
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
      (this.data.mockup ? 0 : this.labelHeight + buttonHeight + padding) +
      (this.nodes.size() > 0 ? 2 * gap : gap)
  }
  getWidth () {
    return clusterWidth
  }
  getLabel () {
    switch (this.labelType) {
      case 'noAddresses':
        return this.data.noAddresses
      case 'id':
        return this.data.id
      case 'tag':
        return this.getTag(this.data)
      case 'actorCategory':
        return this.getActorCategory(this.data) + ''
    }
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
