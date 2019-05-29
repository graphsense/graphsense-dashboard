import {event} from 'd3-selection'
import {map} from 'd3-collection'
import {GraphNode, addressHeight, clusterWidth, padding, expandHandleWidth} from './graphNode.js'
import numeral from 'numeral'
import contextMenu from 'd3-context-menu'

const gap = padding
const noAddressesLabelHeight = 16
const paddingBottom = 7
const noExpandableAddresses = 16

export default class ClusterNode extends GraphNode {
  constructor (dispatcher, cluster, layerId, labelType, colors, currency) {
    super(dispatcher, labelType, cluster, layerId, colors, currency)
    this.nodes = map()
    this.addressFilters = map()
    this.addressFilters.set('limit', 10)
    this.expandLimit = 10
    this.type = 'cluster'
    this.numLetters = 11
  }
  expandable () {
    return this.data.noAddresses < noExpandableAddresses
  }
  menu () {
    return super.menu([
      {
        title: () => this.expandable() && this.nodes.empty() ? 'Expand' : (!this.nodes.empty() ? 'Collapse' : 'Expand'),
        disabled: () => !this.expandable(),
        action: () => this.nodes.empty()
          ? this.dispatcher('loadClusterAddresses', {id: this.id, keyspace: this.data.keyspace, limit: this.data.noAddresses})
          : this.dispatcher('removeClusterAddresses', this.id),
        position: 50
      }
    ])
  }
  serialize () {
    let s = super.serialize()
    s.push(this.nodes.keys())
    return s
  }
  deserialize (version, [x, y, nodes], addressNodes) {
    super.deserialize([x, y])
    nodes.forEach(key => {
      if (version === '0.4.0') {
        key += ',' + this.data.keyspace
      }
      this.add(addressNodes.get(key))
    })
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
    if (this.shouldUpdate(true)) {
      this.root.node().innerHTML = ''
      if (!this.data.mockup) {
        let height = this.getHeight()
        let g = this.root
          .append('g')
          .classed('clusterNode', true)
          .on('click', () => {
            event.stopPropagation()
            this.dispatcher('selectNode', ['cluster', this.id])
          })
          .on('contextmenu', contextMenu(this.menu()))
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
        this.renderExpand(eg, true)
        this.renderExpand(eg, false)
        this.coloring()
        this.renderSelected()
      }
    } else {
      if (this.shouldUpdate('label')) {
        let label = this.root.select('g.label')
        this.renderLabel(label)
        this.coloring()
      }
      if (this.shouldUpdate('select')) {
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
      addressNode.setUpdate(true)
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
    let size = this.nodes.size()
    cumY += size > 0 ? gap : 0
    let button = root.append('g')
      .classed('addressExpand', true)
    let h = this.getHeight()
    let w = this.getWidth()
    let num = (n) => numeral(n).format('0,000')
    button.append('text')
      .attr('text-anchor', 'middle')
      .attr('x', w / 2)
      .attr('y', h - paddingBottom)
      .attr('font-size', noAddressesLabelHeight)
      .text((size > 0 ? num(size) + '/' : '') + num(this.data.noAddresses) + ' addresses')
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
  getId () {
    return this.data.cluster
  }
}
