import {set, map} from 'd3-collection'
import {GraphNode, addressWidth, addressHeight, clusterWidth, padding, expandHandleWidth} from './graphNode.js'

const gap = padding
const buttonHeight = 25
const buttonLabelHeight = 20

export default class ClusterNode extends GraphNode {
  constructor (cluster, layerId, labelType, graph) {
    super(labelType, graph)
    this.id = [cluster.cluster, layerId]
    this.cluster = cluster
    this.nodes = set()
    this.addressFilters = map()
    this.addressFilters.set('limit', 10)
    this.expandLimit = 10
    this.type = 'cluster'
  }
  add (nodeId) {
    this.nodes.add(nodeId)
  }
  has (address) {
    this.nodes.has([address, this.id[1]])
  }
  render (root) {
    // absolute coords for linking, not meant for rendering of the node itself
    this.x = 0
    this.y = 0
    this.root = root
    let cluster = this.cluster
    if (!cluster.mockup) {
      let height = this.getHeight()
      let g = root.append('g')
        .classed('clusterNode', true)
        .on('click', () => {
          this.graph.dispatcher.call('selectNode', null, ['cluster', this.id])
        })
      g.append('rect')
        .attr('x', 0)
        .attr('y', 0)
        .attr('width', clusterWidth)
        .attr('height', height)
      let label = g.append('g')
        .attr('transform', `translate(${padding}, ${height - padding})`)
      this.renderLabel(label)
      let eg = this.root.append('g').classed('expandHandles', true)
      this.renderExpand(eg, true)
      this.renderExpand(eg, false)
      if (this.graph.selectedNode === this) {
        this.select()
      }
    }
  }
  renderAddresses (root) {
    let cumY = padding
    this.nodes.each((addressId) => {
      let addressNode = this.graph.addressNodes.get([addressId, this.id[1]])
      let g = root.append('g')
      addressNode.render(g, padding + expandHandleWidth, cumY)
      cumY += addressNode.getHeight()
    })
    if (this.cluster.mockup) return
    cumY += this.nodes.size() > 0 ? gap : 0
    let button = root.append('g')
      .classed('addressExpand', true)
      .on('click', (e) => {
        this.graph.dispatcher.call('applyAddressFilters', null, [this.id, this.addressFilters])
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
  }
  translate (x, y) {
    super.translate(x, y)
    this.nodes.each((nodeId) => {
      this.graph.addressNodes.get([nodeId, this.id[1]]).translate(x, y)
    })
  }
  getHeight () {
    return this.nodes.size() * addressHeight +
      2 * padding +
      this.labelHeight + buttonHeight +
      (this.nodes.size() > 0 ? 2 * gap : gap)
  }
  getWidth () {
    return clusterWidth
  }
  getLabel () {
    switch (this.labelType) {
      case 'noAddresses':
        return this.cluster.noAddresses
      case 'id':
        return this.cluster.cluster
      case 'tag':
        return this.getTag(this.cluster)
      case 'actorCategory':
        return this.getActorCategory(this.cluster) + ''
    }
  }
  select () {
    this.root.select('g').classed('selected', true)
  }
  deselect () {
    this.root.select('g').classed('selected', false)
  }
  getOutDegree () {
    return this.cluster.out_degree
  }
  getInDegree () {
    return this.cluster.in_degree
  }
}
