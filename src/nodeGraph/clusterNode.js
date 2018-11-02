import {set, map} from 'd3-collection'
import GraphNode from './graphNode.js'

const minWidth = 160
const padding = 10
const addressHeight = 50
const gap = padding
const labelHeight = 20
const addressMinWidth = minWidth - 2 * padding
export default class ClusterNode extends GraphNode {
  constructor (cluster, layerId, labelType, graph) {
    super(labelType, graph)
    this.id = [cluster.cluster, layerId]
    this.cluster = cluster
    this.nodes = set()
    this.outgoingTxsFilters = map()
    this.incomingTxsFilters = map()
    this.addressFilters = map()
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
    console.log('render clusterNode', this.cluster)
    let cluster = this.cluster
    if (!cluster.mockup) {
      let height = this.getHeight()
      this.width = minWidth
      let g = root.append('g')
        .classed('clusterNode', true)
        .on('click', () => {
          console.log('click')
          this.graph.dispatcher.call('selectNode', null, ['cluster', this.id])
        })
      g.append('rect')
        .attr('x', 0)
        .attr('y', 0)
        .attr('width', minWidth)
        .attr('height', height)
      g.append('text')
        .attr('x', padding)
        .attr('y', height - padding)
        .style('font-size', labelHeight + 'px')
        .text(this.getLabel())
      if (this.graph.selectedNode === this) {
        this.select()
      }
    }
    let cumY = padding
    this.nodes.each((addressId) => {
      let addressNode = this.graph.addressNodes.get([addressId, this.id[1]])
      let g = root.append('g')
      addressNode.render(g, padding, cumY, addressHeight, addressMinWidth)
      cumY += addressHeight
    })
  }
  translate (x, y) {
    super.translate(x, y)
    this.nodes.each((nodeId) => {
      this.graph.addressNodes.get([nodeId, this.id[1]]).translate(x, y)
    })
  }
  getHeight () {
    return this.nodes.size() * addressHeight + 2 * padding + labelHeight + gap
  }
  getLabel () {
    switch (this.labelType) {
      case 'noAddresses':
        return this.cluster.noAddresses
      case 'id':
        return this.cluster.cluster
      case 'tag':
        return this.cluster.getTag()
    }
  }
}
