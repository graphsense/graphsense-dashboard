import {set, map} from 'd3-collection'

const minWidth = 160
const padding = 10
const addressHeight = 50
const gap = padding
const labelHeight = 20
const addressMinWidth = minWidth - 2 * padding
export default class ClusterNode {
  constructor (cluster, layerId, graph) {
    this.id = [cluster.cluster, layerId]
    this.cluster = cluster
    this.graph = graph
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
      let size = this.nodes.size()
      this.height = size * addressHeight + 2 * padding + labelHeight + gap
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
        .attr('height', this.height)
      g.append('text')
        .attr('x', padding)
        .attr('y', this.height - padding)
        .style('font-size', labelHeight + 'px')
        .text(`${size} + ${cluster.noAddresses - size}`)
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
    this.x += x
    this.y += y
    this.nodes.each((nodeId) => {
      this.graph.addressNodes.get([nodeId, this.id[1]]).translate(x, y)
    })
  }
  select () {
    this.root.select('g').classed('selected', true)
  }
  deselect () {
    this.root.select('g').classed('selected', false)
  }
  getX () {
    return this.x
  }
  getY () {
    return this.y
  }
  getWidth () {
    return this.width
  }
  getHeight () {
    return this.height
  }
}
