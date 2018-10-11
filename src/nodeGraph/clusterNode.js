import {set, map} from 'd3-collection'

const minWidth = 160
const padding = 10
const addressHeight = 50
const gap = padding
const labelHeight = 20
const addressMinWidth = minWidth - 2 * padding
export default class ClusterNode {
  constructor (cluster, layerId, graph) {
    this.id = [cluster, layerId]
    this.graph = graph
    this.nodes = set()
    this.outgoingTxsFilters = map()
    this.incomingTxsFilters = map()
    this.addressFilters = map()
  }
  add (nodeId) {
    this.nodes.add(nodeId)
  }
  render (root) {
    // absolute coords for linking, not meant for rendering of the node itself
    this.x = 0
    this.y = 0
    this.root = root
    console.log('clusterNode', this.graph, this.id)
    let cluster = this.graph.store.get('cluster', this.id[0])
    if (!cluster.mockup) {
      let size = this.nodes.size()
      this.height = size * addressHeight + 2 * padding + labelHeight + gap
      this.width = minWidth
      let g = root.append('g')
        .classed('clusterNode', true)
      g.append('text')
        .attr('x', padding)
        .attr('y', this.height - padding)
        .style('font-size', labelHeight + 'px')
        .text(`${size} + ${cluster.noAddresses - size}`)
      g.append('rect')
        .attr('x', 0)
        .attr('y', 0)
        .attr('width', minWidth)
        .attr('height', this.height)
        .on('click', () => {
          console.log('click')
          this.graph.dispatcher.call('selectNode', null, ['cluster', this.id])
        })
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
}
