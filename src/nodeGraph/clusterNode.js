import AddressNode from './addressNode.js'
import {map} from 'd3-collection'

const minWidth = 160
const padding = 10
const addressHeight = 50
const gap = padding
const labelHeight = 20
const addressMinWidth = minWidth - 2 * padding

export default class ClusterNode {
  constructor (cluster, layer) {
    this.layer = layer
    this.cluster = cluster
    this.id = cluster.cluster
    this.nodes = map()
    cluster.addresses.each((address) => {
      let a = this.layer.graph.store.get('address', address)
      let ad = new AddressNode(a, this, addressHeight, addressMinWidth)
      this.add(ad)
    })
  }
  add (node) {
    this.nodes.set(node.id, node)
  }
  findAddressNode (address) {
    return this.nodes.get(address)
  }
  render (root) {
    let size = this.cluster.addresses.size()
    let height = size * addressHeight + 2 * padding + labelHeight + gap
    root.append('rect')
      .attr('x', 0)
      .attr('y', 0)
      .attr('width', minWidth)
      .attr('height', height)
      .style('stroke-dasharray', '5')
      .style('stroke', 'black')
      .style('fill', 'none')
    root.append('text')
      .attr('x', padding)
      .attr('y', height - padding)
      .style('font-size', labelHeight + 'px')
      .text(`${size} + ${this.cluster.noAddresses - size}`)
    let cumY = padding
    this.nodes.each((address) => {
      let g = root.append('g')
      address.render(g, padding, cumY)
      cumY += addressHeight
    })
  }
}
