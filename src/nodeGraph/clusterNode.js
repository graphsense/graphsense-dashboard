const minWidth = 160
const padding = 10
const addressHeight = 50
const gap = padding
const labelHeight = 20
const addressLabelHeight = 25
const addressMinWidth = minWidth - 2 * padding

export default class ClusterNode {
  constructor (cluster, layer) {
    this.layer = layer
    this.cluster = cluster
    this.id = cluster.cluster
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
    this.cluster.addresses.each((address) => {
      let a = this.layer.graph.store.get('address', address)
      this.renderAddress(root, padding, cumY, a)
      cumY += addressHeight
    })
  }
  renderAddress (root, x, y, address) {
    root.append('rect')
      .attr('x', x)
      .attr('y', y)
      .attr('width', addressMinWidth)
      .attr('height', addressHeight)
      .attr('rx', 10)
      .attr('ry', 10)
      .style('fill', 'none')
      .style('stroke', 'black')
    root.append('text')
      .attr('x', x + padding)
      .attr('y', y + addressHeight / 2 + addressLabelHeight / 3)
      .style('font-size', addressLabelHeight + 'px')
      .text(address.address.substring(0, 8))
  }
}
