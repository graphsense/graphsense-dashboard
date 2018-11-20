import {set} from 'd3-collection'

const margin = 20

export default class Layer {
  constructor (graph, id) {
    this.id = id
    this.graph = graph
    this.nodes = set()
  }
  add (nodeId) {
    this.nodes.add(nodeId)
  }
  has (nodeId) {
    return this.nodes.has(nodeId)
  }
  render (clusterRoot, addressRoot) {
    let cumY = 0
    this.nodes.each((nodeId) => {
      let node = this.graph.clusterNodes.get([nodeId, this.id])
      // render clusters
      let g = clusterRoot.append('g')
      node.render(g)
      g.attr('transform', `translate(0, ${cumY})`)
      // render addresses
      let ag = addressRoot.append('g')
      node.renderAddresses(ag)
      ag.attr('transform', `translate(0, ${cumY})`)

      // translate cluster node and its addresses
      node.translate(0, cumY)
      let height = node.getHeight()
      cumY += height + margin
    })
  }
  translate (x, y) {
    this.nodes.each((nodeId) => {
      this.graph.clusterNodes.get([nodeId, this.id]).translate(x, y)
    })
  }
}
