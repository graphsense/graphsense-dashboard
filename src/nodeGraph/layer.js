import {map} from 'd3-collection'

const margin = 10

export default class Layer {
  constructor (graph) {
    this.graph = graph
    this.nodes = map()
  }
  add (node) {
    this.nodes.set(node.id, node)
  }
  render (root) {
    let cumY = 0
    this.nodes.each((node, id) => {
      let g = root.append('g')
      node.render(g)
      let box = g.node().getBBox()
      g.attr('transform', `translate(0, ${cumY})`)
      cumY += box.height + margin
    })
  }
}
