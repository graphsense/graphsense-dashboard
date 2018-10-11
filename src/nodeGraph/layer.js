import {map} from 'd3-collection'

const margin = 10

export default class Layer {
  constructor (graph, id) {
    this.id = id
    this.graph = graph
    this.nodes = map()
  }
  add (node) {
    this.nodes.set(node.id, node)
  }
  findAddressNode (address) {
    let a = this.graph.store.get('address', address)
    // if(!a || !a.cluster) return
    console.log(this.nodes, a, a.cluster)
    let c = this.nodes.get(a.cluster)
    // f(!c) return
    console.log('layer', c)
    return c.findAddressNode(address)
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
