import Component from '../component.js'
import {map} from 'd3-collection'

const margin = 20

export default class Layer extends Component {
  constructor (id) {
    super()
    this.id = id
    this.nodes = map()
  }
  serialize () {
    return [this.id, this.nodes.keys()]
  }
  add (node) {
    this.nodes.set(node.id, node)
  }
  has (nodeId) {
    return this.nodes.has(nodeId)
  }
  render (clusterRoot, addressRoot) {
    if (clusterRoot) this.clusterRoot = clusterRoot
    if (addressRoot) this.addressRoot = addressRoot
    if (!this.clusterRoot) throw new Error('no clusterRoot defined')
    if (!this.addressRoot) throw new Error('no addressRoot defined')
    let cumY = 0
    this.nodes.each((node) => {
      // render clusters
      if (this.shouldUpdate()) {
        let g = this.clusterRoot.append('g')
        node.shouldUpdate(true)
        // reset absolute coords of node
        node.x = 0
        node.y = 0
        node.render(g)
        g.attr('transform', `translate(0, ${cumY})`)
        // render addresses
        let ag = this.addressRoot.append('g')
        node.shouldUpdate(true)
        node.renderAddresses(ag)
        ag.attr('transform', `translate(0, ${cumY})`)

        // translate cluster node and its addresses
        node.translate(0, cumY)
        let height = node.getHeight()
        cumY += height + margin
      } else {
        node.render()
        node.renderAddresses()
      }
    })
    super.render()
  }
  translate (x, y) {
    this.nodes.each((node) => {
      node.translate(x, y)
    })
  }
}
