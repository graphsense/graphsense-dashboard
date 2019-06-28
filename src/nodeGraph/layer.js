import Component from '../component.js'
import {map} from 'd3-collection'
import Logger from '../logger.js'

const logger = Logger.create('Layer') // eslint-disable-line no-unused-vars
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
  add (node, addToTop = false) {
    if (this.nodes.has(node.id)) return
    // calc dy so new node does not overlap with existing, moved nodes
    let [maxY, cumY] = this.nodes.values().reduce(([maxY, cumY], node) => {
      cumY += node.getHeight() + margin
      maxY = Math.max(maxY, cumY + node.dy)
      return [maxY, cumY]
    }, [0, 0])
    if (maxY > cumY) {
      node.setDY(maxY - cumY)
    }
    if (addToTop) {
      node.position = 0
      this.nodes.each(node => { node.position++ })
    } else {
      node.position = this.nodes.size()
    }
    this.nodes.set(node.id, node)
  }
  remove (nodeId) {
    this.nodes.remove(nodeId)
    this.getSortedNodes().forEach((node, i) => { node.position = i })
  }
  getSortedNodes () {
    return this.nodes.values().sort((nodeA, nodeB) => nodeA.position - nodeB.position)
  }
  has (nodeId) {
    return this.nodes.has(nodeId)
  }
  getHeight () {
    let height = 0
    this.nodes.each(node => {
      height += node.getHeight()
    })
    return height
  }
  isNodeInUpperHalf (nodeId) {
    let node = this.nodes.get(nodeId)
    if (!node) return

    let min = Infinity
    let max = -Infinity
    this.nodes.each(node => {
      let y = node.getYForLinks()
      min = Math.min(min, y)
      max = Math.max(max, y)
    })
    let half = (max + min) / 2
    return node.getYForLinks() < half
  }
  render (clusterRoot, addressRoot) {
    if (clusterRoot) this.clusterRoot = clusterRoot
    if (addressRoot) this.addressRoot = addressRoot
    if (!this.clusterRoot) throw new Error('no clusterRoot defined')
    if (!this.addressRoot) throw new Error('no addressRoot defined')
    let cumY = 0
    this.getSortedNodes().forEach((node) => {
      // render clusters
      if (this.shouldUpdate()) {
        let g = this.clusterRoot.append('g')
        node.setUpdate(true)
        // reset absolute coords of node
        node.x = 0
        node.y = 0
        node.render(g)
        g.attr('transform', `translate(0, ${cumY})`)
        // render addresses
        let ag = this.addressRoot.append('g')
        node.setUpdate(true)
        node.renderAddresses(ag)
        ag.attr('transform', `translate(${node.dx}, ${cumY + node.dy})`)

        // translate cluster node and its addresses
        node.translate(0, cumY)
        cumY += node.getHeight() + margin
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
