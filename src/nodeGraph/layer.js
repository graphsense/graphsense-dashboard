import Component from '../component.js'
import { map } from 'd3-collection'
import Logger from '../logger.js'

const logger = Logger.create('Layer') // eslint-disable-line no-unused-vars
const margin = 20

export default class Layer extends Component {
  constructor (id) {
    super()
    this.id = id * 1
    this.nodes = map()
    this.x = 0
    this.y = 0
  }

  serialize () {
    return [this.id, this.nodes.keys()]
  }

  add (node, addToTop = false) {
    if (this.nodes.has(node.id)) return
    // calc dy so new node does not overlap with existing, moved nodes
    const [maxY, cumY] = this.nodes.values().reduce(([maxY, cumY], node) => {
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
    const node = this.nodes.get(nodeId)
    if (!node) return

    let min = Infinity
    let max = -Infinity
    this.nodes.each(node => {
      const y = node.getYForLinks()
      min = Math.min(min, y)
      max = Math.max(max, y)
    })
    const half = (max + min) / 2
    return node.getYForLinks() < half
  }

  render (entityRoot, addressRoot) {
    if (entityRoot) this.entityRoot = entityRoot
    if (addressRoot) this.addressRoot = addressRoot
    if (!this.entityRoot) throw new Error('no entityRoot defined')
    if (!this.addressRoot) throw new Error('no addressRoot defined')
    let cumY = 0
    const renderNodeWithPosition = (node, entityRoot, addressesRoot) => {
      // reset absolute coords of node
      entityRoot = entityRoot || node.root
      addressesRoot = addressesRoot || node.addressesRoot
      node.x = 0
      node.y = 0
      node.render(entityRoot)
      entityRoot.attr('transform', `translate(0, ${cumY})`)
      // render addresses
      node.setUpdate(true)
      node.renderAddresses(addressesRoot)
      addressesRoot.attr('transform', `translate(${node.dx + node.ddx}, ${cumY + node.dy + node.ddy})`)
      // translate entity node and its addresses
      node.translate(0, cumY)
    }
    this.getSortedNodes().forEach((node) => {
      // render entities
      if (this.shouldUpdate()) {
        const g = this.entityRoot.append('g')
        const ag = this.addressRoot.append('g')
        node.setUpdate(true)
        renderNodeWithPosition(node, g, ag)
      } else if (node.shouldUpdate('position')) {
        node.setUpdate(true)
        renderNodeWithPosition(node)
        node.translate(this.x, this.y)
      } else {
        node.render()
        node.renderAddresses()
      }
      cumY += node.getHeight() + margin
    })
    super.render()
  }

  translate (x, y) {
    this.x = x
    this.y = y
    this.nodes.each((node) => {
      node.translate(x, y)
    })
  }
}
