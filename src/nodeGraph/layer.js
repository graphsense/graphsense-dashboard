import Component from '../component.js'
import { map } from 'd3-collection'
import Logger from '../logger.js'
import { entityWidth, layerMargin } from '../globals.js'

const logger = Logger.create('Layer') // eslint-disable-line no-unused-vars
const margin = 20

export default class Layer extends Component {
  constructor (id) {
    super()
    this.id = id * 1
    this.nodes = map()
    this.x = (entityWidth + layerMargin) * this.id
    this.y = 0
  }

  serialize () {
    return [this.id, this.nodes.keys()]
  }

  add (node, anchor) {
    if (this.nodes.has(node.id)) return
    node.translate(this.x, (anchor && anchor.y) || 0)
    this.nodes.set(node.id, node)
    this.repositionNodesAround(node)
  }

  repositionNodesAround (node) {
    logger.debug('repositionAround', node.id[0], node, node.getHeight())
    this.nodes.each(sister => {
      logger.debug('reposition sister', sister.id[0], sister, sister.getHeight())
      if (node === sister) return
      let diff = node.getY() + node.getHeight() - sister.getY()
      logger.debug('reposition diff 1', diff)
      if (diff >= 0 && diff <= sister.getHeight()) {
        sister.translate(0, diff + margin)
        this.repositionNodesAround(sister)
        return
      }
      diff = sister.getY() + sister.getHeight() - node.getY()
      logger.debug('reposition diff 2', diff)
      if (diff >= 0 && diff <= node.getHeight()) {
        sister.translate(0, -diff - margin)
        this.repositionNodesAround(sister)
      }
    })
  }

  remove (nodeId) {
    this.nodes.remove(nodeId)
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
      height += node.getHeight() + margin
    })
    height -= margin
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
    const renderNodeWithPosition = (node, entityRoot, addressesRoot) => {
      // reset absolute coords of node
      entityRoot = entityRoot || node.root
      addressesRoot = addressesRoot || node.addressesRoot
      node.render(entityRoot)
      // render addresses
      node.setUpdate(true)
      node.renderAddresses(addressesRoot)
    }
    this.nodes.values().forEach((node) => {
      // render entities
      if (this.shouldUpdate()) {
        const g = this.entityRoot.append('g')
        const ag = this.addressRoot.append('g')
        node.setUpdate(true)
        renderNodeWithPosition(node, g, ag)
      } else if (node.shouldUpdate('position')) {
        node.setUpdate(true)
        renderNodeWithPosition(node)
      } else {
        node.render()
        node.renderAddresses()
      }
    })
    // this.entityRoot.attr('transform', `translate(${this.x}, ${this.y})`)
    // this.addressRoot.attr('transform', `translate(${this.x}, ${this.y})`)
    super.render()
  }

  translate (x, y) {
    this.x += x
    this.y += y
    this.nodes.each((node) => {
      node.translate(x, y)
    })
  }
}
