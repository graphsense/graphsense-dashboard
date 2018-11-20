import {formatCurrency} from '../utils'
import {map} from 'd3-collection'

const padding = 10
const clusterWidth = 190
const expandHandleWidth = 15
const addressWidth = clusterWidth - 2 * padding - 2 * expandHandleWidth
const addressHeight = 50

class GraphNode {
  constructor (labelType, graph) {
    this.labelType = labelType
    this.graph = graph
    this.labelHeight = 25
    this.numLetters = 8
    this.currency = 'btc'
    this.outgoingTxsFilters = map()
    this.incomingTxsFilters = map()
    this.outgoingTxsFilters.set('limit', 10)
    this.incomingTxsFilters.set('limit', 10)
  }
  renderLabel (root) {
    if (!root) {
      root = this.root.select('g.label')
    } else {
      root.classed('label', true)
    }
    let label = this.getLabel()
    let size
    if (label.length > this.numLetters) {
      if (label.length > this.numLetters * 2) {
        size = this.labelHeight * 0.5
        label = label.substring(0, this.numLetters * 2)
      } else {
        size = this.labelHeight * this.numLetters / label.length
      }
    } else {
      size = this.labelHeight
    }
    root.node().innerHTML = ''
    root.append('text')
      .style('font-size', size + 'px')
      .text(label)
  }
  renderExpand (root, isOutgoing) {
    let width = this.getWidth()
    let x = isOutgoing ? width : 0
    let y = 0
    let r = isOutgoing ? 0 : 180
    let a = expandHandleWidth
    let h = this.getHeight()
    let c = h - a
    let g = root.append('g')
      .classed('expandHandle', true)
      .on('click', () => {
        let filters
        if (isOutgoing) {
          filters = this.outgoingTxsFilters
        } else {
          filters = this.incomingTxsFilters
        }
        this.graph.dispatcher.call('applyTxFilters', null, [this.id, isOutgoing, this.type, filters])
      })
    g.append('path')
      .attr('d', `M0 0 C ${a} 0, ${a} 0, ${a} ${a} L ${a} ${c} C ${a} ${h} ${a} ${h} 0 ${h}`)
    let fontSize = expandHandleWidth * 0.8
    let fontX = (expandHandleWidth - fontSize)
    g.append('text')
      .text(isOutgoing ? this.getOutDegree() : this.getInDegree())
      .attr('text-anchor', 'middle')
      .attr('font-size', fontSize + 'px')
      .attr('transform', `translate(${fontX}, ${h / 2}) rotate(90)`)

    g.attr('transform', `translate(${x}, ${y}) rotate(${r} 0 ${h / 2} )`)
  }
  translate (x, y) {
    this.x += x
    this.y += y
  }
  getX () {
    return this.x
  }
  getY () {
    return this.y
  }
  getXForLinks () {
    return this.getX() - expandHandleWidth
  }
  getYForLinks () {
    return this.getY()
  }
  getHeightForLinks () {
    return this.getHeight()
  }
  getWidthForLinks () {
    return this.getWidth() + 2 * expandHandleWidth
  }
  setLabelType (labelType) {
    this.labelType = labelType
  }
  getTag (object) {
    if (object.userDefinedTags) {
      return object.userDefinedTags[0] || ''
    }
    return (this.findTag(object) || {}).tag || ''
  }
  getActorCategory (object) {
    return (this.findTag(object) || {}).actorCategory || ''
  }
  findTag (object) {
    let tags = (object || {}).tags || []
    tags.sort((a, b) => {
      return a - b
    })
    for (let i = 0; i < tags.length; i++) {
      if (tags[i].actorCategory) return tags[i]
    }
  }
  formatCurrency (value) {
    return formatCurrency(value, this.currency)
  }
}

export {GraphNode, addressWidth, addressHeight, padding, clusterWidth, expandHandleWidth}
