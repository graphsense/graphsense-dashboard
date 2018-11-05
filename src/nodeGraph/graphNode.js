import {formatCurrency} from '../utils'
import {map} from 'd3-collection'

export default class GraphNode {
  constructor (labelType, graph) {
    this.labelType = labelType
    this.graph = graph
    this.labelHeight = 25
    this.padding = 10
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
    let len = 30
    let b = len / 2
    let a = Math.sqrt(len * len - b * b)
    let shift = this.type === 'cluster' ? 0 : this.padding
    let width = this.getWidth() + shift
    let height = this.getHeight()
    let x = isOutgoing ? width : shift
    let y = height / 2 - b + shift
    let r = isOutgoing ? 0 : 180
    root.append('path')
      .classed('expandHandle', true)
      .attr('d', `M0 0 L${a} ${b} L0 ${len} Z`)
      .attr('transform', `translate(${x}, ${y}) rotate(${r} 0 ${b} )`)
      .on('click', () => {
        console.log('click expand')
        let filters
        if (isOutgoing) {
          filters = this.outgoingTxsFilters
        } else {
          filters = this.incomingTxsFilters
        }
        this.graph.dispatcher.call('applyTxFilters', null, [this.id, isOutgoing, this.type, filters])
      })
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
  getWidth () {
    return this.width
  }
  getHeight () {
    return this.height
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
