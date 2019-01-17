import {formatCurrency} from '../utils'
import {map} from 'd3-collection'
import {event} from 'd3-selection'
import Component from '../component.js'
import Logger from '../logger.js'
import numeral from 'numeral'

const logger = Logger.create('GraphNode') // eslint-disable-line no-unused-vars

const padding = 10
const clusterWidth = 190
const expandHandleWidth = 15
const addressWidth = clusterWidth - 2 * padding - 2 * expandHandleWidth
const addressHeight = 50
const removeHandleWidth = 15
const removeHandlePadding = 5

const closePath = 'M256 8C119 8 8 119 8 256s111 248 248 248 248-111 248-248S393 8 256 8zm121.6 313.1c4.7 4.7 4.7 12.3 0 17L338 377.6c-4.7 4.7-12.3 4.7-17 0L256 312l-65.1 65.6c-4.7 4.7-12.3 4.7-17 0L134.4 338c-4.7-4.7-4.7-12.3 0-17l65.6-65-65.6-65.1c-4.7-4.7-4.7-12.3 0-17l39.6-39.6c4.7-4.7 12.3-4.7 17 0l65 65.7 65.1-65.6c4.7-4.7 12.3-4.7 17 0l39.6 39.6c4.7 4.7 4.7 12.3 0 17L312 256l65.6 65.1z'

class GraphNode extends Component {
  constructor (dispatcher, labelType, data, layerId, colors, currency) {
    super()
    this.data = data
    this.id = [this.data.id, layerId]
    this.labelType = labelType
    this.dispatcher = dispatcher
    this.labelHeight = 25
    this.numLetters = 8
    this.currency = currency
    this.outgoingTxsFilters = map()
    this.incomingTxsFilters = map()
    this.outgoingTxsFilters.set('limit', 10)
    this.incomingTxsFilters.set('limit', 10)
    this.colors = colors
    // absolute coords for linking, not meant for rendering of the node itself
    this.x = 0
    this.y = 0
  }
  serialize () {
    return [
      this.x,
      this.y
    ]
  }
  deserialize ([x, y]) {
    this.x = x
    this.y = y
  }
  renderLabel (root) {
    if (this.data.mockup) return
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
        this.dispatcher('loadEgonet', {id: this.id, isOutgoing, type: this.data.type, limit: filters.get('limit'), keyspace: this.data.keyspace})
      })
    g.append('path')
      .attr('d', `M0 0 C ${a} 0, ${a} 0, ${a} ${a} L ${a} ${c} C ${a} ${h} ${a} ${h} 0 ${h}`)
    let fontSize = expandHandleWidth * 0.8
    let fontX = (expandHandleWidth - fontSize)
    g.append('text')
      .text(numeral(isOutgoing ? this.getOutDegree() : this.getInDegree()).format('1,000'))
      .attr('text-anchor', 'middle')
      .attr('font-size', fontSize + 'px')
      .attr('transform', `translate(${fontX}, ${h / 2}) rotate(90)`)

    g.attr('transform', `translate(${x}, ${y}) rotate(${r} 0 ${h / 2} )`)
  }
  renderRemove (root) {
    let w = removeHandleWidth
    let x = this.getWidth() - w - removeHandlePadding
    let y = removeHandlePadding
    let fontSize = removeHandleWidth
    let g = root.append('g')
      .classed('removeHandle', true)
      .on('click', () => {
        this.dispatcher('removeNode', [this.type, this.id])
        event.stopPropagation()
      })
    g.append('rect')
      .attr('x', 0)
      .attr('y', 0)
      .attr('width', w)
      .attr('height', w)
    g.append('path')
      .attr('d', closePath)
      .attr('transform', 'scale(0.028)')
      .attr('x', w / 2)
      .attr('y', w / 2 + fontSize / 3)
    g.attr('transform', `translate(${x}, ${y})`)
  }
  renderSelected () {
    this.root.select('g').classed('selected', this.selected)
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
    this.shouldUpdateLabel()
  }
  getName () {
    if (this.data.type === 'cluster') return this.data.id
    if (this.data.type === 'address') return this.data.id.substring(0, 8)
    return ''
  }
  getTag () {
    if (this.data.notes) {
      return this.data.notes
    }
    if (this.data.tags && this.data.tags.length > 1) {
      return this.data.tags.length + ' tags'
    }
    return this.findTag('tag') || ''
  }
  getActorCategory () {
    return this.findTag('actorCategory') || ''
  }
  findTag (field) {
    let tags = (this.data || {}).tags || []
    tags.sort((a, b) => {
      return a.timestamp - b.timestamp
    })
    for (let i = 0; i < tags.length; i++) {
      if (tags[i][field]) return tags[i][field]
    }
  }
  getLabel () {
    switch (this.labelType) {
      case 'noAddresses':
        return this.data.noAddresses
      case 'id':
        return this.getName()
      case 'balance':
        return this.formatCurrency(this.data.totalReceived[this.currency] - this.data.totalSpent[this.currency], this.data.keyspace)
      case 'tag':
        return this.getTag() || this.getName()
      case 'actorCategory':
        return this.getActorCategory()
    }
  }
  coloring () {
    let color
    switch (this.labelType) {
      case 'noAddresses':
        color = this.colors.range(this.data.noAddresses)
        break
      case 'tag':
        let tag
        if (this.data.notes) {
          tag = '__'
        } else if (!this.data.tags || this.data.tags.length === 0) {
          tag = ''
        } else if (this.data.tags.length > 1) {
          tag = '_'
        } else {
          tag = this.getTag(this.data)
        }
        color = this.colors.tags(tag)
        break
      case 'id':
      case 'actorCategory':
        color = this.colors.categories(this.getActorCategory())
        break
    }
    this.root
      .select('.rect')
      .style('color', color)
    this.root
      .selectAll('.expandHandle path')
      .style('color', color)
  }
  formatCurrency (value) {
    return formatCurrency(value, this.currency)
  }
  select () {
    if (this.selected) return
    this.selected = true
    if (this.shouldUpdate() === 'label') {
      this.shouldUpdate('select+label')
    } else if (!this.shouldUpdate()) {
      this.shouldUpdate('select')
    }
  }
  deselect () {
    if (!this.selected) return
    this.selected = false
    if (this.shouldUpdate() === 'label') {
      this.shouldUpdate('select+label')
    } else if (!this.shouldUpdate()) {
      this.shouldUpdate('select')
    }
  }
  shouldUpdateLabel () {
    if (this.shouldUpdate() === 'select') {
      this.shouldUpdate('select+label')
    } else if (!this.shouldUpdate()) {
      this.shouldUpdate('label')
    }
  }
  setCurrency (currency) {
    this.currency = currency
    this.shouldUpdate('label')
  }
  getOutDegree () {
    return this.data.outDegree
  }
  getInDegree () {
    return this.data.inDegree
  }
}

export {GraphNode, addressWidth, addressHeight, padding, clusterWidth, expandHandleWidth}
