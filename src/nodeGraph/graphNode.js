import {formatCurrency} from '../utils'
import {map} from 'd3-collection'
import {event} from 'd3-selection'
import Component from '../component.js'
import Logger from '../logger.js'
import numeral from 'numeral'
import {clusterWidth, expandHandleWidth, moreThan1TagCategory} from '../globals.js'

const logger = Logger.create('GraphNode') // eslint-disable-line no-unused-vars

const padding = 10
const addressWidth = clusterWidth - 2 * padding - 2 * expandHandleWidth
const addressHeight = 50
const noExpandableNeighbors = 25

class GraphNode extends Component {
  constructor (dispatcher, labelType, data, layerId, colors, currency) {
    super()
    this.data = data
    this.id = [this.data.id, layerId, this.data.keyspace]
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
    this.dx = 0
    this.dy = 0
    this.ddx = 0
    this.ddy = 0
    this.searchingNeighborsIn = false
    this.searchingNeighborsOut = false
  }
  expandableNeighbors (isOutgoing) {
    return this.getDegree(isOutgoing) < noExpandableNeighbors
  }
  expandCollapseNeighborsOrShowTable (isOutgoing) {
    if (this.expandableNeighbors(isOutgoing)) {
      let limit = this.getDegree(isOutgoing)
      this.dispatcher('loadEgonet', {id: this.id, isOutgoing, type: this.data.type, limit, keyspace: this.data.keyspace})
    } else {
      this.dispatcher('initNeighborsTableWithNode', {id: this.data.id, isOutgoing, type: this.data.type, keyspace: this.data.keyspace})
    }
  }
  menu (subClassItems = []) {
    return subClassItems.concat([
      {
        title: 'Add note',
        action: () => {
          this.dispatcher('noteDialog', {x: event.x - 50, y: event.y - 50, node: this})
        },
        position: 90
      },
      {
        title: 'Remove',
        action: () => {
          this.dispatcher('removeNode', [this.type, this.id])
        },
        position: 100
      }
    ]).sort((i1, i2) => i1.position - i2.position)
  }
  searchingNeighbors (isOutgoing, state) {
    if (isOutgoing) {
      this.searchingNeighborsOut = state
    } else {
      this.searchingNeighborsIn = state
    }
    logger.debug('searchingNeighbors', this.searchingNeighborsIn, this.searchingNeighborsOut)
    this.setUpdate(true)
  }
  serialize () {
    return [
      this.x,
      this.y,
      this.dx,
      this.dy
    ]
  }
  deserialize ([x, y, dx, dy]) {
    this.x = x
    this.y = y
    this.dx = this.ddx = dx
    this.dy = this.ddy = dy
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
    logger.debug('node', root.node())
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
        this.expandCollapseNeighborsOrShowTable(isOutgoing)
      })
    g.append('path')
      .classed('expandHandlePath', true)
      .attr('d', `M0 0 C ${a} 0, ${a} 0, ${a} ${a} L ${a} ${c} C ${a} ${h} ${a} ${h} 0 ${h}`)
    let fontSize = expandHandleWidth * 0.8
    let fontX = (expandHandleWidth - fontSize)
    g.append('text')
      .text(numeral(this.getDegree(isOutgoing)).format('1,000'))
      .attr('text-anchor', 'middle')
      .attr('font-size', fontSize + 'px')
      .attr('transform', `translate(${fontX}, ${h / 2}) rotate(90)`)

    g.attr('transform', `translate(${x}, ${y}) rotate(${r} 0 ${h / 2} )`)
  }
  renderSelected () {
    this.root.select('g').classed('selected', this.selected || this.highlighted)
  }
  translate (x, y) {
    this.x += x
    this.y += y
  }
  setDY (dy) {
    this.dy = this.ddy = dy
  }
  getX () {
    return this.x + this.dx
  }
  getY () {
    return this.y + this.dy
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
    this.setUpdate('label')
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
    return this.findTag('label') || ''
  }
  getActorCategory () {
    let tags = (this.data || {}).tags || []
    let categories = {}
    tags.forEach(tag => {
      categories[tag['category']] = (categories[tag['category']] || 0) + 1
    })
    let entry = Object.entries(categories).sort(([_, v1], [__, v2]) => v1 - v2)[0]
    logger.debug('categories', categories, entry)
    if (entry) return entry[0]
  }
  getNote () {
    return this.data.notes
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
        return this.getNote() || this.getName()
      case 'balance':
        return this.formatCurrency(this.data.balance[this.currency], this.data.keyspace)
      case 'tag':
        return this.getTag() || this.getName()
      case 'category':
        return this.getActorCategory() || this.getName()
    }
  }
  coloring () {
    let tag
    if (!this.data.tags || this.data.tags.length === 0) {
      tag = ''
    } else {
      tag = this.getActorCategory() || ''
      if (!tag && this.data.tags.length > 1) {
        tag = moreThan1TagCategory
      }
    }
    logger.debug('coloring tag', tag)
    let color = this.colors.categories(tag)
    this.root
      .select('.addressNodeRect,.clusterNodeRect')
      .style('fill', color)
      .style('stroke', 'black')
      .style('stroke-width', '1px')
    this.root
      .selectAll('.expandHandlePath')
      .style('fill', color)
      .style('stroke', 'black')
      .style('stroke-width', '1px')
  }
  formatCurrency (value) {
    return formatCurrency(value, this.currency)
  }
  select () {
    if (this.selected) return
    this.selected = true
    this.setUpdate('select')
  }
  deselect () {
    if (!this.selected) return
    this.selected = false
    this.setUpdate('select')
  }
  highlight () {
    if (this.highlighted) return
    this.highlighted = true
    this.setUpdate('select')
  }
  unhighlight () {
    if (!this.highlighted) return
    this.highlighted = false
    this.setUpdate('select')
  }
  setCurrency (currency) {
    this.currency = currency
    this.setUpdate('label')
  }
  getDegree (isOutgoing) {
    return isOutgoing ? this.getOutDegree() : this.getInDegree()
  }
  getOutDegree () {
    return this.data.outDegree
  }
  getInDegree () {
    return this.data.inDegree
  }
}

export {GraphNode, addressWidth, addressHeight, padding, clusterWidth, expandHandleWidth}
