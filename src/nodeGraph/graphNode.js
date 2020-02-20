import {formatCurrency} from '../utils'
import {map} from 'd3-collection'
import {event} from 'd3-selection'
import Component from '../component.js'
import Logger from '../logger.js'
import numeral from 'numeral'
import {entityWidth, expandHandleWidth} from '../globals.js'

const logger = Logger.create('GraphNode') // eslint-disable-line no-unused-vars

const padding = 10
const addressWidth = entityWidth - 2 * padding - 2 * expandHandleWidth
const addressHeight = 50
const noExpandableNeighbors = 25

class GraphNode extends Component {
  constructor (dispatcher, labelType, data, layerId, colors, currency) {
    super()
    this.data = data
    this.id = [this.data.id, layerId, this.data.keyspace]
    this.labelType = labelType
    this.dispatcher = dispatcher
    this.labelHeight = 20
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
          this.dispatcher('noteDialog', {x: event.x - 50, y: event.y - 50, nodeId: this.id, nodeType: this.data.type})
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
    let dy = 0
    let maxLetters = this.numLetters * 2
    let resizeFactor = 1.3
    if (label.length > this.numLetters * 4) {
      label = label.substring(0, this.numLetters * 4)
    }
    if (label.length > this.numLetters) {
      if (label.length > maxLetters) {
        size = this.labelHeight * 0.5 * resizeFactor
        label = label.split(' ')
        label = label.reduce((words, word) => {
          let l = words.length - 1
          let lwl = l >= 0 ? words[l].length : 0
          let space = maxLetters - lwl
          if (word.length > maxLetters) {
            let first = word.substring(0, space)
            let rest = word.substring(space)
            rest = rest.match(new RegExp('.{1,' + maxLetters + '}', 'g'))
            words[l] += ' ' + first
            return words.concat(rest)
          }
          if (word.length > space) {
            return words.concat([word])
          }
          words[l] += ' ' + word
          return words
        }, [''])
        dy = -3
      } else {
        size = this.labelHeight * this.numLetters / label.length * resizeFactor
        dy = -3 * ((label.length - this.numLetters) / this.numLetters)
      }
    } else {
      size = this.labelHeight
    }
    if (!Array.isArray(label)) label = [label]
    root.node().innerHTML = ''

    dy -= (label.length - 1) * (this.labelHeight / 2.5)
    let t = root.append('text')
      .style('font-size', size + 'px')
      .attr('transform', `translate(0, ${dy})`)

    label.forEach((row, i) => {
      t.append('tspan')
        .attr('x', 0)
        .attr('dy', ((i > 0) * 1.2) + 'em')
        .text(row)
    })
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
    if (this.data.type === 'entity') return this.data.id
    if (this.data.type === 'address') return this.data.id.substring(0, 8)
    return ''
  }
  getTag () {
    if (this.data.notes) {
      return this.data.notes
    }
    let tags = (this.data || {}).tags || []
    let grouped = {}
    tags.forEach(tag => {
      if (!tag.label) return
      grouped[tag.label] = (grouped[tag.label] || 0) + 1
    })
    let entries = Object.entries(grouped)
    if (entries.length < 2) return entries[0] && entries[0][0]
    return entries.length + ' tags'
  }
  getActorCategory () {
    return this.data.mainCategory
  }
  getNote () {
    return this.data.notes
  }
  getLabel () {
    if (this.type === 'entity') {
      let label = ''
      let tag = this.getNote() || this.getTag()
      let category = this.getActorCategory()
      if (tag) {
        label = tag
      }
      if (category) {
        if (tag) {
          label += ' (' + category + ')'
        } else {
          label = category
        }
      }
      if (!label) {
        return this.getName()
      }
      return label
    }
    switch (this.labelType) {
      case 'no_addresses':
        return this.data.no_addresses
      case 'id':
        return this.getNote() || this.getName()
      case 'balance':
        return this.formatCurrency(this.data.balance[this.currency], this.data.keyspace)
      case 'tag':
        return this.getNote() || this.getTag() || this.getName()
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
        tag = ''
      }
    }
    let color = this.colors.categories(tag)
    this.root
      .select('.addressNodeRect,.entityNodeRect')
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
    return this.data.out_degree
  }
  getInDegree () {
    return this.data.in_degree
  }
}

export {GraphNode, addressWidth, addressHeight, padding, entityWidth, expandHandleWidth}
