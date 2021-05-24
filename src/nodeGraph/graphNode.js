import { t } from '../lang.js'
import { formatCurrency } from '../utils'
import { map } from 'd3-collection'
import { event } from 'd3-selection'
import Component from '../component.js'
import Logger from '../logger.js'
import numeral from 'numeral'
import { entityWidth, expandHandleWidth } from '../globals.js'

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
    this.searchingNeighborsIn = false
    this.searchingNeighborsOut = false
    this.entityDash = '4 1'
  }

  expandableNeighbors (isOutgoing) {
    return this.getDegree(isOutgoing) < noExpandableNeighbors
  }

  expandCollapseNeighborsOrShowTable (isOutgoing) {
    if (this.expandableNeighbors(isOutgoing)) {
      const limit = this.getDegree(isOutgoing)
      this.dispatcher('loadEgonet', { id: this.id, isOutgoing, type: this.data.type, limit, keyspace: this.data.keyspace })
    } else {
      this.dispatcher('initNeighborsTableWithNode', { id: this.id, isOutgoing, type: this.data.type })
    }
  }

  menu (subClassItems = []) {
    return subClassItems.concat([
      {
        title: t('Annotate'),
        action: () => {
          this.dispatcher('noteDialog', { x: event.x - 50, y: event.y - 50, nodeId: this.id, nodeType: this.data.type })
        },
        position: 90
      },
      {
        title: t('Remove'),
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
      0, // formerly dx
      0, // formerly dy
      this.color
    ]
  }

  deserialize ([x, y, dx, dy, color]) {
    this.x = x + dx
    this.y = y + dy
    this.color = color
  }

  renderLabel (root) {
    if (this.data.mockup) return
    let label = this.getLabel()
    let size
    let dy = 0
    const maxLetters = this.numLetters * 2
    const resizeFactor = 1.3
    if (label.length > this.numLetters * 4) {
      label = label.substring(0, this.numLetters * 4)
    }
    if (label.length > this.numLetters) {
      if (label.length > maxLetters) {
        size = this.labelHeight * 0.5 * resizeFactor
        label = label.split(' ')
        label = label.reduce((words, word) => {
          const l = words.length - 1
          const lwl = l >= 0 ? words[l].length : 0
          const space = maxLetters - lwl
          if (word.length > maxLetters) {
            const first = word.substring(0, space)
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
    const t = root.append('text')
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
    const width = this.getWidth()
    const x = isOutgoing ? width : 0
    const y = 0
    const r = isOutgoing ? 0 : 180
    const a = expandHandleWidth
    const h = this.getHeight()
    const c = h - a
    const g = root.append('g')
      .classed('expandHandle', true)
      .on('click', () => {
        this.expandCollapseNeighborsOrShowTable(isOutgoing)
        event.stopPropagation()
      })
    g.append('path')
      .classed('expandHandlePath', true)
      .attr('d', `M0 0 C ${a} 0, ${a} 0, ${a} ${a} L ${a} ${c} C ${a} ${h} ${a} ${h} 0 ${h}`)
      .style('stroke-dasharray', this.data.type === 'entity' ? this.entityDash : '')
    const fontSize = expandHandleWidth * 0.8
    const fontX = (expandHandleWidth - fontSize)
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
    logger.debug('translate', x, y)
    this.x += x
    this.y += y
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

  tags () {
    if (!this.data || !this.data.tags) return []
    if (this.data.type === 'entity') return this.data.tags.entity_tags
    return this.data.tags
  }

  getTag () {
    if (this.data.notes) {
      return this.data.notes
    }
    const tags = this.tags()
    const grouped = {}
    tags.forEach(tag => {
      if (!tag.label) return
      grouped[tag.label] = (grouped[tag.label] || 0) + 1
    })
    const entries = Object.entries(grouped)
    if (entries.length < 2) return (entries[0] && entries[0][0])
    if (entries.length < 3) return (entries[0] && entries[0][0]) + (entries[1] ? ' ' + entries[1][0] : '')
    return entries.length + ' ' + t('tags')
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
      const tag = this.getNote() || this.getTag()
      const category = this.getActorCategory()
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
    const tags = this.tags()
    if (tags.length === 0) {
      tag = ''
    } else {
      tag = this.getActorCategory() || ''
      if (!tag && tags.length > 1) {
        tag = ''
      }
    }
    const color = this.color || this.colors.categories(tag)
    logger.debug('category color', this.data, tag, color)
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

  setColoring (color) {
    this.color = color
    this.setUpdate(true)
  }
}

export { GraphNode, addressWidth, addressHeight, padding, entityWidth, expandHandleWidth }
