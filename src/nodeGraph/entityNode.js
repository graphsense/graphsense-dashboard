import { t } from '../lang.js'
import { event } from 'd3-selection'
import { map } from 'd3-collection'
import { GraphNode, addressHeight, entityWidth, padding, expandHandleWidth } from './graphNode.js'
import numeral from 'numeral'
import contextMenu from 'd3-context-menu'
import Logger from '../logger.js'

const logger = Logger.create('EntityNode') // eslint-disable-line no-unused-vars

const gap = padding
const noAddressesLabelHeight = 16
const paddingBottom = 7
const noExpandableAddresses = 16

const sort = (getValue) => (n1, n2) => {
  const v1 = getValue(n1.data)
  const v2 = getValue(n2.data)
  return v1 > v2 ? 1 : (v1 < v2 ? -1 : 0)
}

export default class EntityNode extends GraphNode {
  constructor (dispatcher, entity, layerId, labelType, colors, currency) {
    super(dispatcher, labelType, entity, layerId, colors, currency)
    this.nodes = map()
    this.addressFilters = map()
    this.addressFilters.set('limit', 10)
    this.expandLimit = 10
    this.type = 'entity'
    this.numLetters = 11
    this.sortAddressesProperty = data => data.id
    this.currencyLabelHeight = Math.max(this.labelHeight - 18, 12)
  }

  sortAddresses (getValue) {
    this.sortAddressesProperty = getValue
  }

  expandable () {
    return this.data.no_addresses < noExpandableAddresses
  }

  isExpand () {
    return this.expandable() && this.nodes.size() < this.data.no_addresses
  }

  isCollapse () {
    return this.expandable() && this.nodes.size() === this.data.no_addresses
  }

  expandCollapseOrShowAddressTable () {
    if (this.isExpand()) {
      this.dispatcher('loadEntityAddresses', { id: this.id, keyspace: this.data.keyspace, limit: this.data.no_addresses })
    } else if (this.isCollapse()) {
      this.dispatcher('removeEntityAddresses', this.id)
    } else {
      this.dispatcher('initAddressesTableWithEntity', { id: this.data.id, keyspace: this.data.keyspace, type: 'entity' })
    }
  }

  expandCollapseOrShowAddressTableTitle () {
    return this.isExpand() ? t('Expand') : (this.isCollapse() ? t('Collapse') : t('Show address table'))
  }

  menu () {
    const items = []
    const searchNeighborsDialog = isOutgoing => this.dispatcher('searchNeighborsDialog', { x: event.x - 120, y: event.y - 50, id: this.id, type: this.type, isOutgoing })
    items.push(
      {
        title: () => this.expandCollapseOrShowAddressTableTitle(),
        action: () => this.expandCollapseOrShowAddressTable(),
        position: 50
      })
    if (this.nodes.size() > 1) {
      items.push({
        title: t('Sort addresses by'),
        position: 60,
        children: [
          {
            title: t('Final balance'),
            action: () => this.dispatcher('sortEntityAddresses', { entity: this.id, property: data => data.total_received.value - data.total_spent.value })
          },
          {
            title: t('Total received'),
            action: () => this.dispatcher('sortEntityAddresses', { entity: this.id, property: data => data.total_received.value })
          },
          {
            title: t('No. neighbors'),
            children: [
              {
                title: t('Incoming'),
                action: () => this.dispatcher('sortEntityAddresses', { entity: this.id, property: data => data.in_degree })
              },
              {
                title: t('Outgoing'),
                action: () => this.dispatcher('sortEntityAddresses', { entity: this.id, property: data => data.out_degree })
              }
            ]
          },
          {
            title: t('No. transactions'),
            children: [
              {
                title: t('Incoming'),
                action: () => this.dispatcher('sortEntityAddresses', { entity: this.id, property: data => data.no_incoming_txs })
              },
              {
                title: t('Outgoing'),
                action: () => this.dispatcher('sortEntityAddresses', { entity: this.id, property: data => data.no_outgoing_txs })
              }
            ]
          },
          {
            title: t('First usage'),
            action: () => this.dispatcher('sortEntityAddresses', { entity: this.id, property: data => data.first_tx.timestamp })
          },
          {
            title: t('Last usage'),
            action: () => this.dispatcher('sortEntityAddresses', { entity: this.id, property: data => data.last_tx.timestamp })
          }
        ]
      })
    }
    items.push(
      {
        title: t('Search'),
        children: [
          {
            title: t('Incoming'),
            action: () => searchNeighborsDialog(false)
          },
          {
            title: t('Outgoing'),
            action: () => searchNeighborsDialog(true)
          }
        ]

      }
    )
    return super.menu(items)
  }

  serialize () {
    const s = super.serialize()
    s.push(this.nodes.keys())
    return s
  }

  deserialize (version, [x, y, dx, dy, nodes], addressNodes) {
    super.deserialize([x, y, dx, dy])
    nodes.forEach(key => {
      if (version === '0.4.0') {
        key += ',' + this.data.keyspace
      }
      this.add(addressNodes.get(key))
    })
  }

  add (node) {
    if (!node.id) throw new Error('not a node', node)
    this.nodes.set(node.id, node)
  }

  has (address) {
    this.nodes.has([address, this.id[1]])
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (this.shouldUpdate()) {
      this.root.node().innerHTML = ''
      if (!this.data.mockup) {
        const height = this.getHeight()
        const g = this.root
          .append('g')
          .classed('entityNode', true)
          .attr('transform', `translate(${this.dx + this.ddx}, ${this.dy + this.ddy})`)
          .on('click', () => {
            event.stopPropagation()
            this.dispatcher('selectNode', ['entity', this.id])
          })
          .on('contextmenu', contextMenu(this.menu()))
          .on('mouseover', () => this.dispatcher('tooltip', 'entity'))
          .on('mouseout', () => this.dispatcher('hideTooltip'))
        g.node().addEventListener('mousedown', (e) => {
          if (e.button !== 0) return
          e.stopPropagation()
          e.preventDefault()
          this.dispatcher('dragNodeStart', { id: this.id, type: this.type, x: e.clientX, y: e.clientY })
        })
        /* .call(drag()
            .on('start', () => {
              this.dispatcher('dragNodeStart', { id: this.id, type: this.type, x: event.dx, y: event.dy })
            })
            .on('drag', () => {
              if (Math.abs(event.dx) > 10 || Math.abs(event.dy) > 10) return
              this.dispatcher('dragNode', { x: event.dx, y: event.dy })
            })
            .on('end', () => {
              this.dispatcher('dragNodeEnd')
            }))
            */
        g.append('rect')
          .classed('entityNodeRect', true)
          .attr('width', entityWidth)
          .attr('height', height)
          .style('stroke-dasharray', this.entityDash)
        const label = g.append('g')
          .classed('label', true)
          .attr('transform', `translate(${padding}, ${padding / 2 + this.labelHeight})`)
        this.renderLabel(label)
        const currency = g.append('g')
          .classed('label', true)
          .attr('transform', `translate(${this.getWidth() - padding}, ${padding / 2 + this.currencyLabelHeight})`)
        this.renderCurrency(currency)
        const eg = g.append('g').classed('expandHandles', true)
        this.renderExpand(eg, true)
        this.renderExpand(eg, false)
        this.coloring()
        this.renderSelected()
      }
    } else {
      if (this.shouldUpdate('label')) {
        const label = this.root.select('g.label')
        this.renderLabel(label)
        this.coloring()
      }
      if (this.shouldUpdate('select')) {
        this.renderSelected()
      }
    }
    super.render()
  }

  renderAddresses (root) {
    if (!this.shouldUpdate()) {
      this.nodes.each(addressNode => addressNode.render())
      return
    }
    if (root) this.addressesRoot = root
    if (!this.addressesRoot) throw new Error('root not defined')
    this.addressesRoot.node().innerHTML = ''
    let cumY = 2 * padding + this.labelHeight
    this.nodes
      .values()
      .sort(sort(this.sortAddressesProperty))
      .forEach((addressNode) => {
        const g = this.addressesRoot.append('g')
        addressNode.setUpdate(true)
        // reset absolute coords
        addressNode.x = 0
        addressNode.y = 0
        const x = padding + expandHandleWidth
        const y = cumY
        addressNode.render(g)
        addressNode.translate(x + this.dx + this.ddx, y + this.dy + this.ddy)
        g.attr('transform', `translate(${x}, ${y})`)
        cumY += addressNode.getHeight()
      })
    if (this.data.mockup) return
    const size = this.nodes.size()
    cumY += size > 0 ? gap : 0
    const button = this.addressesRoot.append('g')
      .classed('addressExpand', true)
    const h = this.getHeight()
    const w = this.getWidth()
    const num = (n) => numeral(n).format('0,000')
    const plural = this.data.no_addresses > 1 ? 'es' : ''
    const translation = (size > 0 ? 'num_of_address' : 'num_address') + plural
    button.append('text')
      .attr('text-anchor', 'middle')
      .attr('x', w / 2)
      .attr('y', h - paddingBottom)
      .attr('font-size', noAddressesLabelHeight)
      .attr('title', this.expandCollapseOrShowAddressTableTitle())
      .text(t(translation, num(this.data.no_addresses), num(size)))
      .on('click', () => {
        event.stopPropagation()
        this.dispatcher('selectNode', ['entity', this.id])
        this.expandCollapseOrShowAddressTable()
      })
    super.render()
  }

  renderCurrency (root) {
    root.append('text')
      .attr('text-anchor', 'end')
      .style('font-size', this.currencyLabelHeight + 'px')
      .text(this.data.keyspace.toUpperCase())
  }

  translate (x, y) {
    super.translate(x, y)
    this.nodes.each((node) => {
      node.translate(x, y)
    })
  }

  getHeight () {
    return this.nodes.size() * addressHeight +
      2 * padding +
      (this.data.mockup ? 0 : this.labelHeight + noAddressesLabelHeight) +
      (this.nodes.size() > 0 ? 2 * gap : gap)
  }

  getWidth () {
    return entityWidth
  }

  getId () {
    return this.data.entity
  }
}
