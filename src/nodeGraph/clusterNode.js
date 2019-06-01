import {event} from 'd3-selection'
import {map} from 'd3-collection'
import {GraphNode, addressHeight, clusterWidth, padding, expandHandleWidth} from './graphNode.js'
import numeral from 'numeral'
import contextMenu from 'd3-context-menu'
import Logger from '../logger.js'
import {drag} from 'd3-drag'

const logger = Logger.create('ClusterNode') // eslint-disable-line no-unused-vars

const gap = padding
const noAddressesLabelHeight = 16
const paddingBottom = 7
const noExpandableAddresses = 16

const sort = (getValue) => (n1, n2) => {
  let v1 = getValue(n1.data)
  let v2 = getValue(n2.data)
  return v1 > v2 ? 1 : (v1 < v2 ? -1 : 0)
}

export default class ClusterNode extends GraphNode {
  constructor (dispatcher, cluster, layerId, labelType, colors, currency) {
    super(dispatcher, labelType, cluster, layerId, colors, currency)
    this.nodes = map()
    this.addressFilters = map()
    this.addressFilters.set('limit', 10)
    this.expandLimit = 10
    this.type = 'cluster'
    this.numLetters = 11
    this.sortAddressesProperty = data => data.id
  }
  sortAddresses (getValue) {
    this.sortAddressesProperty = getValue
  }
  expandable () {
    return this.data.noAddresses < noExpandableAddresses
  }
  isExpand () {
    return this.expandable() && this.nodes.size() < this.data.noAddresses
  }
  isCollapse () {
    return this.expandable() && this.nodes.size() === this.data.noAddresses
  }
  expandCollapseOrShowAddressTable () {
    if (this.isExpand()) {
      this.dispatcher('loadClusterAddresses', {id: this.id, keyspace: this.data.keyspace, limit: this.data.noAddresses})
    } else if (this.isCollapse()) {
      this.dispatcher('removeClusterAddresses', this.id)
    } else {
      this.dispatcher('initAddressesTableWithCluster', {id: this.data.id, keyspace: this.data.keyspace, type: 'cluster'})
    }
  }
  expandCollapseOrShowAddressTableTitle () {
    return this.isExpand() ? 'Expand' : (this.isCollapse() ? 'Collapse' : 'Show address table')
  }
  menu () {
    let items = []
    items.push(
      { title: () => this.expandCollapseOrShowAddressTableTitle(),
        action: () => this.expandCollapseOrShowAddressTable(),
        position: 50
      })
    if (this.nodes.size() > 1) {
      items.push({ title: 'Sort addresses by',
        position: 60,
        children: [
          { title: 'Final balance',
            action: () => this.dispatcher('sortClusterAddresses', {cluster: this.id, property: data => data.totalReceived.satoshi - data.totalSpent.satoshi})
          },
          { title: 'Total received',
            action: () => this.dispatcher('sortClusterAddresses', {cluster: this.id, property: data => data.totalReceived.satoshi})
          },
          { title: 'No. neighbors',
            children: [
              { title: 'Incoming',
                action: () => this.dispatcher('sortClusterAddresses', {cluster: this.id, property: data => data.inDegree})
              },
              { title: 'Outgoing',
                action: () => this.dispatcher('sortClusterAddresses', {cluster: this.id, property: data => data.outDegree})
              }
            ]
          },
          { title: 'No. transactions',
            children: [
              { title: 'Incoming',
                action: () => this.dispatcher('sortClusterAddresses', {cluster: this.id, property: data => data.noIncomingTxs})
              },
              { title: 'Outgoing',
                action: () => this.dispatcher('sortClusterAddresses', {cluster: this.id, property: data => data.noOutgoingTxs})
              }
            ]
          },
          { title: 'First usage',
            action: () => this.dispatcher('sortClusterAddresses', {cluster: this.id, property: data => data.firstTx.timestamp})
          },
          { title: 'Last usage',
            action: () => this.dispatcher('sortClusterAddresses', {cluster: this.id, property: data => data.lastTx.timestamp})
          }
        ]
      })
    }
    return super.menu(items)
  }
  serialize () {
    let s = super.serialize()
    s.push(this.nodes.keys())
    return s
  }
  deserialize (version, [x, y, nodes], addressNodes) {
    super.deserialize([x, y])
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
    if (this.shouldUpdate(true)) {
      this.root.node().innerHTML = ''
      if (!this.data.mockup) {
        let height = this.getHeight()
        let g = this.root
          .append('g')
          .classed('clusterNode', true)
          .on('click', () => {
            event.stopPropagation()
            this.dispatcher('selectNode', ['cluster', this.id])
          })
          .on('contextmenu', contextMenu(this.menu()))
          .call(drag()
            .on('drag', () => {
              if (Math.abs(event.dx) > 10 || Math.abs(event.dy) > 10) return
              this.dispatcher('dragNode', {id: this.id, type: 'cluster', dx: event.dx, dy: event.dy})
            }))
        g.append('rect')
          .classed('rect', true)
          .attr('width', clusterWidth)
          .attr('height', height)
        let label = g.append('g')
          .classed('label', true)
          .attr('transform', `translate(${padding}, ${padding / 2 + this.labelHeight})`)
        this.renderLabel(label)
        let eg = g.append('g').classed('expandHandles', true)
        this.renderExpand(eg, true)
        this.renderExpand(eg, false)
        this.coloring()
        this.renderSelected()
      }
    } else {
      if (this.shouldUpdate('label')) {
        let label = this.root.select('g.label')
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
    root.node().innerHTML = ''
    let cumY = 2 * padding + this.labelHeight
    this.nodes
      .values()
      .sort(sort(this.sortAddressesProperty))
      .forEach((addressNode) => {
        let g = root.append('g')
        addressNode.setUpdate(true)
        // reset absolute coords
        addressNode.x = 0
        addressNode.y = 0
        let x = padding + expandHandleWidth
        let y = cumY
        addressNode.render(g)
        addressNode.translate(x + this.dx, y + this.dy)
        g.attr('transform', `translate(${x}, ${y})`)
        cumY += addressNode.getHeight()
      })
    if (this.data.mockup) return
    let size = this.nodes.size()
    cumY += size > 0 ? gap : 0
    let button = root.append('g')
      .classed('addressExpand', true)
    let h = this.getHeight()
    let w = this.getWidth()
    let num = (n) => numeral(n).format('0,000')
    button.append('text')
      .attr('text-anchor', 'middle')
      .attr('x', w / 2)
      .attr('y', h - paddingBottom)
      .attr('font-size', noAddressesLabelHeight)
      .attr('title', this.expandCollapseOrShowAddressTableTitle())
      .text((size > 0 ? num(size) + '/' : '') + num(this.data.noAddresses) + ' addresses')
      .on('click', () => {
        event.stopPropagation()
        this.expandCollapseOrShowAddressTable()
      })
    super.render()
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
    return clusterWidth
  }
  getId () {
    return this.data.cluster
  }
}
