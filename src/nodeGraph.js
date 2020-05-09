import { t } from './lang.js'
import { easeCubicOut } from 'd3-ease'
import { create, event } from 'd3-selection'
import { scalePow, scaleOrdinal } from 'd3-scale'
import { set, map } from 'd3-collection'
import { schemeSet3 } from 'd3-scale-chromatic'
import { hsl } from 'd3-color'
import { linkHorizontal } from 'd3-shape'
import Layer from './nodeGraph/layer.js'
import EntityNode from './nodeGraph/entityNode.js'
import AddressNode from './nodeGraph/addressNode.js'
import Component from './component.js'
import { formatCurrency, nodesIdentical } from './utils'
import Logger from './logger.js'
import { entityWidth, expandHandleWidth } from './globals.js'

const logger = Logger.create('NodeGraph') // eslint-disable-line no-unused-vars

const margin = 300

const zoomSlowity = 5

const zoomDuration = 1000

const hsl2rgb = (h, s, l) => {
  h = h % 360
  s = s * 100 + '%'
  l = l * 100 + '%'
  return `hsl(${h}, ${s}, ${l})`
}

const lightnessFactor = {
  entity: 1,
  address: 0.83
}
const defaultColor = {
  entity: hsl2rgb(178, 0, 0.95),
  address: hsl2rgb(178, 0, 0.90)
}

const transactionsPixelRange = [1, 7]

const colorScale = scaleOrdinal(schemeSet3)

const maxNumSnapshots = 10

const getOutgoing = (n1, n2) => n1.keyspace === n2.keyspace && n1.outgoing.get(n2.id)

export default class NodeGraph extends Component {
  constructor (dispatcher, labelType, currency, txLabelType) {
    super()
    this.currency = currency
    this.dispatcher = dispatcher
    this.labelType = labelType
    this.txLabelType = txLabelType
    // nodes of the graph
    this.entityNodes = map()
    this.addressNodes = map()
    // ids of addresses/entities present in graph
    this.references = { address: map(), entity: map() }
    this.adding = set()
    this.selectedNode = null
    this.highlightedNodes = []
    this.selectedLink = null
    this.layers = []
    this.colorMapCategories = map()
    this.colorMapTags = map()
    const colorGen = (map, type) => (k) => {
      if (!k) return defaultColor[type]
      let color = map.get(k)
      if (color === undefined) {
        color = colorScale(map.size())
        map.set(k, color.toString())
      }
      const c = hsl(color)
      c.l = c.l * lightnessFactor[type]
      return c
    }
    this.colors =
      {
        entity: {
          categories: colorGen(this.colorMapCategories, 'entity'),
          tags: colorGen(this.colorMapTags, 'entity'),
          range: (v) => defaultColor.entity
        },
        address: {
          categories: colorGen(this.colorMapCategories, 'address'),
          tags: colorGen(this.colorMapTags, 'address'),
          range: (v) => defaultColor.address
        }
      }
    this.linker = linkHorizontal()
      .x(([node, isSource, scale]) => isSource ? node.getXForLinks() + node.getWidthForLinks() : node.getXForLinks() - this.arrowSummit)
      .y(([node, isSource, scale]) => node.getYForLinks() + node.getHeightForLinks() / 2)
    this.shadowLinker = linkHorizontal()
      .x(([node, isSource]) => isSource ? node.getXForLinks() + node.getWidthForLinks() : node.getXForLinks())
      .y(([node, isSource]) => node.getYForLinks() + node.getHeightForLinks() / 2)
    this.snapshots = []

    this.currentSnapshotIndex = -1
    // initialize with true to allow initial snapshot
    this.dirty = true
    this.createSnapshot()
    this.w = window.innerWidth
    this.h = window.innerHeight
    this.k = 1
    this.x = 0
    this.dx = 0
    this.y = 0
    this.dy = 0
    this.dk = 0

    window.addEventListener('resize', () => {
      this.w = window.innerWidth
      this.h = window.innerHeight
      this.renderViewBox()
    })
  }

  setCategoryColors (cc) {
    if (cc === null || Array.isArray(cc) || typeof cc !== 'object') return
    for (const category in cc) {
      logger.debug('category', category, cc[category])
      const color = cc[category]
      this.colorMapCategories.set(category, color)
    }
    this.setUpdate('layers')
  }

  addCategories (categories) {
    if (!Array.isArray(categories)) return
    const s = this.colorMapCategories.size()
    categories.forEach((category, i) => {
      if (this.colorMapCategories.has(category)) return
      const c = hsl(colorScale(s + i))
      c.s -= 0.1
      this.colorMapCategories.set(category, c.toString())
    })
    this.setUpdate('layers')
  }

  setNodes (node, type) {
    let nodes
    if (type === 'entity') {
      nodes = this.entityNodes
    } else if (type === 'address') {
      nodes = this.addressNodes
    }
    nodes.set(node.id, node)
    const id = [node.id[0], node.id[2]]
    const refCount = this.references[type].get(id) || 0
    this.references[type].set(id, refCount + 1)
  }

  removeFromNodes (nodeId, type) {
    let nodes
    if (type === 'entity') {
      nodes = this.entityNodes
    } else if (type === 'address') {
      nodes = this.addressNodes
    }
    nodes.remove(nodeId)
    const id = [nodeId[0], nodeId[2]]
    const refCount = this.references[type].get(id)
    if (refCount === 1) {
      this.references[type].remove(id)
    } else {
      this.references[type].set(id, refCount - 1)
    }
  }

  setAddressNodes (node) {
    this.setNodes(node, 'address')
  }

  setEntityNodes (node) {
    this.setNodes(node, 'entity')
  }

  removeAddressNode (nodeId) {
    this.removeFromNodes(nodeId, 'address')
  }

  removeEntityNode (nodeId) {
    this.removeFromNodes(nodeId, 'entity')
  }

  clearNodes (type) {
    let nodes
    if (type === 'entity') {
      nodes = this.entityNodes
    } else if (type === 'address') {
      nodes = this.addressNodes
    }
    nodes.clear()
    this.references[type].clear()
  }

  setUpdateNodes (type, id, update) {
    let nodes = null
    if (type === 'address') {
      nodes = this.addressNodes
    }
    if (type === 'entity') {
      nodes = this.entityNodes
    }
    if (!nodes) return
    nodes.each((node) => { if (node.id[0] == id) node.setUpdate(update) }) // eslint-disable-line eqeqeq
  }

  getNodeChecker () {
    return (id, type, keyspace) => this.references[type].has([id, keyspace])
  }

  getCategoryColors () {
    return this.colorMapCategories
  }

  createSnapshot () {
    // don't create snapshot if nothing has changed
    if (!this.dirty) return
    // don't create snapshot if there are more nodes to come
    if (!this.adding.empty()) return
    this.snapshots = this.snapshots.slice(0, this.currentSnapshotIndex + 1)
    this.snapshots.push(this.serializeGraph())
    this.dirty = false
    if (this.snapshots.length > maxNumSnapshots) {
      this.snapshots.shift()
      return
    }
    this.currentSnapshotIndex++
  }

  loadNextSnapshot (store) {
    const s = this.snapshots[this.currentSnapshotIndex + 1]
    if (!s) return
    this.currentSnapshotIndex++
    this.loadSnapshot(store, s)
  }

  loadPreviousSnapshot (store) {
    const s = this.snapshots[this.currentSnapshotIndex - 1]
    if (!s) return
    this.currentSnapshotIndex--
    this.loadSnapshot(store, s)
  }

  loadSnapshot (store, s) {
    this.clearNodes('address')
    this.clearNodes('entity')
    this.layers = []
    this.deserializeGraph(null, store, s[0], s[1], s[2])
    this.setUpdate('layers')
  }

  thereAreMorePreviousSnapshots () {
    return !!this.snapshots[this.currentSnapshotIndex - 1]
  }

  thereAreMoreNextSnapshots () {
    return !!this.snapshots[this.currentSnapshotIndex + 1]
  }

  getNode (id, type) {
    let nodes
    if (type === 'address') {
      nodes = this.addressNodes
    } else if (type === 'entity') {
      nodes = this.entityNodes
    }
    return nodes.get(id)
  }

  searchingNeighbors (id, type, isOutgoing, state) {
    const node = this.getNode(id, type)
    if (!node) return
    node.searchingNeighbors(isOutgoing, state)
  }

  dragNodeStart (id, type, x, y) {
    const entity = this.entityNodes.get(id)
    if (!entity) return
    this.draggingNode = entity
  }

  dragNode (dx, dy) {
    if (!this.draggingNode) return

    const entity = this.draggingNode

    const ddx = entity.dx + dx
    const ddy = entity.dy + dy

    if (ddx - 2 * expandHandleWidth < margin / -2) return
    if (ddx + 2 * expandHandleWidth > margin / 2) return

    const layer = this.findLayer(this.draggingNode.id[1])
    if (!layer) return

    const nodes = layer.nodes.values()
    const x = entity.x + ddx - expandHandleWidth
    const y = entity.y + ddy
    logger.debug('y', y, dy)
    const cw = entity.getWidthForLinks()
    const ch = entity.getHeightForLinks()
    for (let i = 0; i < nodes.length; i++) {
      const sister = nodes[i]
      if (sister === entity) continue
      const sx = sister.getXForLinks()
      const sy = sister.getYForLinks()
      const sw = sister.getWidthForLinks()
      const sh = sister.getHeightForLinks()
      if (((x + cw >= sx && x + cw <= sx + sw) ||
          (x >= sx && x <= sx + sw)
      ) &&
          ((y + ch >= sy && y + ch <= sy + sh) ||
          (y >= sy && y <= sy + sh)
          )
      ) return
    }

    entity.dx = ddx
    entity.dy = ddy

    entity.setUpdate('position')
    this.setUpdate('link', this.draggingNode.id)
  }

  dragNodeEnd () {
    const entity = this.draggingNode
    if (!entity) return
    this.draggingNode = null
    this.dirty = true
  }

  sortEntityAddresses (id, property) {
    const entity = this.entityNodes.get(id)
    logger.debug('sort addresses entity', entity)
    if (!entity) return
    entity.sortAddresses(property)
    this.setUpdate('layers')
  }

  deselect () {
    logger.debug('deselect', this.selectedNode)
    if (!this.selectedNode) return
    this.selectedNode.deselect()
    this.highlightedNodes.forEach(node => node.unhighlight())
    this.highlightedNodes = []
    this.selectedNode.setUpdate('select')
    this.selectedNode = null
  }

  deselectLink () {
    if (!this.selectedLink) return
    this.setUpdate('link', this.selectedLink[0])
    this.selectedLink = null
  }

  selectNodeWhenLoaded ([id, type, keyspace]) {
    this.nextSelectedNode = { id, type, keyspace }
  }

  selectNodeIfIsNextNode (node) {
    logger.debug('selectNodeIfIsNextNode', node, this.nextSelectedNode)
    if (!this.nextSelectedNode) return
    if (this.nextSelectedNode.type !== node.data.type) return
    if (this.nextSelectedNode.keyspace !== node.data.keyspace) return
    if (this.nextSelectedNode.id != node.data.id) return // eslint-disable-line eqeqeq
    this._selectNode(node)
    this.nextSelectedNode = null
  }

  zoomToHighlightedNodes () {
    logger.debug('zoom highlighted', this.highlightedNodes)
    if (this.highlightedNodes.length === 0) return
    this.x += this.dx
    this.y += this.dy
    this.k += this.dk

    let x1 = Infinity
    let y1 = Infinity
    let x2 = -Infinity
    let y2 = -Infinity
    this.highlightedNodes.forEach(node => {
      x1 = Math.min(x1, node.getXForLinks())
      y1 = Math.min(y1, node.getYForLinks())
      x2 = Math.max(x2, node.getXForLinks() + node.getWidthForLinks())
      y2 = Math.max(y2, node.getYForLinks() + node.getHeightForLinks())
    })
    const dx = (x2 - x1)
    const dy = (y2 - y1)
    // padding
    x1 -= dx * 0.3
    x2 += dx * 0
    y1 -= dy * 0.3
    y2 += dy * 0
    logger.debug('zoom bbox', x1, x2, y1, y2)
    let k = this.k
    if (x2 - x1 > this.w) {
      k = this.w / (x2 - x1)
    }
    if (y2 - y1 > this.h) {
      const k2 = this.h / (y2 - y1)
      k = Math.min(k, k2)
    }
    logger.debug('zoom k', k)
    const x = (x2 + x1) / 2
    const y = (y2 + y1) / 2
    const now = performance.now() // eslint-disable-line no-undef
    this.zoomTarget = {
      x, y, k, started: now
    }
    logger.debug('zoomTarget', this.zoomTarget)
    window.requestAnimationFrame(t => this.zoom(t))
  }

  zoom (time) {
    if (!this.zoomTarget) return
    const d = (time - this.zoomTarget.started) / zoomDuration
    const completed = easeCubicOut(d)
    logger.debug('zoom', this.x, this.y, this.zoomTarget.x, this.zoomTarget.y, completed)
    this.dx = (this.zoomTarget.x - this.x) * completed
    this.dy = (this.zoomTarget.y - this.y) * completed
    this.dk = (this.k - this.zoomTarget.k) * d
    this.renderViewBox()
    if (completed < 1) {
      window.requestAnimationFrame(t => this.zoom(t))
    } else {
      this.zoomTarget = null
      this.x += this.dx
      this.y += this.dy
      this.k += this.dk
      this.dx = this.dy = this.dk = 0
    }
  }

  setTxLabel (type) {
    this.txLabelType = type
    this.setUpdate('links')
  }

  setCurrency (currency) {
    this.currency = currency
    this.addressNodes.each(node => node.setCurrency(currency))
    this.entityNodes.each(node => node.setCurrency(currency))
    if (this.txLabelType === 'estimated_value') {
      this.setUpdate('links')
    }
  }

  setEntityLabel (labelType) {
    this.labelType.entityLabel = labelType
    this.entityNodes.each((node) => {
      node.setLabelType(labelType)
    })
  }

  setAddressLabel (labelType) {
    this.labelType.addressLabel = labelType
    this.addressNodes.each((node) => {
      node.setLabelType(labelType)
    })
  }

  selectNode (type, nodeId, multi) {
    const sel = this.getNode(nodeId, type)
    if (!sel) return
    this._selectNode(sel, multi)
    this.deselectLink()
  }

  _selectNode (sel, multi) {
    // deselect single
    const identical = (node1, node2) => nodesIdentical(node1.data, node2.data)
    if (multi) {
      if (this.selectedNode && identical(sel, this.selectedNode)) {
        this.highlightedNodes.forEach(node => {
          if (identical(node, sel)) {
            node.unhighlight()
            node.deselect()
          }
        })
        this.highlightedNodes = this.highlightedNodes.filter(node => !identical(node, sel))
        this.selectedNode = null
        if (this.highlightedNodes.length > 0) {
          this.selectedNode = this.highlightedNodes[0]
          this.selectedNode.select()
        }
        return
      }
      if (this.highlightedNodes.indexOf(sel) !== -1) {
        // don't unhighlight a highlighted node of same id with selected one
        if (identical(this.selectedNode, sel)) return
        this.highlightedNodes = this.highlightedNodes.filter(node => {
          if (identical(node, sel)) {
            node.unhighlight()
            return false
          }
          return true
        })
        return
      }
    }

    // select
    if (this.selectedNode) {
      this.selectedNode.deselect()
    }
    if (!multi) {
      this.highlightedNodes.forEach(node => node.unhighlight())
      this.highlightedNodes = []
    }
    let nodes
    if (sel.data.type === 'entity') {
      nodes = this.entityNodes
    } else if (sel.data.type === 'address') {
      nodes = this.addressNodes
    }
    nodes.each(node => {
      if (identical(node, sel)) {
        node.highlight()
        this.highlightedNodes.push(node)
      }
    })
    sel.select()
    this.selectedNode = sel
    this.zoomToHighlightedNodes()
  }

  selectLink (source, target) {
    this.deselectLink()
    this.selectedLink = [source.id, target.id]
    this.setUpdate('link', source.id)
    this.deselect()
    logger.debug('setupate', this.update)
  }

  isSelectedLink (source, target) {
    return JSON.stringify([source.id, target.id]) == JSON.stringify(this.selectedLink) // eslint-disable-line eqeqeq
  }

  setResultEntityAddresses (id, addresses) {
    const entity = this.entityNodes.get(id)
    addresses.forEach((address) => {
      if (this.addressNodes.has([address.id, id[1], address.keyspace])) return
      const addressNode = new AddressNode(this.dispatcher, address, id[1], this.labelType.addressLabel, this.colors.address, this.currency)
      logger.debug('new AddressNode', addressNode)
      this.setAddressNodes(addressNode)
      entity.add(addressNode)
      this.dirty = true
    })
    this.setUpdate('layers')
  }

  findAddressNode (address, layerId) {
    return this.addressNodes.get([address, layerId])
  }

  add (object, anchor) {
    logger.debug('add', object, anchor)
    this.adding.remove(object.id)
    let layerIds
    if (!anchor) {
      layerIds = this.additionLayerBySelection(object.id)
      if (layerIds === false) layerIds = this.additionLayerBySearch(object)
      layerIds = layerIds || 0
    } else if (anchor.nodeType === 'entity' && object.type === 'address') {
      layerIds = anchor.nodeId[1]
    } else {
      layerIds = anchor.nodeId[1] * 1 + (anchor.isOutgoing ? 1 : -1)
    }
    if (!Array.isArray(layerIds)) {
      layerIds = [layerIds]
    }
    logger.debug('layerIds', layerIds)
    let node
    layerIds.forEach(layerId => {
      node = this.addLayer(layerId, object, anchor)
    })
    this.setUpdate('layers')
    return node
  }

  findLayer (layerId) {
    return this.layers.filter(({ id }) => id == layerId)[0] // eslint-disable-line eqeqeq
  }

  addLayer (layerId, object, anchor) {
    let layer = this.findLayer(layerId)
    if (!layer) {
      layer = new Layer(layerId)
      if (anchor && anchor.isOutgoing === false) {
        this.layers.unshift(layer)
      } else {
        if (layerId >= 0) {
          this.layers.push(layer)
        } else {
          this.layers.unshift(layer)
        }
      }
    }
    let node
    if (object.type === 'address') {
      let addressNode = this.addressNodes.get([object.id, layerId, object.keyspace])
      if (addressNode) {
        this.selectNodeIfIsNextNode(addressNode)
        return node
      }
      addressNode = new AddressNode(this.dispatcher, object, layerId, this.labelType.addressLabel, this.colors.address, this.currency)
      this.setAddressNodes(addressNode)
      this.selectNodeIfIsNextNode(addressNode)
      logger.debug('new AddressNode', addressNode)
      node = this.entityNodes.get([object.entity.id, layerId, object.entity.keyspace])
      if (!node) {
        node = new EntityNode(this.dispatcher, object.entity, layerId, this.labelType.entityLabel, this.colors.entity, this.currency)
      }
      node.add(addressNode)
      this.setEntityNodes(node)
    } else if (object.type === 'entity') {
      node = this.entityNodes.get([object.id, layerId, object.keyspace])
      if (node) {
        this.selectNodeIfIsNextNode(node)
        return node
      }
      node = new EntityNode(this.dispatcher, object, layerId, this.labelType.entityLabel, this.colors.entity, this.currency)
      this.setEntityNodes(node)
      logger.debug('new EntityNode', node)
      this.selectNodeIfIsNextNode(node)
    } else {
      throw Error('unknown node type')
    }
    this.dirty = true

    const anchorLayer = anchor && this.findLayer(anchor.nodeId[1])

    const addToTop = anchorLayer && anchorLayer.isNodeInUpperHalf(anchor.nodeId)

    layer.add(node, addToTop)
    return node
  }

  remove (nodeType, nodeId) {
    logger.debug('remove', nodeType, nodeId)
    const node = this.getNode(nodeId, nodeType)
    if (nodeType === 'address') {
      this.removeAddressNode(nodeId)
    } else if (nodeType === 'entity') {
      this.removeEntityNode(nodeId)
    }
    if (this.selectedNode === node) {
      this.selectedNode = null
    }
    const layer = this.findLayer(nodeId[1])
    logger.debug('remove layer', nodeId, layer)
    if (!layer) return
    if (nodeType === 'address') {
      this.entityNodes.remove('mockup' + nodeId)
      layer.nodes.each(entity => {
        entity.nodes.remove(nodeId)
      })
    } else if (nodeType === 'entity') {
      node.nodes.each(node => this.removeAddressNode(node.id))
      this.entityNodes.remove(nodeId)
      if (layer.nodes.size() === 0) {
        this.layers = this.layers.filter(l => l !== layer)
      } else {
        layer.remove(nodeId)
      }
    }
    this.setUpdate('layers')
  }

  removeEntityAddresses (id) {
    const entity = this.entityNodes.get(id)
    if (!entity) return
    entity.nodes.each((address) => this.remove('address', address.id))
  }

  additionLayerBySelection (addressId) {
    if (!addressId) return false
    if (!(this.selectedNode instanceof EntityNode)) return false
    const entity = this.selectedNode.data
    if (!entity.addresses.has(addressId)) return false
    return this.selectedNode.id[1]
  }

  additionLayerBySearch (node) {
    logger.debug('search', node)
    const ids = set()
    if (this.selectedNode && getOutgoing(this.selectedNode.data, node)) {
      logger.debug('select layer by selected node', this.selectedNode.id[1] + 1)
      ids.add(this.selectedNode.id[1] + 1)
    }
    if (this.selectedNode && getOutgoing(node, this.selectedNode.data)) {
      logger.debug('select layer by selected node (incoming)', this.selectedNode.id[1] - 1)
      ids.add(this.selectedNode.id[1] - 1)
    }

    if (this.layers[0]) {
      const nodes = this.layers[0].nodes.values()
      for (let j = 0; j < nodes.length; j++) {
        if (node.type === 'entity' && getOutgoing(node, nodes[j].data)) {
          logger.debug('select layer by incoming node', nodes[j], this.layers[0].id - 1)
          ids.add(this.layers[0].id - 1)
        }
        if (node.entity && getOutgoing(node.entity, nodes[j].data)) {
          logger.debug('select layer by incoming node on entity level', nodes[j], this.layers[0].id - 1)
          ids.add(this.layers[0].id - 1)
        }
        if (node.type === 'address') {
          const addresses = nodes[j].nodes.values()
          for (let k = 0; k < addresses.length; k++) {
            if (getOutgoing(node, addresses[k].data)) {
              logger.debug('select layer by incoming node on address level', addresses[k], this.layers[0].id - 1)
              ids.add(this.layers[0].id - 1)
            }
          }
        }
      }
    }

    for (let i = this.layers.length - 1; i >= 0; i--) {
      const nodes = this.layers[i].nodes.values()
      for (let j = 0; j < nodes.length; j++) {
        if (node.type === 'entity' && getOutgoing(nodes[j].data, node)) {
          logger.debug('select layer by outgoing node', nodes[j], this.layers[i].id + 1)
          ids.add(this.layers[i].id + 1)
        }
        if (node.entity && getOutgoing(nodes[j].data, node.entity)) {
          logger.debug('select layer by outgoing node on entity level', nodes[j], this.layers[i].id + 1)
          ids.add(this.layers[i].id + 1)
        }
        if (node.type === 'address') {
          const addresses = nodes[j].nodes.values()
          for (let k = 0; k < addresses.length; k++) {
            if (getOutgoing(addresses[k].data, node)) {
              logger.debug('select layer by outgoing node on address level', addresses[k], this.layers[i].id + 1)
              ids.add(this.layers[i].id + 1)
            }
          }
        }
      }
    }
    if (!node.entity) return false
    for (let i = 0; i < this.layers.length; i++) {
      if (this.layers[i].has([node.entity.id, this.layers[i].id])) {
        logger.debug('select layer by entity', this.layers[i], this.layers[i].id)
        ids.add(this.layers[i].id)
      }
    }
    if (ids.size() === 0) return false
    return ids.values()
  }

  getSvg () {
    return this.root.innerHTML
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    let entityRoot, entityShadowsRoot, addressShadowsRoot, addressRoot, entityLinksRoot, addressLinksRoot
    logger.debug('graph should update', this.update)
    if (this.shouldUpdate(true)) {
      logger.debug('redraw graph')
      this.svg = create('svg')
        .classed('w-full h-full graph', true)
        .attr('preserveAspectRatio', 'xMidYMid slice')
        .attr('xmlns', 'http://www.w3.org/2000/svg')
      this.renderViewBox()
      this.svg.node()
        .addEventListener('mousedown', (e) => {
          e.preventDefault()
          this.dispatcher('screenDragStart', { x: e.clientX, y: e.clientY })
        })
      this.svg.node()
        .addEventListener('mousemove', (e) => {
          if (this.dragging) {
            e.preventDefault()
            this.dispatcher('screenDragMove', { x: e.clientX, y: e.clientY })
          }
        })
      this.svg.node()
        .addEventListener('mouseup', (e) => {
          if (this.dragging) {
            e.preventDefault()
            this.dispatcher('screenDragStop')
          }
        })
      this.svg.node()
        .addEventListener('wheel', (e) => {
          e.preventDefault()
          this.dispatcher('screenZoom', {
            w: e.view.innerWidth,
            h: e.view.innerHeight,
            x: e.clientX,
            y: e.clientY,
            d: e.deltaY
          })
        })
      const markerHeight = transactionsPixelRange[1]
      this.arrowSummit = markerHeight
      this.svg.node().innerHTML = '<defs>' +
        (['black', 'red'].map(color =>
          `<marker id="arrow1-${color}" markerWidth="${this.arrowSummit}" markerHeight="${markerHeight}" refX="0" refY="${markerHeight / 2}" orient="auto" markerUnits="userSpaceOnUse">` +
           `<path d="M0,0 L0,${markerHeight} L${this.arrowSummit},${markerHeight / 2} Z" style="fill: ${color};" />` +
         '</marker>'
        )).join('') +
        '</defs>'
      this.graphRoot = this.svg.append('g')
      this.svg.on('click', () => {
        this.dispatcher('deselect')
      })
      this.root.appendChild(this.svg.node())

      entityShadowsRoot = this.graphRoot.append('g').classed('entityShadowsRoot', true)
      entityRoot = this.graphRoot.append('g').classed('entityRoot', true)
      addressShadowsRoot = this.graphRoot.append('g').classed('addressShadowsRoot', true)
      addressRoot = this.graphRoot.append('g').classed('addressRoot', true)
      entityLinksRoot = this.graphRoot.append('g').classed('entityLinksRoot', true)
      addressLinksRoot = this.graphRoot.append('g').classed('addressLinksRoot', true)
    } else {
      entityShadowsRoot = this.graphRoot.select('g.entityShadowsRoot')
      addressShadowsRoot = this.graphRoot.select('g.addressShadowsRoot')
      entityLinksRoot = this.graphRoot.select('g.entityLinksRoot')
      addressLinksRoot = this.graphRoot.select('g.addressLinksRoot')
      entityRoot = this.graphRoot.select('g.entityRoot')
      addressRoot = this.graphRoot.select('g.addressRoot')
      if (this.shouldUpdate('viewbox')) {
        this.renderViewBox()
      }
    }
    // render in this order
    this.renderLayers(entityRoot, addressRoot)
    this.renderLinks(entityLinksRoot, addressLinksRoot)
    this.renderShadows(entityShadowsRoot, addressShadowsRoot)
    super.render()
    return this.root
  }

  renderViewBox () {
    this.svg.attr('viewBox', `${this.x + this.dx - this.w / this.k / 2} ${this.y + this.dy - this.h / this.k / 2} ${this.w / (this.k + this.dk)} ${this.h / (this.k + this.dk)}`)
  }

  renderLayers (entityRoot, addressRoot, transform) {
    if (this.shouldUpdate('layers')) {
      entityRoot.node().innerHTML = ''
      addressRoot.node().innerHTML = ''
      this.layers
        .forEach((layer) => {
          if (layer.nodes.size() === 0) return
          layer.setUpdate(true)
          const cRoot = entityRoot.append('g')
          const aRoot = addressRoot.append('g')
          layer.render(cRoot, aRoot)
          // let first = layer.getFirst()
          // box.height += first.dy
          // let last = layer.getLast()
          // if (last !== first) {
          // box.height -= last.dy
          // }
          const layerHeight = layer.getHeight()
          const w = entityWidth + margin
          const x = layer.id * w
          const y = layerHeight / -2
          cRoot.attr('transform', `translate(${x}, ${y})`)
          aRoot.attr('transform', `translate(${x}, ${y})`)
          layer.translate(x, y)
        })
      if (this.layers.length === 0) {
        entityRoot.append('text')
          .attr('text-anchor', 'middle')
          .attr('fill', 'lightgrey')
          .text(t('Nothing to display yet'))
      }
    } else {
      this.layers.forEach((layer) => {
        if (layer.nodes.size() === 0) return
        layer.render()
      })
    }
  }

  renderLinks (entityRoot, addressRoot) {
    if (this.shouldUpdate('layers') || this.shouldUpdate('links')) {
      entityRoot.node().innerHTML = ''
      addressRoot.node().innerHTML = ''

      for (let i = 0; i < this.layers.length; i++) {
        // prepare the domain and links
        const domain = [1 / 0, 0]
        const entityLinksFromAddresses = {}
        this.layers[i].nodes.each((c) => {
          // stores links between neighbor entity resulting from address links
          entityLinksFromAddresses[c.data.id] = set()
          c.nodes.each((a) => {
            this.prepareLinks(domain, this.layers[i + 1], a)
              .forEach(cl => entityLinksFromAddresses[c.data.id].add(cl))
          })
          this.prepareEntityLinks(domain, this.layers[i + 1], c, entityLinksFromAddresses[c.data.id])
        })
        // render links
        this.layers[i].nodes.each((c) => {
          c.nodes.each((a) => {
            this.linkToLayer(addressRoot, domain, this.layers[i + 1], a)
          })
          this.linkToLayerEntity(entityRoot, domain, this.layers[i + 1], c, entityLinksFromAddresses[c.data.id])
        })
      }
    } else if (this.shouldUpdate('link')) {
      // updating the in- and outgoing links of one node (ie. when it is moved)
      const nodeIds = this.getUpdate('link')
      if (!nodeIds) return
      nodeIds.forEach(nodeId => {
        const node = this.getNode(nodeId, 'entity')
        let addressLinkSelects = ''
        const selector = (nodeId) => 'g.link[data-target="' + nodeId + '"],g.link[data-source="' + nodeId + '"]'
        if (node) {
          node.nodes.each(address => {
            addressLinkSelects += ',' + selector(address.id)
          })
        }
        ([addressRoot, entityRoot]).forEach(root => {
          root.selectAll(selector(nodeId) + addressLinkSelects)
            .nodes()
            .map((link) => {
              const a = [
                link.getAttribute('data-source'),
                link.getAttribute('data-target'),
                link.getAttribute('data-label'),
                link.getAttribute('data-scale')
              ]
              link.parentElement.removeChild(link)
              return a
            }).forEach(([s, t, label, scale]) => {
              const source = this.getNode(s, 'address') || this.getNode(s, 'entity')
              const target = this.getNode(t, 'address') || this.getNode(t, 'entity')
              this.drawLink(root, label, scale, source, target)
            })
        })
      })
    }
  }

  prepareLinks (domain, layer, address) {
    const entityLinks = []
    if (layer) {
      layer.nodes.each((entity2) => {
        let hasLinks = false
        entity2.nodes.each((address2) => {
          const ntx = getOutgoing(address.data, address2.data)
          if (ntx === undefined || ntx === false) return
          if (ntx !== null) {
            this.updateDomain(domain, this.findValueAndLabel(ntx)[0])
          }
          hasLinks = true
        })
        if (hasLinks) {
          entityLinks.push(entity2.data.id)
        }
      })
    }
    return entityLinks
  }

  prepareEntityLinks (domain, layer, source, entityLinksFromAddresses) {
    if (layer) {
      layer.nodes.each((entity2) => {
        const ntx = getOutgoing(source.data, entity2.data)
        if (ntx === undefined || ntx === false) return
        // skip entity if contains in entityLinksFromAddresses
        if (entityLinksFromAddresses.has(entity2.data.id)) return
        if (ntx !== null) {
          this.updateDomain(domain, this.findValueAndLabel(ntx)[0])
        }
      })
    }
  }

  updateDomain (domain, value) {
    domain[0] = Math.min(domain[0], value)
    domain[1] = Math.max(domain[1], value)
  }

  linkToLayer (root, domain, layer, address) {
    if (layer) {
      layer.nodes.each((entity2) => {
        entity2.nodes.each((address2) => {
          const ntx = getOutgoing(address.data, address2.data)
          if (ntx === undefined || ntx === false) return
          this.renderLink(root, domain, address, address2, ntx)
        })
      })
    }
  }

  linkToLayerEntity (root, domain, layer, source, entityLinksFromAddresses) {
    if (layer) {
      layer.nodes.each((entity2) => {
        const ntx = getOutgoing(source.data, entity2.data)
        if (ntx === undefined || ntx === false) return
        // skip entity if contains in entityLinksFromAddresses
        if (entityLinksFromAddresses.has(entity2.data.id)) return
        this.renderLink(root, domain, source, entity2, ntx)
      })
    }
  }

  renderShadows (entityRoot, addressRoot) {
    if (!this.shouldUpdate()) return
    entityRoot.node().innerHTML = ''
    addressRoot.node().innerHTML = ''
    // TODO use a data structure which stores and lists entries in sorted order to prevent this sorting
    const sort = (node1, node2) => {
      return node1.id[1] - node2.id[1]
    }
    this.linkShadows(addressRoot, this.addressNodes.values().sort(sort))
    this.linkShadows(entityRoot, this.entityNodes.values().sort(sort))
  }

  linkShadows (root, nodes) {
    nodes.forEach((node1) => {
      for (let i = 0; i < nodes.length; i++) {
        const node2 = nodes[i]
        if (node1 === node2) continue
        if (node1.id[0] !== node2.id[0]) continue
        if (node1.data.keyspace !== node2.data.keyspace) continue
        if (node1.id[1] >= node2.id[1]) continue
        this.drawShadow(root, node1, node2)
        // stop iterating if a shadow to next layer was found
        return
      }
    })
  }

  drawShadow (root, source, target) {
    const path = this.shadowLinker({ source: [source, true], target: [target, false] })
    root.append('path').classed('shadow', true).attr('d', path)
      .on('mouseover', () => this.dispatcher('tooltip', 'shadow'))
      .on('mouseout', () => this.dispatcher('hideTooltip'))
  }

  renderLink (root, domain, source, target, tx) {
    let value = 1
    let label = ''
    let clickable = false
    let scale
    if (tx !== null) {
      const l = this.findValueAndLabel(tx)
      value = l[0]
      label = l[1]
      clickable = tx.tx_list && tx.tx_list.length > 0
    }
    // scalePow chooses the median of range, if domain is a-a (instead a-b)
    // so force it to use the lower range bound
    if (domain[0] !== Infinity && domain[0] !== domain[1]) {
      scale = scalePow().domain(domain).range(transactionsPixelRange)(value)
    } else {
      scale = transactionsPixelRange[0]
    }
    this.drawLink(root, label, scale, source, target, clickable)
  }

  drawLink (root, label, scale, source, target, clickable) {
    const path = this.linker({ source: [source, true, scale], target: [target, false, scale] })
    const g1 = root.append('g')
      .attr('class', 'link')
      .classed('selected', this.isSelectedLink(source, target))
      .attr('data-target', target.id)
      .attr('data-source', source.id)
      .attr('data-label', label)
      .attr('data-scale', scale)
      .on('mouseover', () => this.dispatcher('tooltip', 'link'))
      .on('mouseout', () => this.dispatcher('hideTooltip'))
      .on('click', () => {
        this.dispatcher('clickLink', { source, target })
        event.stopPropagation()
      })
    g1.append('path').attr('d', path)
      .classed('linkPathFrame', true)
      .style('stroke-width', '6px')
      .style('opacity', 0)
    g1.append('path').attr('d', path)
      .classed('linkPath', true)
      .style('stroke-width', scale + 'px')
      .style('fill', 'none')
      .style('stroke', 'black')
      .style('marker-end', 'url(#arrow1-black)')
    const sourceX = source.getXForLinks() + source.getWidthForLinks()
    const sourceY = source.getYForLinks() + source.getHeightForLinks() / 2
    const targetX = target.getXForLinks() - this.arrowSummit
    const targetY = target.getYForLinks() + target.getHeightForLinks() / 2
    const fontSize = 12
    const x = (sourceX + targetX) / 2
    const y = (sourceY + targetY) / 2 + fontSize / 3
    const g2 = g1.append('g')

    const f = () => {
      return g2.append('text')
        .classed('linkText', true)
        .attr('text-anchor', 'middle')
        .text(label)
        .style('font-size', fontSize)
        .attr('x', x)
        .attr('y', y)
    }

    const t = f()

    const box = t.node().getBBox()

    const width = box.width // (label + '').length * fontSize
    const height = box.height // fontSize * 1.2

    t.remove()

    g2.append('rect')
      .classed('linkRect', true)
      .attr('rx', fontSize / 2)
      .attr('ry', fontSize / 2)
      .attr('x', x - width / 2)
      .attr('y', y - height * 0.85)
      .attr('width', width)
      .attr('height', height)

    f()
  }

  findValueAndLabel (tx) {
    let value, label
    if (this.txLabelType === 'estimated_value') {
      value = tx[this.txLabelType].value
      label = formatCurrency(tx[this.txLabelType][this.currency], this.currency, { dontAppendCurrency: true, keyspace: tx.keyspace })
    } else if (this.txLabelType === 'no_txs') {
      value = label = tx[this.txLabelType]
    } else {
      value = 0
      label = '?'
    }
    return [value, label]
  }

  serializeGraph () {
    const entityNodes = []
    this.entityNodes.each(node => entityNodes.push([node.id, node.serialize()]))

    const addressNodes = []
    this.addressNodes.each(node => addressNodes.push([node.id, node.serialize()]))

    const layers = []
    this.layers.forEach(layer => layers.push(layer.serialize()))
    return [entityNodes, addressNodes, layers]
  }

  serialize () {
    const s = this.serializeGraph()

    return [
      this.currency,
      this.labelType,
      this.txLabelType,
      this.x,
      this.y,
      this.k,
      s[0],
      s[1],
      s[2],
      this.colorMapCategories.entries(),
      this.colorMapTags.entries()
    ]
  }

  deserializeGraph (version, store, entityNodes, addressNodes, layers) {
    addressNodes.forEach(([nodeId, address]) => {
      if (version === '0.4.0') {
        const found = store.find(nodeId[0], 'address')
        if (!found) return
        nodeId[2] = found.keyspace
      }
      const data = store.get(nodeId[2], 'address', nodeId[0])
      const node = new AddressNode(this.dispatcher, data, nodeId[1], this.labelType.addressLabel, this.colors.address, this.currency)
      node.deserialize(address)
      this.setAddressNodes(node)
    })
    entityNodes.forEach(([nodeId, entity]) => {
      if (version === '0.4.0') {
        const found = store.find(nodeId[0], 'entity')
        if (!found) return
        nodeId[2] = found.keyspace
      }
      const data = store.get(nodeId[2], 'entity', nodeId[0])
      const node = new EntityNode(this.dispatcher, data, nodeId[1], this.labelType.entityLabel, this.colors.entity, this.currency)
      node.deserialize(version, entity, this.addressNodes)
      this.setEntityNodes(node)
    })
    layers.forEach(([id, entityKeys]) => {
      const l = new Layer(id)
      entityKeys.forEach(key => {
        if (version === '0.4.0') {
          let found = null
          this.entityNodes.each(node => {
            if (!found && ([node.id[0], node.id[1]]).join(',') === key) found = node
          })
          if (!found) return
          key = found.id
        }
        l.add(this.entityNodes.get(key))
      })
      this.layers.push(l)
    })
  }

  deserialize (version, [
    currency,
    labelType,
    txLabelType,
    x,
    y,
    k,
    entityNodes,
    addressNodes,
    layers,
    colorMapCategories,
    colorMapTags
  ], store) {
    this.currency = currency
    this.labelType = labelType
    this.txLabelType = txLabelType
    this.x = x
    this.y = y
    this.k = k
    colorMapCategories.forEach(({ key, value }) => {
      this.colorMapCategories.set(key, value)
    })
    colorMapTags.forEach(({ key, value }) => {
      this.colorMapTags.set(key, value)
    })
    this.deserializeGraph(version, store, entityNodes, addressNodes, layers)
  }

  screenDragStart (coords) {
    this.dragging = coords
    this.dx = this.dy = 0
  }

  screenDragMove ({ x, y }) {
    this.dx = (this.dragging.x - x) / this.k
    this.dy = (this.dragging.y - y) / this.k
    this.setUpdate('viewbox')
  }

  screenDragStop () {
    this.dragging = null
    this.x += this.dx
    this.y += this.dy
    this.dx = this.dy = 0
    this.setUpdate('viewbox')
  }

  screenZoom ({ x, y, d }) {
    const k = this.k
    const dir = d / Math.abs(d)
    this.k /= Math.pow(1 + (dir / zoomSlowity), Math.abs(d) / 53)
    const wp = (this.w / 2 - x) / this.w
    const hp = (this.h / 2 - y) / this.h
    // zoom to mouse cursor
    this.x += (this.w / this.k - this.w / k) * wp
    this.y += (this.h / this.k - this.h / k) * hp
    this.setUpdate('viewbox')
  }
}
