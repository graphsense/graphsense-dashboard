import {create, event} from 'd3-selection'
import {scalePow, scaleOrdinal} from 'd3-scale'
import {set, map} from 'd3-collection'
import {schemeSet3} from 'd3-scale-chromatic'
import {hsl} from 'd3-color'
import {linkHorizontal} from 'd3-shape'
import {zoom, zoomIdentity} from 'd3-zoom'
import Layer from './nodeGraph/layer.js'
import EntityNode from './nodeGraph/entityNode.js'
import AddressNode from './nodeGraph/addressNode.js'
import Component from './component.js'
import {formatCurrency} from './utils'
import Logger from './logger.js'
import {entityWidth, expandHandleWidth} from './globals.js'

const logger = Logger.create('NodeGraph') // eslint-disable-line no-unused-vars

const margin = 300
const w = 800
const h = 600

const x = w / -2
const y = h / -2

const hsl2rgb = (h, s, l) => {
  h = h % 360
  s = s * 100 + '%'
  l = l * 100 + '%'
  return `hsl(${h}, ${s}, ${l})`
}

const lightnessFactor = {
  'entity': 1,
  'address': 0.83
}
const defaultColor = {
  'entity': hsl2rgb(178, 0, 0.95),
  'address': hsl2rgb(178, 0, 0.90)
}

const transactionsPixelRange = [1, 7]

const colorScale = scaleOrdinal(schemeSet3)

const maxNumSnapshots = 4

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
    this.references = {address: map(), entity: map()}
    this.adding = set()
    this.selectedNode = null
    this.highlightedNodes = []
    this.layers = []
    this.transform = {k: 1, x: 0, y: 0, dx: 0, dy: 0}
    this.colorMapCategories = map()
    this.colorMapTags = map()
    let colorGen = (map, type) => (k) => {
      if (!k) return defaultColor[type]
      let color = map.get(k)
      if (color === undefined) {
        color = colorScale(map.size())
        map.set(k, color.toString())
      }
      let c = hsl(color)
      c.l = c.l * lightnessFactor[type]
      return c
    }
    this.colors =
      {
        'entity': {
          categories: colorGen(this.colorMapCategories, 'entity'),
          tags: colorGen(this.colorMapTags, 'entity'),
          range: (v) => defaultColor['entity']
        },
        'address': {
          categories: colorGen(this.colorMapCategories, 'address'),
          tags: colorGen(this.colorMapTags, 'address'),
          range: (v) => defaultColor['address']
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
  }
  setCategoryColors (cc) {
    if (cc === null || Array.isArray(cc) || typeof cc !== 'object') return
    for (let category in cc) {
      logger.debug('category', category, cc[category])
      let color = cc[category]
      this.colorMapCategories.set(category, color)
    }
    this.setUpdate('layers')
  }
  setCategories (categories) {
    if (!Array.isArray(categories)) return
    categories.forEach((category, i) => {
      if (this.colorMapCategories.has(category)) return
      let c = hsl(colorScale(i))
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
    let id = [node.id[0], node.id[2]]
    let refCount = this.references[type].get(id) || 0
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
    let id = [nodeId[0], nodeId[2]]
    let refCount = this.references[type].get(id)
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
    let s = this.snapshots[this.currentSnapshotIndex + 1]
    if (!s) return
    this.currentSnapshotIndex++
    this.loadSnapshot(store, s)
  }
  loadPreviousSnapshot (store) {
    let s = this.snapshots[this.currentSnapshotIndex - 1]
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
    let node = this.getNode(id, type)
    if (!node) return
    node.searchingNeighbors(isOutgoing, state)
  }
  dragNode (id, type, dx, dy) {
    let layer = this.findLayer(id[1])
    if (!layer) return
    let entity = layer.nodes.get(id)
    if (!entity) return

    dx /= this.transform.k
    dy /= this.transform.k

    entity.ddx += dx
    entity.ddy += dy

    if (entity.ddx - 2 * expandHandleWidth < margin / -2) return
    if (entity.ddx + 2 * expandHandleWidth > margin / 2) return

    let nodes = layer.nodes.values()
    let x = entity.x + entity.ddx - expandHandleWidth
    let y = entity.y + entity.ddy
    let cw = entity.getWidthForLinks()
    let ch = entity.getHeightForLinks()
    for (let i = 0; i < nodes.length; i++) {
      let sister = nodes[i]
      if (sister === entity) continue
      let sx = sister.getXForLinks()
      let sy = sister.getYForLinks()
      let sw = sister.getWidthForLinks()
      let sh = sister.getHeightForLinks()
      if (((x + cw >= sx && x + cw <= sx + sw) ||
          (x >= sx && x <= sx + sw)
      ) &&
          ((y + ch >= sy && y + ch <= sy + sh) ||
          (y >= sy && y <= sy + sh)
          )
      ) return
    }
    entity.dx = entity.ddx
    entity.dy = entity.ddy
    entity.setUpdate('position')
    this.setUpdate('link', id)
  }
  dragNodeEnd (id, type) {
    let entity = this.entityNodes.get(id)
    if (!entity) return
    entity.ddx = entity.dx
    entity.ddy = entity.dy
    this.dirty = true
  }
  sortEntityAddresses (id, property) {
    let entity = this.entityNodes.get(id)
    logger.debug('sort addresses entity', entity)
    if (!entity) return
    entity.sortAddresses(property)
    this.setUpdate('layers')
  }
  deselect () {
    if (!this.selectedNode) return
    this.selectedNode.deselect()
    this.selectedNode.setUpdate('select')
    this.selectedNode = null
  }
  selectNodeWhenLoaded ([id, type, keyspace]) {
    this.nextSelectedNode = {id, type, keyspace}
  }
  selectNodeIfIsNextNode (node) {
    logger.debug('selectNodeIfIsNextNode', node, this.nextSelectedNode)
    if (!this.nextSelectedNode) return
    if (this.nextSelectedNode.type !== node.data.type) return
    if (this.nextSelectedNode.keyspace !== node.data.keyspace) return
    if (this.nextSelectedNode.id != node.data.id) return // eslint-disable-line eqeqeq
    this._selectNode(node)
    this.nextSelectedNode = null
    this.zoomToNodes = true
  }
  zoomToHighlightedNodes () {
    if (!this.zoomToNodes) return
    logger.debug('highlighted', this.highlightedNodes)
    if (this.highlightedNodes.length === 0) return
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
    let dx = (x2 - x1)
    let dy = (y2 - y1)
    x1 -= dx * 0.3
    x2 += dx * 0
    y1 -= dy * 0.1
    y2 += dy * 0.1
    let k = w / Math.max(w, (x2 - x1))
    let x = (x2 + x1) / -2
    let y = (y2 + y1) / -2
    let transform = zoomIdentity.scale(k).translate(x, y)
    /*
    TODO make transition duration depend on distance of transforms
    let vx = x * k - this.transform.x * this.transform.k
    let vy = y * k - this.transform.y * this.transform.k
    let len = Math.sqrt(vx * vx + vy * vy)
    logger.debug('len', len)
    let duration = len / 200 * 1000
    */
    let duration = 1000
    this.svg.transition().duration(duration).call(this.zoom.transform, transform)
    this.zoomToNodes = false
  }
  setTxLabel (type) {
    this.txLabelType = type
    this.setUpdate('links')
  }
  setCurrency (currency) {
    this.currency = currency
    this.addressNodes.each(node => node.setCurrency(currency))
    this.entityNodes.each(node => node.setCurrency(currency))
    if (this.txLabelType === 'estimatedValue') {
      this.setUpdate('links')
    }
  }
  setEntityLabel (labelType) {
    this.labelType['entityLabel'] = labelType
    this.entityNodes.each((node) => {
      node.setLabelType(labelType)
    })
  }
  setAddressLabel (labelType) {
    this.labelType['addressLabel'] = labelType
    this.addressNodes.each((node) => {
      node.setLabelType(labelType)
    })
  }
  selectNode (type, nodeId) {
    let sel = this.getNode(nodeId, type)
    if (!sel) return
    this._selectNode(sel)
  }
  _selectNode (sel) {
    sel.select()
    if (this.selectedNode && this.selectedNode !== sel) {
      this.selectedNode.deselect()
    }
    this.highlightedNodes.forEach(node => node.unhighlight())
    this.highlightedNodes = []
    let nodes
    if (sel.data.type === 'entity') {
      nodes = this.entityNodes
    } else if (sel.data.type === 'address') {
      nodes = this.addressNodes
    }
    nodes.each(node => {
      if (node.data.id === sel.data.id) {
        node.highlight()
        this.highlightedNodes.push(node)
      }
    })
    logger.debug('highlighted in select', this.highlightedNodes)
    this.selectedNode = sel
  }
  setResultEntityAddresses (id, addresses) {
    let entity = this.entityNodes.get(id)
    addresses.forEach((address) => {
      if (this.addressNodes.has([address.id, id[1], address.keyspace])) return
      let addressNode = new AddressNode(this.dispatcher, address, id[1], this.labelType['addressLabel'], this.colors['address'], this.currency)
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
    return this.layers.filter(({id}) => id == layerId)[0] // eslint-disable-line eqeqeq
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
      addressNode = new AddressNode(this.dispatcher, object, layerId, this.labelType['addressLabel'], this.colors['address'], this.currency)
      this.setAddressNodes(addressNode)
      this.selectNodeIfIsNextNode(addressNode)
      logger.debug('new AddressNode', addressNode)
      node = this.entityNodes.get([object.entity.id, layerId, object.entity.keyspace])
      if (!node) {
        node = new EntityNode(this.dispatcher, object.entity, layerId, this.labelType['entityLabel'], this.colors['entity'], this.currency)
      }
      node.add(addressNode)
      this.setEntityNodes(node)
    } else if (object.type === 'entity') {
      node = this.entityNodes.get([object.id, layerId, object.keyspace])
      if (node) {
        this.selectNodeIfIsNextNode(node)
        return node
      }
      node = new EntityNode(this.dispatcher, object, layerId, this.labelType['entityLabel'], this.colors['entity'], this.currency)
      this.setEntityNodes(node)
      logger.debug('new EntityNode', node)
      this.selectNodeIfIsNextNode(node)
    } else {
      throw Error('unknown node type')
    }
    this.dirty = true

    let anchorLayer = anchor && this.findLayer(anchor.nodeId[1])

    let addToTop = anchorLayer && anchorLayer.isNodeInUpperHalf(anchor.nodeId)

    layer.add(node, addToTop)
    return node
  }
  remove (nodeType, nodeId) {
    logger.debug('remove', nodeType, nodeId)
    let node = this.getNode(nodeId, nodeType)
    if (nodeType === 'address') {
      this.removeAddressNode(nodeId)
    } else if (nodeType === 'entity') {
      this.removeEntityNode(nodeId)
    }
    if (this.selectedNode === node) {
      this.selectedNode = null
    }
    let layer = this.findLayer(nodeId[1])
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
    let entity = this.entityNodes.get(id)
    if (!entity) return
    entity.nodes.each((address) => this.remove('address', address.id))
  }
  additionLayerBySelection (addressId) {
    if (!addressId) return false
    if (!(this.selectedNode instanceof EntityNode)) return false
    let entity = this.selectedNode.data
    if (!entity.addresses.has(addressId)) return false
    return this.selectedNode.id[1]
  }
  additionLayerBySearch (node) {
    logger.debug('search', node)
    let ids = set()
    if (this.selectedNode && this.selectedNode.data.outgoing.has(node.id)) {
      logger.debug('select layer by selected node', this.selectedNode.id[1] + 1)
      ids.add(this.selectedNode.id[1] + 1)
    }
    if (this.selectedNode && node.outgoing.has(this.selectedNode.data.id)) {
      logger.debug('select layer by selected node (incoming)', this.selectedNode.id[1] - 1)
      ids.add(this.selectedNode.id[1] - 1)
    }

    if (this.layers[0]) {
      let nodes = this.layers[0].nodes.values()
      for (let j = 0; j < nodes.length; j++) {
        if (node.type === 'entity' && node.outgoing.has(nodes[j].data.id)) {
          logger.debug('select layer by incoming node', nodes[j], this.layers[0].id - 1)
          ids.add(this.layers[0].id - 1)
        }
        if (node.entity && node.entity.outgoing.has(nodes[j].data.id)) {
          logger.debug('select layer by incoming node on entity level', nodes[j], this.layers[0].id - 1)
          ids.add(this.layers[0].id - 1)
        }
        if (node.type === 'address') {
          let addresses = nodes[j].nodes.values()
          for (let k = 0; k < addresses.length; k++) {
            if (node.outgoing.has(addresses[k].data.id)) {
              logger.debug('select layer by incoming node on address level', addresses[k], this.layers[0].id - 1)
              ids.add(this.layers[0].id - 1)
            }
          }
        }
      }
    }

    for (let i = this.layers.length - 1; i >= 0; i--) {
      let nodes = this.layers[i].nodes.values()
      for (let j = 0; j < nodes.length; j++) {
        let outgoing = nodes[j].data.outgoing
        if (node.type === 'entity' && outgoing.has(node.id)) {
          logger.debug('select layer by outgoing node', nodes[j], this.layers[i].id + 1)
          ids.add(this.layers[i].id + 1)
        }
        if (node.entity && outgoing.has(node.entity.id)) {
          logger.debug('select layer by outgoing node on entity level', nodes[j], this.layers[i].id + 1)
          ids.add(this.layers[i].id + 1)
        }
        if (node.type === 'address') {
          let addresses = nodes[j].nodes.values()
          for (let k = 0; k < addresses.length; k++) {
            if (addresses[k].data.outgoing.has(node.id)) {
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
    let entityRoot, entityShadowsRoot, addressShadowsRoot, addressRoot, linksRoot
    logger.debug('graph should update', this.shouldUpdate())
    let transformGraph = () => {
      let x = this.transform.x
      let y = this.transform.y
      this.graphRoot.attr('transform', `translate(${x}, ${y}) scale(${this.transform.k})`)
    }
    if (this.shouldUpdate(true)) {
      this.zoom = zoom()
      this.svg = create('svg')
        .classed('w-full h-full graph', true)
        .attr('viewBox', `${x} ${y} ${w} ${h}`)
        .attr('preserveAspectRatio', 'xMidYMid slice')
        .attr('xmlns', 'http://www.w3.org/2000/svg')
        .call(this.zoom.on('zoom', () => {
          this.transform.k = event.transform.k
          this.transform.x = event.transform.x
          this.transform.y = event.transform.y
          transformGraph()
        }))
      let markerHeight = transactionsPixelRange[1]
      this.arrowSummit = markerHeight
      this.svg.node().innerHTML = '<defs>' +
        (['black', 'red'].map(color =>
          `<marker id="arrow1-${color}" markerWidth="${this.arrowSummit}" markerHeight="${markerHeight}" refX="0" refY="${markerHeight / 2}" orient="auto" markerUnits="userSpaceOnUse">` +
           `<path d="M0,0 L0,${markerHeight} L${this.arrowSummit},${markerHeight / 2} Z" style="fill: ${color};" />` +
         '</marker>'
        )).join('') +
        '</defs>'
      this.graphRoot = this.svg.append('g')
      transformGraph()
      this.svg.on('click', () => {
        this.dispatcher('deselect')
      })
      this.root.appendChild(this.svg.node())

      entityShadowsRoot = this.graphRoot.append('g').classed('entityShadowsRoot', true)
      entityRoot = this.graphRoot.append('g').classed('entityRoot', true)
      addressShadowsRoot = this.graphRoot.append('g').classed('addressShadowsRoot', true)
      linksRoot = this.graphRoot.append('g').classed('linksRoot', true)
      addressRoot = this.graphRoot.append('g').classed('addressRoot', true)
    } else {
      entityShadowsRoot = this.graphRoot.select('g.entityShadowsRoot')
      addressShadowsRoot = this.graphRoot.select('g.addressShadowsRoot')
      linksRoot = this.graphRoot.select('g.linksRoot')
      entityRoot = this.graphRoot.select('g.entityRoot')
      addressRoot = this.graphRoot.select('g.addressRoot')
    }
    // render in this order
    this.renderLayers(entityRoot, addressRoot)
    this.renderLinks(linksRoot)
    this.renderShadows(entityShadowsRoot, addressShadowsRoot)
    this.zoomToHighlightedNodes()
    super.render()
    return this.root
  }
  renderLayers (entityRoot, addressRoot, transform) {
    if (this.shouldUpdate('layers')) {
      entityRoot.node().innerHTML = ''
      addressRoot.node().innerHTML = ''
      this.layers
        .forEach((layer) => {
          if (layer.nodes.size() === 0) return
          layer.setUpdate(true)
          let cRoot = entityRoot.append('g')
          let aRoot = addressRoot.append('g')
          layer.render(cRoot, aRoot)
          // let first = layer.getFirst()
          // box.height += first.dy
          // let last = layer.getLast()
          // if (last !== first) {
          // box.height -= last.dy
          // }
          let layerHeight = layer.getHeight()
          let w = entityWidth + margin
          let x = layer.id * w
          let y = layerHeight / -2
          cRoot.attr('transform', `translate(${x}, ${y})`)
          aRoot.attr('transform', `translate(${x}, ${y})`)
          layer.translate(x, y)
        })
      if (this.layers.length === 0) {
        entityRoot.append('text')
          .attr('text-anchor', 'middle')
          .attr('fill', 'lightgrey')
          .text('Nothing to display yet!')
      }
    } else {
      this.layers.forEach((layer) => {
        if (layer.nodes.size() === 0) return
        layer.render()
      })
    }
  }
  renderLinks (root) {
    if (this.shouldUpdate('layers') || this.shouldUpdate('links')) {
      root.node().innerHTML = ''

      for (let i = 0; i < this.layers.length; i++) {
        // prepare the domain and links
        let domain = [1 / 0, 0]
        let entityLinksFromAddresses = {}
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
            this.linkToLayer(root, domain, this.layers[i + 1], a)
          })
          this.linkToLayerEntity(root, domain, this.layers[i + 1], c, entityLinksFromAddresses[c.data.id])
        })
      }
    } else if (this.shouldUpdate('link')) {
      // updating the in- and outgoing links of one node (ie. when it is moved)
      let nodeId = this.getUpdate('link')
      if (!nodeId) return
      let node = this.getNode(nodeId, 'entity')
      let addressLinkSelects = ''
      let selector = (nodeId) => 'g.link[data-target="' + nodeId + '"],g.link[data-source="' + nodeId + '"]'
      if (node) {
        node.nodes.each(address => {
          addressLinkSelects += ',' + selector(address.id)
        })
      }
      root.selectAll(selector(nodeId) + addressLinkSelects)
        .nodes()
        .map((link) => {
          let a = [
            link.getAttribute('data-source'),
            link.getAttribute('data-target'),
            link.getAttribute('data-label'),
            link.getAttribute('data-scale')
          ]
          link.parentElement.removeChild(link)
          return a
        }).forEach(([s, t, label, scale]) => {
          let source = this.getNode(s, 'address') || this.getNode(s, 'entity')
          let target = this.getNode(t, 'address') || this.getNode(t, 'entity')
          this.drawLink(root, label, scale, source, target)
        })
    }
  }
  prepareLinks (domain, layer, address) {
    let neighbors = address.data.outgoing
    let entityLinks = []
    if (layer) {
      layer.nodes.each((entity2) => {
        let hasLinks = false
        entity2.nodes.each((address2) => {
          let ntx = neighbors.get(address2.data.id)
          if (ntx === undefined) return
          this.updateDomain(domain, this.findValueAndLabel(ntx)[0])
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
    let neighbors = source.data.outgoing
    if (layer) {
      layer.nodes.each((entity2) => {
        let ntx = neighbors.get(entity2.data.id)
        if (ntx === undefined) return
        // skip entity if contains in entityLinksFromAddresses
        if (entityLinksFromAddresses.has(entity2.data.id)) return
        this.updateDomain(domain, this.findValueAndLabel(ntx)[0])
      })
    }
  }
  updateDomain (domain, value) {
    domain[0] = Math.min(domain[0], value)
    domain[1] = Math.max(domain[1], value)
  }
  linkToLayer (root, domain, layer, address) {
    let neighbors = address.data.outgoing
    if (layer) {
      layer.nodes.each((entity2) => {
        entity2.nodes.each((address2) => {
          let ntx = neighbors.get(address2.data.id)
          if (ntx === undefined) return
          this.renderLink(root, domain, address, address2, ntx)
        })
      })
    }
  }
  linkToLayerEntity (root, domain, layer, source, entityLinksFromAddresses) {
    let neighbors = source.data.outgoing
    if (layer) {
      layer.nodes.each((entity2) => {
        let ntx = neighbors.get(entity2.data.id)
        if (ntx === undefined) return
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
    let sort = (node1, node2) => {
      return node1.id[1] - node2.id[1]
    }
    this.linkShadows(addressRoot, this.addressNodes.values().sort(sort))
    this.linkShadows(entityRoot, this.entityNodes.values().sort(sort))
  }
  linkShadows (root, nodes) {
    nodes.forEach((node1) => {
      for (let i = 0; i < nodes.length; i++) {
        let node2 = nodes[i]
        if (node1 === node2) continue
        if (node1.id[0] !== node2.id[0]) continue
        if (node1.id[1] >= node2.id[1]) continue
        this.drawShadow(root, node1, node2)
        // stop iterating if a shadow to next layer was found
        return
      }
    })
  }
  drawShadow (root, source, target) {
    let path = this.shadowLinker({source: [source, true], target: [target, false]})
    root.append('path').classed('shadow', true).attr('d', path)
      .on('mouseover', () => this.dispatcher('tooltip', 'shadow'))
      .on('mouseout', () => this.dispatcher('hideTooltip'))
  }
  renderLink (root, domain, source, target, tx) {
    let value, label
    [value, label] = this.findValueAndLabel(tx)
    let scale
    // scalePow chooses the median of range, if domain is a-a (instead a-b)
    // so force it to use the lower range bound
    if (domain[0] !== domain[1]) {
      scale = scalePow().domain(domain).range(transactionsPixelRange)(value)
    } else {
      scale = transactionsPixelRange[0]
    }
    this.drawLink(root, label, scale, source, target)
  }
  drawLink (root, label, scale, source, target) {
    let path = this.linker({source: [source, true, scale], target: [target, false, scale]})
    let g1 = root.append('g').classed('link', true)
      .attr('data-target', target.id)
      .attr('data-source', source.id)
      .attr('data-label', label)
      .attr('data-scale', scale)
      .on('mouseover', () => this.dispatcher('tooltip', 'link'))
      .on('mouseout', () => this.dispatcher('hideTooltip'))
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
    let sourceX = source.getXForLinks() + source.getWidthForLinks()
    let sourceY = source.getYForLinks() + source.getHeightForLinks() / 2
    let targetX = target.getXForLinks() - this.arrowSummit
    let targetY = target.getYForLinks() + target.getHeightForLinks() / 2
    let fontSize = 12
    let x = (sourceX + targetX) / 2
    let y = (sourceY + targetY) / 2 + fontSize / 3
    let g2 = g1.append('g')

    let f = () => {
      return g2.append('text')
        .classed('linkText', true)
        .attr('text-anchor', 'middle')
        .text(label)
        .style('font-size', fontSize)
        .attr('x', x)
        .attr('y', y)
    }

    let t = f()

    let box = t.node().getBBox()

    let width = box.width // (label + '').length * fontSize
    let height = box.height // fontSize * 1.2

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
      label = formatCurrency(tx[this.txLabelType][this.currency], this.currency, {dontAppendCurrency: true, keyspace: tx.keyspace})
    } else if (this.txLabelType === 'no_txs') {
      value = label = tx[this.txLabelType]
    } else {
      value = 0
      label = '?'
    }
    return [value, label]
  }
  serializeGraph () {
    let entityNodes = []
    this.entityNodes.each(node => entityNodes.push([node.id, node.serialize()]))

    let addressNodes = []
    this.addressNodes.each(node => addressNodes.push([node.id, node.serialize()]))

    let layers = []
    this.layers.forEach(layer => layers.push(layer.serialize()))
    return [entityNodes, addressNodes, layers]
  }
  serialize () {
    let entityNodes, addressNodes, layers

    [entityNodes, addressNodes, layers] = this.serializeGraph()

    return [
      this.currency,
      this.labelType,
      this.txLabelType,
      this.transform,
      entityNodes,
      addressNodes,
      layers,
      this.colorMapCategories.entries(),
      this.colorMapTags.entries()
    ]
  }
  deserializeGraph (version, store, entityNodes, addressNodes, layers) {
    addressNodes.forEach(([nodeId, address]) => {
      if (version === '0.4.0') {
        let found = store.find(nodeId[0], 'address')
        if (!found) return
        nodeId[2] = found.keyspace
      }
      let data = store.get(nodeId[2], 'address', nodeId[0])
      let node = new AddressNode(this.dispatcher, data, nodeId[1], this.labelType['addressLabel'], this.colors['address'], this.currency)
      node.deserialize(address)
      this.setAddressNodes(node)
    })
    entityNodes.forEach(([nodeId, entity]) => {
      if (version === '0.4.0') {
        let found = store.find(nodeId[0], 'entity')
        if (!found) return
        nodeId[2] = found.keyspace
      }
      let data = store.get(nodeId[2], 'entity', nodeId[0])
      let node = new EntityNode(this.dispatcher, data, nodeId[1], this.labelType['entityLabel'], this.colors['entity'], this.currency)
      node.deserialize(version, entity, this.addressNodes)
      this.setEntityNodes(node)
    })
    layers.forEach(([id, entityKeys]) => {
      let l = new Layer(id)
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
    transform,
    entityNodes,
    addressNodes,
    layers,
    colorMapCategories,
    colorMapTags
  ], store) {
    this.currency = currency
    this.labelType = labelType
    this.txLabelType = txLabelType
    this.transform = transform
    colorMapCategories.forEach(({key, value}) => {
      this.colorMapCategories.set(key, value)
    })
    colorMapTags.forEach(({key, value}) => {
      this.colorMapTags.set(key, value)
    })
    this.deserializeGraph(version, store, entityNodes, addressNodes, layers)
  }
}
