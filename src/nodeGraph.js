import {create, event} from 'd3-selection'
import {scalePow} from 'd3-scale'
import {set, map} from 'd3-collection'
import {linkHorizontal} from 'd3-shape'
import {drag} from 'd3-drag'
import {zoom} from 'd3-zoom'
import Layer from './nodeGraph/layer.js'
import ClusterNode from './nodeGraph/clusterNode.js'
import AddressNode from './nodeGraph/addressNode.js'
import Component from './component.js'
import {formatCurrency} from './utils'
import Logger from './logger.js'
import {clusterWidth, categories, expandHandleWidth} from './globals.js'

const logger = Logger.create('NodeGraph') // eslint-disable-line no-unused-vars

const margin = 300
const x = -300
const y = -300
const w = 800
const h = 600

const hsl2rgb = (h, s, l) => {
  h = h % 360
  s = s * 100 + '%'
  l = l * 100 + '%'
  return `hsl(${h}, ${s}, ${l})`
}

const chromaStep = 67
const saturation = 94 / 255
const lightness = {
  'cluster': 209 / 255,
  'address': 230 / 255
}
const defaultColor = {
  'cluster': hsl2rgb(178, 0, lightness['cluster']),
  'address': hsl2rgb(178, 0, lightness['address'])
}

const transactionsPixelRange = [1, 7]

const predefinedCategories =
  categories.reduce((obj, category) => {
    switch (category) {
      case 'Darknet crawl':
        obj[category] = chromaStep * 0
        break
      case 'Exchanges':
        obj[category] = chromaStep * 1
        break
      case 'Gambling':
        obj[category] = chromaStep * 2
        break
      case 'Miner':
        obj[category] = chromaStep * 3
        break
      case 'Old/historic':
        obj[category] = chromaStep * 4
        break
      case 'Organization':
        obj[category] = chromaStep * 5
        break
      case 'Pools':
        obj[category] = chromaStep * 6
        break
      case 'Services/others':
        obj[category] = chromaStep * 7
        break
    }
    return obj
  }, {})

const maxNumSnapshots = 4

const createColor = (chroma, type) => {
  return hsl2rgb(chroma, saturation, lightness[type])
}

export default class NodeGraph extends Component {
  constructor (dispatcher, labelType, currency, txLabelType) {
    super()
    this.currency = currency
    this.dispatcher = dispatcher
    this.labelType = labelType
    this.txLabelType = txLabelType
    this.clusterNodes = map()
    this.addressNodes = map()
    this.adding = set()
    this.layers = []
    this.transform = {k: 1, x: 0, y: 0, dx: 0, dy: 0}
    this.colorMapCategories = map(predefinedCategories)
    this.colorMapTags = map()
    this.colorGen = (map, type) => {
      return (k) => {
        if (!k) return defaultColor[type]
        let chroma = map.get(k)
        if (chroma === undefined) {
          chroma = map.size() * chromaStep
          map.set(k, chroma)
        }
        logger.debug('colorGen', type, k, chroma)
        return createColor(chroma, type)
      }
    }
    this.colors =
      {
        'cluster': {
          categories: this.colorGen(this.colorMapCategories, 'cluster'),
          tags: this.colorGen(this.colorMapTags, 'cluster'),
          range: (v) => defaultColor['cluster']
        },
        'address': {
          categories: this.colorGen(this.colorMapCategories, 'address'),
          tags: this.colorGen(this.colorMapTags, 'address'),
          range: (v) => defaultColor['address']
        }
      }
    this.snapshots = []
    this.currentSnapshotIndex = -1
    // initialize with true to allow initial snapshot
    this.dirty = true
    this.createSnapshot()
  }
  getCategoryColors () {
    let colors = {}
    for (let cat in predefinedCategories) {
      colors[cat] = createColor(predefinedCategories[cat], 'cluster')
    }
    return colors
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
    this.addressNodes.clear()
    this.clusterNodes.clear()
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
    } else if (type === 'cluster') {
      nodes = this.clusterNodes
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
    let cluster = layer.nodes.get(id)
    if (!cluster) return

    dx /= this.transform.k
    dy /= this.transform.k

    cluster.ddx += dx
    cluster.ddy += dy

    if (cluster.ddx - 2 * expandHandleWidth < margin / -2) return
    if (cluster.ddx + 2 * expandHandleWidth > margin / 2) return

    let nodes = layer.nodes.values()
    let x = cluster.x + cluster.ddx - expandHandleWidth
    let y = cluster.y + cluster.ddy
    let cw = cluster.getWidthForLinks()
    let ch = cluster.getHeightForLinks()
    for (let i = 0; i < nodes.length; i++) {
      let sister = nodes[i]
      if (sister === cluster) continue
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
    cluster.dx = cluster.ddx
    cluster.dy = cluster.ddy
    this.setUpdate('layers')
  }
  dragNodeEnd (id, type) {
    let cluster = this.clusterNodes.get(id)
    if (!cluster) return
    cluster.ddx = cluster.dx
    cluster.ddy = cluster.dy
    this.dirty = true
  }
  sortClusterAddresses (id, property) {
    let cluster = this.clusterNodes.get(id)
    logger.debug('sort addresses cluster', cluster)
    if (!cluster) return
    cluster.sortAddresses(property)
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
  }
  setTxLabel (type) {
    this.txLabelType = type
    this.setUpdate('links')
  }
  setCurrency (currency) {
    this.currency = currency
    this.addressNodes.each(node => node.setCurrency(currency))
    this.clusterNodes.each(node => node.setCurrency(currency))
    if (this.txLabelType === 'estimatedValue') {
      this.setUpdate('links')
    }
  }
  setClusterLabel (labelType) {
    this.labelType['clusterLabel'] = labelType
    this.clusterNodes.each((node) => {
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
    this.selectedNode = sel
  }
  setResultClusterAddresses (id, addresses) {
    let cluster = this.clusterNodes.get(id)
    addresses.forEach((address) => {
      if (this.addressNodes.has([address.id, id[1], address.keyspace])) return
      let addressNode = new AddressNode(this.dispatcher, address, id[1], this.labelType['addressLabel'], this.colors['address'], this.currency)
      logger.debug('new AddressNode', addressNode)
      this.addressNodes.set(addressNode.id, addressNode)
      cluster.add(addressNode)
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
    } else if (anchor.nodeType === 'cluster' && object.type === 'address') {
      layerIds = anchor.nodeId[1]
    } else {
      // TODO is this safe? Are layer ids consecutive?
      layerIds = anchor.nodeId[1] + (anchor.isOutgoing ? 1 : -1)
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
      this.selectNodeIfIsNextNode(addressNode)
      logger.debug('new AddressNode', addressNode)
      this.addressNodes.set(addressNode.id, addressNode)
      node = this.clusterNodes.get([object.cluster.id, layerId, object.cluster.keyspace])
      if (!node) {
        node = new ClusterNode(this.dispatcher, object.cluster, layerId, this.labelType['clusterLabel'], this.colors['cluster'], this.currency)
      }
      node.add(addressNode)
    } else if (object.type === 'cluster') {
      node = this.clusterNodes.get([object.id, layerId, object.keyspace])
      if (node) {
        this.selectNodeIfIsNextNode(node)
        return node
      }
      node = new ClusterNode(this.dispatcher, object, layerId, this.labelType['clusterLabel'], this.colors['cluster'], this.currency)
      logger.debug('new ClusterNode', node)
      this.selectNodeIfIsNextNode(node)
    } else {
      throw Error('unknown node type')
    }
    this.clusterNodes.set(node.id, node)
    this.dirty = true

    let anchorLayer = anchor && this.findLayer(anchor.nodeId[1])

    let addToTop = anchorLayer && anchorLayer.isNodeInUpperHalf(anchor.nodeId)

    layer.add(node, addToTop)
    return node
  }
  remove (nodeType, nodeId) {
    logger.debug('remove', nodeType, nodeId)
    let nodes
    if (nodeType === 'address') {
      nodes = this.addressNodes
    } else if (nodeType === 'cluster') {
      nodes = this.clusterNodes
    }
    let node = nodes.get(nodeId)
    nodes.remove(nodeId)
    if (this.selectedNode === node) {
      this.selectedNode = null
    }
    let layer = this.findLayer(nodeId[1])
    logger.debug('remove layer', nodeId, layer)
    if (!layer) return
    if (nodeType === 'address') {
      this.clusterNodes.remove('mockup' + nodeId)
      layer.nodes.each(cluster => {
        cluster.nodes.remove(nodeId)
      })
    } else if (nodeType === 'cluster') {
      node.nodes.each(node => this.addressNodes.remove(node.id))
      this.clusterNodes.remove(nodeId)
      if (layer.nodes.size() === 0) {
        this.layers = this.layers.filter(l => l !== layer)
      } else {
        layer.remove(nodeId)
      }
    }
    this.setUpdate('layers')
  }
  removeClusterAddresses (id) {
    let cluster = this.clusterNodes.get(id)
    if (!cluster) return
    cluster.nodes.each((address) => this.remove('address', address.id))
  }
  additionLayerBySelection (addressId) {
    if (!addressId) return false
    if (!(this.selectedNode instanceof ClusterNode)) return false
    let cluster = this.selectedNode.data
    if (!cluster.addresses.has(addressId)) return false
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
        if (node.type === 'cluster' && node.outgoing.has(nodes[j].data.id)) {
          logger.debug('select layer by incoming node', nodes[j], this.layers[0].id - 1)
          ids.add(this.layers[0].id - 1)
        }
        if (node.cluster && node.cluster.outgoing.has(nodes[j].data.id)) {
          logger.debug('select layer by incoming node on cluster level', nodes[j], this.layers[0].id - 1)
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
        if (node.type === 'cluster' && outgoing.has(node.id)) {
          logger.debug('select layer by outgoing node', nodes[j], this.layers[i].id + 1)
          ids.add(this.layers[i].id + 1)
        }
        if (node.cluster && outgoing.has(node.cluster.id)) {
          logger.debug('select layer by outgoing node on cluster level', nodes[j], this.layers[i].id + 1)
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
    if (!node.cluster) return false
    for (let i = 0; i < this.layers.length; i++) {
      if (this.layers[i].has([node.cluster.id, this.layers[i].id])) {
        logger.debug('select layer by cluster', this.layers[i], this.layers[i].id)
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
    let clusterRoot, clusterShadowsRoot, addressShadowsRoot, addressRoot, linksRoot
    logger.debug('graph should update', this.shouldUpdate())
    let transformGraph = () => {
      let x = this.transform.x + this.transform.dx * this.transform.k
      let y = this.transform.y + this.transform.dy * this.transform.k
      this.graphRoot.attr('transform', `translate(${x}, ${y}) scale(${this.transform.k})`)
    }
    if (this.shouldUpdate(true)) {
      let svg = create('svg')
        .classed('w-full h-full graph', true)
        .attr('viewBox', `${x} ${y} ${w} ${h}`)
        .attr('preserveAspectRatio', 'xMidYMid meet')
        .attr('xmlns', 'http://www.w3.org/2000/svg')
        .call(drag().on('drag', () => {
          this.transform.dx += event.dx
          this.transform.dy += event.dy
          transformGraph()
        }))
        .call(zoom().on('zoom', () => {
          this.transform.k = event.transform.k
          this.transform.x = event.transform.x
          this.transform.y = event.transform.y
          transformGraph()
        }))
      let markerHeight = transactionsPixelRange[1]
      this.arrowSummit = markerHeight
      svg.node().innerHTML = '<defs>' +
        (['black', 'red'].map(color =>
          `<marker id="arrow1-${color}" markerWidth="${this.arrowSummit}" markerHeight="${markerHeight}" refX="0" refY="${markerHeight / 2}" orient="auto" markerUnits="userSpaceOnUse">` +
           `<path d="M0,0 L0,${markerHeight} L${this.arrowSummit},${markerHeight / 2} Z" style="fill: ${color};" />` +
         '</marker>'
        )).join('') +
        '</defs>'
      this.graphRoot = svg.append('g')
      transformGraph()
      svg.on('click', () => {
        this.dispatcher('deselect')
      })
      this.root.appendChild(svg.node())

      clusterShadowsRoot = this.graphRoot.append('g').classed('clusterShadowsRoot', true)
      clusterRoot = this.graphRoot.append('g').classed('clusterRoot', true)
      addressShadowsRoot = this.graphRoot.append('g').classed('addressShadowsRoot', true)
      linksRoot = this.graphRoot.append('g').classed('linksRoot', true)
      addressRoot = this.graphRoot.append('g').classed('addressRoot', true)
    } else {
      clusterShadowsRoot = this.graphRoot.select('g.clusterShadowsRoot')
      addressShadowsRoot = this.graphRoot.select('g.addressShadowsRoot')
      linksRoot = this.graphRoot.select('g.linksRoot')
      clusterRoot = this.graphRoot.select('g.clusterRoot')
      addressRoot = this.graphRoot.select('g.addressRoot')
    }
    // render in this order
    this.renderLayers(clusterRoot, addressRoot)
    this.renderLinks(linksRoot)
    this.renderShadows(clusterShadowsRoot, addressShadowsRoot)
    super.render()
    return this.root
  }
  renderLayers (clusterRoot, addressRoot, transform) {
    if (this.shouldUpdate('layers')) {
      clusterRoot.node().innerHTML = ''
      addressRoot.node().innerHTML = ''
      this.layers
        .forEach((layer) => {
          if (layer.nodes.size() === 0) return
          layer.setUpdate(true)
          let cRoot = clusterRoot.append('g')
          let aRoot = addressRoot.append('g')
          layer.render(cRoot, aRoot)
          // let first = layer.getFirst()
          // box.height += first.dy
          // let last = layer.getLast()
          // if (last !== first) {
          // box.height -= last.dy
          // }
          let layerHeight = layer.getHeight()
          let w = clusterWidth + margin
          let x = layer.id * w
          let y = layerHeight / -2
          cRoot.attr('transform', `translate(${x}, ${y})`)
          aRoot.attr('transform', `translate(${x}, ${y})`)
          layer.translate(x, y)
        })
    } else {
      this.layers.forEach((layer) => {
        if (layer.nodes.size() === 0) return
        layer.render()
      })
    }
  }
  renderLinks (root) {
    if (!this.shouldUpdate('layers') && !this.shouldUpdate('links')) return
    root.node().innerHTML = ''
    const link = linkHorizontal()
      .x(([node, isSource, scale]) => isSource ? node.getXForLinks() + node.getWidthForLinks() : node.getXForLinks() - this.arrowSummit)
      .y(([node, isSource, scale]) => node.getYForLinks() + node.getHeightForLinks() / 2)

    for (let i = 0; i < this.layers.length; i++) {
      // prepare the domain and links
      let domain = [1 / 0, 0]
      let clusterLinksFromAddresses = {}
      this.layers[i].nodes.each((c) => {
        // stores links between neighbor clusters resulting from address links
        clusterLinksFromAddresses[c.data.id] = set()
        c.nodes.each((a) => {
          this.prepareLinks(domain, this.layers[i + 1], a)
            .forEach(cl => clusterLinksFromAddresses[c.data.id].add(cl))
        })
        this.prepareClusterLinks(domain, this.layers[i + 1], c, clusterLinksFromAddresses[c.data.id])
      })
      // render links
      this.layers[i].nodes.each((c) => {
        c.nodes.each((a) => {
          this.linkToLayer(root, link, domain, this.layers[i + 1], a)
        })
        this.linkToLayerCluster(root, link, domain, this.layers[i + 1], c, clusterLinksFromAddresses[c.data.id])
      })
    }
  }
  prepareLinks (domain, layer, address) {
    let neighbors = address.data.outgoing
    let clusterLinks = []
    if (layer) {
      layer.nodes.each((cluster2) => {
        let hasLinks = false
        cluster2.nodes.each((address2) => {
          let ntx = neighbors.get(address2.data.id)
          if (ntx === undefined) return
          this.updateDomain(domain, this.findValueAndLabel(ntx)[0])
          hasLinks = true
        })
        if (hasLinks) {
          clusterLinks.push(cluster2.data.id)
        }
      })
    }
    return clusterLinks
  }
  prepareClusterLinks (domain, layer, source, clusterLinksFromAddresses) {
    let neighbors = source.data.outgoing
    if (layer) {
      layer.nodes.each((cluster2) => {
        let ntx = neighbors.get(cluster2.data.id)
        if (ntx === undefined) return
        // skip cluster if contains in clusterLinksFromAddresses
        if (clusterLinksFromAddresses.has(cluster2.data.id)) return
        this.updateDomain(domain, this.findValueAndLabel(ntx)[0])
      })
    }
  }
  updateDomain (domain, value) {
    domain[0] = Math.min(domain[0], value)
    domain[1] = Math.max(domain[1], value)
  }
  linkToLayer (root, link, domain, layer, address) {
    let neighbors = address.data.outgoing
    if (layer) {
      layer.nodes.each((cluster2) => {
        cluster2.nodes.each((address2) => {
          let ntx = neighbors.get(address2.data.id)
          if (ntx === undefined) return
          this.renderLink(root, link, domain, address, address2, ntx)
        })
      })
    }
  }
  linkToLayerCluster (root, link, domain, layer, source, clusterLinksFromAddresses) {
    let neighbors = source.data.outgoing
    if (layer) {
      layer.nodes.each((cluster2) => {
        let ntx = neighbors.get(cluster2.data.id)
        if (ntx === undefined) return
        // skip cluster if contains in clusterLinksFromAddresses
        if (clusterLinksFromAddresses.has(cluster2.data.id)) return
        this.renderLink(root, link, domain, source, cluster2, ntx)
      })
    }
  }
  renderShadows (clusterRoot, addressRoot) {
    if (!this.shouldUpdate()) return
    clusterRoot.node().innerHTML = ''
    addressRoot.node().innerHTML = ''
    const link = linkHorizontal()
      .x(([node, isSource]) => isSource ? node.getXForLinks() + node.getWidthForLinks() : node.getXForLinks())
      .y(([node, isSource]) => node.getYForLinks() + node.getHeightForLinks() / 2)
    // TODO use a data structure which stores and lists entries in sorted order to prevent this sorting
    let sort = (node1, node2) => {
      return node1.id[1] - node2.id[1]
    }
    this.linkShadows(addressRoot, link, this.addressNodes.values().sort(sort))
    this.linkShadows(clusterRoot, link, this.clusterNodes.values().sort(sort))
  }
  linkShadows (root, link, nodes) {
    nodes.forEach((node1) => {
      for (let i = 0; i < nodes.length; i++) {
        let node2 = nodes[i]
        if (node1 === node2) continue
        if (node1.id[0] !== node2.id[0]) continue
        if (node1.id[1] >= node2.id[1]) continue
        let path = link({source: [node1, true], target: [node2, false]})
        root.append('path').classed('shadow', true).attr('d', path)
        // stop iterating if a shadow to next layer was found
        return
      }
    })
  }
  renderLink (root, link, domain, source, target, tx) {
    let value, label
    [value, label] = this.findValueAndLabel(tx)
    let scale = scalePow().domain(domain).range(transactionsPixelRange)(value)
    let path = link({source: [source, true, scale], target: [target, false, scale]})
    let g1 = root.append('g').classed('link', true)
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
    if (this.txLabelType === 'estimatedValue') {
      value = tx[this.txLabelType].satoshi
      label = formatCurrency(tx[this.txLabelType][this.currency], this.currency, {dontAppendCurrency: true, keyspace: tx.keyspace})
    } else if (this.txLabelType === 'noTransactions') {
      value = label = tx[this.txLabelType]
    } else {
      value = 0
      label = '?'
    }
    return [value, label]
  }
  serializeGraph () {
    let clusterNodes = []
    this.clusterNodes.each(node => clusterNodes.push([node.id, node.serialize()]))

    let addressNodes = []
    this.addressNodes.each(node => addressNodes.push([node.id, node.serialize()]))

    let layers = []
    this.layers.forEach(layer => layers.push(layer.serialize()))
    return [clusterNodes, addressNodes, layers]
  }
  serialize () {
    let clusterNodes, addressNodes, layers

    [clusterNodes, addressNodes, layers] = this.serializeGraph()

    return [
      this.currency,
      this.labelType,
      this.txLabelType,
      this.transform,
      clusterNodes,
      addressNodes,
      layers,
      this.colorMapCategories.entries(),
      this.colorMapTags.entries()
    ]
  }
  deserializeGraph (version, store, clusterNodes, addressNodes, layers) {
    addressNodes.forEach(([nodeId, address]) => {
      if (version === '0.4.0') {
        let found = store.find(nodeId[0], 'address')
        if (!found) return
        nodeId[2] = found.keyspace
      }
      let data = store.get(nodeId[2], 'address', nodeId[0])
      let node = new AddressNode(this.dispatcher, data, nodeId[1], this.labelType['addressLabel'], this.colors['address'], this.currency)
      node.deserialize(address)
      this.addressNodes.set(node.id, node)
    })
    clusterNodes.forEach(([nodeId, cluster]) => {
      if (version === '0.4.0') {
        let found = store.find(nodeId[0], 'cluster')
        if (!found) return
        nodeId[2] = found.keyspace
      }
      let data = store.get(nodeId[2], 'cluster', nodeId[0])
      let node = new ClusterNode(this.dispatcher, data, nodeId[1], this.labelType['clusterLabel'], this.colors['cluster'], this.currency)
      node.deserialize(version, cluster, this.addressNodes)
      this.clusterNodes.set(node.id, node)
    })
    layers.forEach(([id, clusterKeys]) => {
      let l = new Layer(id)
      clusterKeys.forEach(key => {
        if (version === '0.4.0') {
          let found = null
          this.clusterNodes.each(node => {
            if (!found && ([node.id[0], node.id[1]]).join(',') === key) found = node
          })
          if (!found) return
          key = found.id
        }
        l.add(this.clusterNodes.get(key))
      })
      this.layers.push(l)
    })
  }
  deserialize (version, [
    currency,
    labelType,
    txLabelType,
    transform,
    clusterNodes,
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
    this.deserializeGraph(version, store, clusterNodes, addressNodes, layers)
  }
}
