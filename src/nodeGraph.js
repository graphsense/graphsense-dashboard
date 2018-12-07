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

const margin = 300
const x = -300
const y = -300
const w = 800
const h = 600

const chromaStep = 36
const saturation = 94 / 255 * 100
const lightness = {
  'cluster': 209 / 255 * 100,
  'address': 230 / 255 * 100
}
const defaultColor = {
  'cluster': `hsl(178, 0%, ${lightness['cluster']}%)`,
  'address': `hsl(178, 0%, ${lightness['address']}%)`
}

const transactionsPixelRange = [1, 7]

const predefinedCategories = {
  'Darknet crawl': chromaStep * 1,
  'Exchange': chromaStep * 2,
  'Exchanges': chromaStep * 2,
  'Gambling': chromaStep * 3,
  'Miner': chromaStep * 4,
  'Old/historic': chromaStep * 5,
  'Organization': chromaStep * 6,
  'Pools': chromaStep * 7,
  'Services/others': chromaStep * 8
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
    this.viewBox = {x, y, w, h}
    this.colorMapCategories = map(predefinedCategories)
    this.colorMapTags = map()
    this.colorGen = (map, type) => {
      return (k) => {
        if (!k) return defaultColor[type]
        k = 'k' + k
        let chroma = map.get(k)
        if (chroma === undefined) {
          chroma = map.size() * chromaStep
          map.set(k, chroma)
        }
        return `hsl(${chroma}, ${saturation}%, ${lightness[type]}%)`
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
  }
  deselect () {
    if (!this.selectedNode) return
    this.selectedNode.deselect()
    this.selectedNode.shouldUpdate('select')
    this.selectedNode = null
  }
  selectNodeWhenLoaded ([id, type]) {
    this.nextSelectedNode = {id, type}
  }
  selectNodeIfIsNextNode (node) {
    console.log('selectNodeIfIsNextNode', node, this.nextSelectedNode)
    if (!this.nextSelectedNode) return
    if (this.nextSelectedNode.type !== node.data.type) return
    if (this.nextSelectedNode.id != node.data.id) return // eslint-disable-line eqeqeq
    this._selectNode(node)
    this.nextSelectedNode = null
  }
  setTxLabel (type) {
    this.txLabelType = type
    this.shouldUpdate('links')
  }
  setCurrency (currency) {
    this.currency = currency
    this.addressNodes.each(node => node.setCurrency(currency))
    this.clusterNodes.each(node => node.setCurrency(currency))
    if (this.txLabelType === 'estimatedValue') {
      this.shouldUpdate('links')
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
    let nodes
    if (type === 'address') {
      nodes = this.addressNodes
    } else if (type === 'cluster') {
      nodes = this.clusterNodes
    }
    let sel = nodes.get(nodeId)
    if (sel) {
      this._selectNode(sel)
    }
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
      if (this.addressNodes.has([address.id, id[1]])) return
      let addressNode = new AddressNode(this.dispatcher, address, id[1], this.labelType['addressLabel'], this.colors['address'], this.currency)
      console.log('new AddressNode', addressNode)
      this.addressNodes.set(addressNode.id, addressNode)
      cluster.add(addressNode)
    })
    this.shouldUpdate('layers')
  }
  findAddressNode (address, layerId) {
    return this.addressNodes.get([address, layerId])
  }
  add (object, anchor) {
    console.log('add', object, anchor)
    this.adding.remove(object.id)
    let layerId
    if (!anchor) {
      layerId = this.additionLayerBySelection(object.id)
      if (layerId === false) layerId = this.additionLayerBySearch(object)
      layerId = layerId || 0
    } else {
      layerId = anchor.nodeId[1] + (anchor.isOutgoing ? 1 : -1)
    }
    console.log('layer', layerId)
    let filtered = this.layers.filter(({id}) => id === layerId)
    let layer
    if (filtered.length === 0) {
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
    } else {
      layer = filtered[0]
    }
    let node
    if (object.type === 'address') {
      let addressNode = this.addressNodes.get([object.id, layerId])
      if (addressNode) {
        this.selectNodeIfIsNextNode(addressNode)
        return
      }
      addressNode = new AddressNode(this.dispatcher, object, layerId, this.labelType['addressLabel'], this.colors['address'], this.currency)
      this.selectNodeIfIsNextNode(addressNode)
      console.log('new AddressNode', addressNode)
      this.addressNodes.set(addressNode.id, addressNode)
      node = this.clusterNodes.get([object.cluster.id, layerId])
      if (!node) {
        node = new ClusterNode(this.dispatcher, object.cluster, layerId, this.labelType['clusterLabel'], this.colors['cluster'], this.currency)
      }
      node.add(addressNode)
    } else if (object.type === 'cluster') {
      node = this.clusterNodes.get([object.id, layerId])
      if (node) {
        this.selectNodeIfIsNextNode(node)
        return
      }
      node = new ClusterNode(this.dispatcher, object, layerId, this.labelType['clusterLabel'], this.colors['cluster'], this.currency)
      console.log('new ClusterNode', node)
      this.selectNodeIfIsNextNode(node)
    } else {
      throw Error('unknown node type')
    }
    this.clusterNodes.set(node.id, node)

    layer.add(node)
    this.shouldUpdate('layers')
  }
  remove (nodeType, nodeId) {
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
    let layer = this.layers.filter(l => l.id === nodeId[1])[0]
    console.log('remove layer', nodeId, layer)
    if (nodeType === 'address') {
      this.clusterNodes.remove('mockup' + nodeId)
      layer.nodes.each(cluster => {
        cluster.nodes.remove(nodeId)
      })
    } else if (nodeType === 'cluster') {
      node.nodes.each(node => this.addressNodes.remove(node.id))
      layer.nodes.remove(nodeId)
      if (layer.nodes.size() === 0) {
        this.layers = this.layers.filter(l => l !== layer)
      }
    }
    this.shouldUpdate('layers')
  }
  additionLayerBySelection (addressId) {
    if (!addressId) return false
    if (!(this.selectedNode instanceof ClusterNode)) return false
    let cluster = this.selectedNode.data
    if (!cluster.addresses.has(addressId)) return false
    return this.selectedNode.id[1]
  }
  additionLayerBySearch (node) {
    console.log('search', node)
    if (this.selectedNode && this.selectedNode.data.outgoing.has(node.id)) {
      console.log('select layer by selected node')
      return this.selectedNode.id[1] + 1
    }
    if (this.selectedNode && node.outgoing.has(this.selectedNode.data.id)) {
      console.log('select layer by selected node (incoming)')
      return this.selectedNode.id[1] - 1
    }

    if (this.layers[0]) {
      let nodes = this.layers[0].nodes.values()
      for (let j = 0; j < nodes.length; j++) {
        if (node.type === 'cluster' && node.outgoing.has(nodes[j].data.id)) {
          console.log('select layer by incoming node', nodes[j])
          return this.layers[0].id - 1
        }
        if (node.cluster && node.cluster.outgoing.has(nodes[j].data.id)) {
          console.log('select layer by incoming node on cluster level', nodes[j])
          return this.layers[0].id - 1
        }
        if (node.type === 'address') {
          let addresses = nodes[j].nodes.values()
          for (let k = 0; k < addresses.length; k++) {
            if (node.outgoing.has(addresses[k].data.id)) {
              console.log('select layer by incoming node on address level', addresses[k])
              return this.layers[0].id - 1
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
          console.log('select layer by outgoing node', nodes[j])
          return this.layers[i].id + 1
        }
        if (node.cluster && outgoing.has(node.cluster.id)) {
          console.log('select layer by outgoing node on cluster level', nodes[j])
          return this.layers[i].id + 1
        }
        if (node.type === 'address') {
          let addresses = nodes[j].nodes.values()
          for (let k = 0; k < addresses.length; k++) {
            if (addresses[k].data.outgoing.has(node.id)) {
              console.log('select layer by outgoing node on address level', addresses[k])
              return this.layers[i].id + 1
            }
          }
        }
      }
    }
    if (!node.cluster) return false
    for (let i = 0; i < this.layers.length; i++) {
      if (this.layers[i].has([node.cluster.id, this.layers[i].id])) {
        console.log('select layer by cluster', this.layers[i])
        return this.layers[i].id
      }
    }
    return false
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    let transform = {k: 1, x: 0, y: 0}
    let tx = 0
    let ty = 0
    let clusterRoot, clusterShadowsRoot, addressShadowsRoot, addressRoot, linksRoot
    console.log('graph should update', this.shouldUpdate())
    if (this.shouldUpdate() === true) {
      this.svg = create('svg')
        .classed('w-full h-full', true)
        .attr('viewBox', (({x, y, w, h}) => `${x} ${y} ${w} ${h}`)(this.viewBox))
        .attr('preserveAspectRatio', 'xMidYMid meet')
        .call(drag().on('drag', () => {
          tx -= event.dx / transform.k
          ty -= event.dy / transform.k
          let w_ = w / transform.k
          let h_ = h / transform.k
          let x_ = x + tx + (w - w_) / 2
          let y_ = y + ty + (h - h_) / 2
          this.svg.attr('viewBox', (({x, y, w, h}) => `${x_} ${y_} ${w_} ${h_}`)(this.viewBox))
        }))
        .call(zoom().on('zoom', () => {
        // store current zoom transform
          transform.k = event.transform.k
          transform.x = event.transform.x
          transform.y = event.transform.y
          let w_ = w / event.transform.k
          let h_ = h / event.transform.k
          let x_ = x + tx + (w - w_) / 2
          let y_ = y + ty + (h - h_) / 2
          this.svg.attr('viewBox', `${x_} ${y_} ${w_} ${h_}`)
        }))
      this.svg.on('click', () => {
        this.dispatcher('deselect')
      })
      this.root.innerHTML = ''
      this.root.appendChild(this.svg.node())
      clusterShadowsRoot = this.svg.append('g').classed('clusterShadowsRoot', true)
      clusterRoot = this.svg.append('g').classed('clusterRoot', true)
      addressShadowsRoot = this.svg.append('g').classed('addressShadowsRoot', true)
      linksRoot = this.svg.append('g').classed('linksRoot', true)
      addressRoot = this.svg.append('g').classed('addressRoot', true)
    } else {
      clusterShadowsRoot = this.svg.select('g.clusterShadowsRoot')
      addressShadowsRoot = this.svg.select('g.addressShadowsRoot')
      linksRoot = this.svg.select('g.linksRoot')
      clusterRoot = this.svg.select('g.clusterRoot')
      addressRoot = this.svg.select('g.addressRoot')
    }
    // render in this order
    this.renderLayers(clusterRoot, addressRoot)
    this.renderLinks(linksRoot)
    this.renderShadows(clusterShadowsRoot, addressShadowsRoot)
    super.render()
    return this.root
  }
  renderLayers (clusterRoot, addressRoot) {
    let cumX = 0
    if (this.shouldUpdate() === true || this.shouldUpdate() === 'layers') {
      clusterRoot.node().innerHTML = ''
      addressRoot.node().innerHTML = ''
      this.layers.forEach((layer) => {
        if (layer.nodes.size() === 0) return
        layer.shouldUpdate(true)
        let cRoot = clusterRoot.append('g')
        let aRoot = addressRoot.append('g')
        layer.render(cRoot, aRoot)
        let box = aRoot.node().getBBox()
        let x = cumX - box.width / 2
        let y = box.height / -2
        cRoot.attr('transform', `translate(${x}, ${y})`)
        aRoot.attr('transform', `translate(${x}, ${y})`)
        cumX = x + box.width + margin
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
    if (this.shouldUpdate() !== true && this.shouldUpdate() !== 'layers' && this.shouldUpdate() !== 'links') return
    root.node().innerHTML = ''
    const link = linkHorizontal()
      .x(([node, isSource]) => isSource ? node.getXForLinks() + node.getWidthForLinks() : node.getXForLinks())
      .y(([node, isSource]) => node.getYForLinks() + node.getHeightForLinks() / 2)

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
    let path = link({source: [source, true], target: [target, false]})
    let g1 = root.append('g').classed('link', true)
    g1.append('path').attr('d', path)
      .classed('frame', true)
    g1.append('path').attr('d', path)
      .style('stroke-width', scale + 'px')
    let sourceX = source.getXForLinks() + source.getWidthForLinks()
    let sourceY = source.getYForLinks() + source.getHeightForLinks() / 2
    let targetX = target.getXForLinks()
    let targetY = target.getYForLinks() + target.getHeightForLinks() / 2
    let fontSize = 10
    let x = (sourceX + targetX) / 2
    let y = (sourceY + targetY) / 2 + fontSize / 3
    let g2 = g1.append('g')

    let f = () => {
      return g2.append('text')
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
  serialize () {
    let clusterNodes = []
    this.clusterNodes.each(node => clusterNodes.push([node.id, node.serialize()]))

    let addressNodes = []
    this.addressNodes.each(node => addressNodes.push([node.id, node.serialize()]))

    let layers = []
    this.layers.forEach(layer => layers.push(layer.serialize()))

    return [
      this.currency,
      this.labelType,
      this.txLabelType,
      this.viewBox,
      clusterNodes,
      addressNodes,
      layers,
      this.colorMapCategories.entries(),
      this.colorMapTags.entries()
    ]
  }
  deserialize ([
    currency,
    labelType,
    txLabelType,
    viewBox,
    clusterNodes,
    addressNodes,
    layers,
    colorMapCategories,
    colorMapTags
  ], store) {
    this.currency = currency
    this.labelType = labelType
    this.txLabelType = txLabelType
    this.viewBox = viewBox
    colorMapCategories.forEach(({key, value}) => {
      this.colorMapCategories.set(key, value)
    })
    colorMapTags.forEach(({key, value}) => {
      this.colorMapTags.set(key, value)
    })
    addressNodes.forEach(([nodeId, address]) => {
      let data = store.get('address', nodeId[0])
      let node = new AddressNode(this.dispatcher, data, nodeId[1], this.labelType['addressLabel'], this.colors['address'], this.currency)
      node.deserialize(address)
      this.addressNodes.set(nodeId, node)
    })
    clusterNodes.forEach(([nodeId, cluster]) => {
      let data = store.get('cluster', nodeId[0])
      let node = new ClusterNode(this.dispatcher, data, nodeId[1], this.labelType['clusterLabel'], this.colors['cluster'], this.currency)
      node.deserialize(cluster, this.addressNodes)
      this.clusterNodes.set(nodeId, node)
    })
    layers.forEach(([id, clusterKeys]) => {
      let l = new Layer(id)
      clusterKeys.forEach(key => {
        l.add(this.clusterNodes.get(key))
      })
      this.layers.push(l)
    })
  }
}
