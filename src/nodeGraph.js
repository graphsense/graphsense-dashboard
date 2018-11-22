import {create, event} from 'd3-selection'
import {set} from 'd3-collection'
import {linkHorizontal} from 'd3-shape'
import {drag} from 'd3-drag'
import {zoom} from 'd3-zoom'
import Layer from './nodeGraph/layer.js'
import ClusterNode from './nodeGraph/clusterNode.js'
import AddressNode from './nodeGraph/addressNode.js'
import Component from './component.js'
import RMap from './rmap.js'

const margin = 200
const x = -300
const y = -300
const w = 600
const h = 600

export default class NodeGraph extends Component {
  constructor (dispatcher, labelType) {
    super()
    this.dispatcher = dispatcher
    this.labelType = labelType
    this.clusterNodes = new RMap()
    this.addressNodes = new RMap()
    this.adding = set()
    this.layers = []
    this.viewBox = {x, y, w, h}
  }
  setResultNode (object) {
    let nodes
    if (object.type === 'address') {
      nodes = this.addressNodes
    } else if (object.type === 'cluster') {
      nodes = this.clusterNodes
    }
    nodes.each(node => {
      if (node.data.id === object.id) {
        node.shouldUpdate(true)
      }
    })
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
      sel.select()
      if (this.selectedNode && this.selectedNode !== sel) {
        this.selectedNode.deselect()
      }
      this.selectedNode = sel
    }
  }
  setResultClusterAddresses (id, addresses) {
    let cluster = this.clusterNodes.get(id)
    addresses.forEach((address) => {
      if (this.addressNodes.has([address.id, id[1]])) return
      let addressNode = new AddressNode(this.dispatcher, address, id[1], this.labelType['addressLabel'])
      console.log('new AddressNode', addressNode)
      this.addressNodes.set(addressNode.id, addressNode)
      cluster.add(addressNode)
    })
    this.shouldUpdate('layers')
  }
  resultNode (object) {
  }
  findAddressNode (address, layerId) {
    return this.addressNodes.get([address, layerId])
  }
  add (object, anchor) {
    this.adding.remove(object.id)
    let layerId
    if (!anchor) {
      layerId = this.additionLayerBySelection(object.id)
      layerId = false
      if (layerId === false) layerId = this.additionLayerBySearch(object)
      layerId = layerId || 0
    } else {
      layerId = anchor.nodeId[1] + (anchor.isOutgoing ? 1 : -1)
    }
    console.log('add', object, layerId)
    let filtered = this.layers.filter(({id}) => id === layerId)
    let layer
    if (filtered.length === 0) {
      layer = new Layer(layerId)
      if (anchor && anchor.isOutgoing === false) {
        this.layers.unshift(layer)
      } else {
        this.layers.push(layer)
      }
    } else {
      layer = filtered[0]
    }
    let node
    if (object.type === 'address') {
      if (this.addressNodes.has([object.address, layerId])) return
      let addressNode = new AddressNode(this.dispatcher, object, layerId, this.labelType['addressLabel'])
      console.log('new AddressNode', addressNode)
      this.addressNodes.set(addressNode.id, addressNode)
      node = this.clusterNodes.get([object.cluster.id, layerId])
      if (!node) {
        node = new ClusterNode(this.dispatcher, object.cluster, layerId, this.labelType['clusterLabel'])
      }
      node.add(addressNode)
    } else if (object.type === 'cluster') {
      if (this.clusterNodes.has([object.cluster, layerId])) return
      node = new ClusterNode(this.dispatcher, object, layerId, this.labelType['clusterLabel'])
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
    node.setRemoved(true)
    if (nodeType === 'address') {
      let mockup = this.clusterNodes.get('mockup' + nodeId)
      if (mockup) {
        mockup.setRemoved(true)
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
    console.log('search', node.cluster)
    for (let i = 0; i < this.layers.length; i++) {
      console.log('searching layer', this.layers[i])
      if (this.layers[i].has(node.cluster)) {
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
    if (!this.shouldUpdate()) return
    root.node().innerHTML = ''
    const link = linkHorizontal()
      .x(([node, isSource]) => isSource ? node.getXForLinks() + node.getWidthForLinks() : node.getXForLinks())
      .y(([node, isSource]) => node.getYForLinks() + node.getHeightForLinks() / 2)
    for (let i = 0; i < this.layers.length; i++) {
      this.layers[i].nodes.each((c) => {
        this.linkToLayerCluster(root, link, this.layers[i + 1], c.data.outgoing, c, true)
        c.nodes.each((a) => {
          this.linkToLayer(root, link, this.layers[i + 1], a.data.outgoing, a, true)
        })
      })
    }
  }
  linkToLayer (root, link, layer, neighbors, source, isOutgoing) {
    if (layer) {
      layer.nodes.each((cluster2) => {
        cluster2.nodes.each((address2) => {
          if (!neighbors.has(address2.data.id)) return
          let path = link({source: [source, isOutgoing], target: [address2, !isOutgoing]})
          root.append('path').classed('link', true).attr('d', path)
        })
      })
    }
  }
  linkToLayerCluster (root, link, layer, neighbors, source, isOutgoing) {
    if (layer) {
      layer.nodes.each((cluster2) => {
        if (!neighbors.has(cluster2.data.id)) return
        let path = link({source: [source, isOutgoing], target: [cluster2, !isOutgoing]})
        root.append('path').classed('link', true).attr('d', path)
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
    console.log('sorted', this.addressNodes, this.addressNodes.values())// .sort(sort))
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
}
