import {create, event} from 'd3-selection'
import {set, map} from 'd3-collection'
import {linkHorizontal} from 'd3-shape'
import {drag} from 'd3-drag'
import {zoom, zoomTransform} from 'd3-zoom'
import Layer from './nodeGraph/layer.js'
import ClusterNode from './nodeGraph/clusterNode.js'
import AddressNode from './nodeGraph/addressNode.js'

const margin = 200
const x = -300
const y = -300
const w = 600
const h = 600

export default class NodeGraph {
  constructor (dispatcher, store, labelType) {
    this.dispatcher = dispatcher
    this.store = store
    this.labelType = labelType
    this.clusterNodes = map()
    this.addressNodes = map()
    this.adding = set()
    this.layers = []
    this.viewBox = {x, y, w, h}
    this.dispatcher.on('addNode.graph', (request) => {
      let a = this.store.get(request.type, request.id)
      this.adding.add(request.id)
      if (!a) {
        this.dispatcher.call('loadNode', null, request)
        return
      }
      if (request.type === 'address' && !a.cluster) {
        this.dispatcher.call('loadClusterForAddress', null, request)
        return
      }
      if (!a.tags) {
        this.dispatcher.call('loadTags', null, {params: [request.id, request.type]})
      }
      this.add(a, request.anchorNode, request.isOutgoing)
    })
    this.dispatcher.on('resultTags.graph', (response) => {
      let nodeType = response.request.params[1]
      let nodeId = response.request.params[0]
      let fun = (type) => {
        return (node) => {
          if (node[type][type] === nodeId) {
            if (!node[type].tags) {
              node[type].tags = response.result.tags
            }
            node.renderLabel()
          }
        }
      }
      if (nodeType === 'address') {
        this.addressNodes.each(fun(nodeType))
      }
      if (nodeType === 'cluster') {
        this.clusterNodes.each(fun(nodeType))
      }
    })
    this.dispatcher.on('resultNode.graph', (response) => {
      if (!this.adding.has(response.result.address) && !this.adding.has(response.result.cluster)) return
      let o = this.store.add(response.result)
      if (response.request.type === 'address' && !o.cluster) {
        this.dispatcher.call('loadClusterForAddress', null, response.request)
        return
      }
      this.add(o, response.request.anchorNode, response.request.isOutgoing)
    })
    this.dispatcher.on('resultClusterForAddress.graph', (response) => {
      if (!this.adding.has(response.request.id)) return
      // merge address into cluster object for store
      this.store.add({...response.result, forAddress: response.request.id})
      let address = this.store.get('address', response.request.id)
      this.add(address, response.request.anchorNode, response.request.isOutgoing)
    })
    this.dispatcher.on('selectNode.graph', ([type, nodeId]) => {
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
    })
    this.dispatcher.on('resultEgonet.graph', ({type, id, isOutgoing, result}) => {
      let a = this.store.get(type, id[0])
      console.log('a', a)
      result.nodes.forEach((node) => {
        if (node.id === id[0] || node.nodeType !== type) return
        let request = {
          anchorNode: id,
          isOutgoing,
          type,
          id: node.id
        }
        if (isOutgoing) {
          a.outgoing.add(node.id)
        } else {
          a.incoming.add(node.id)
        }
        this.dispatcher.call('addNode', null, request)
      })
    })
    this.dispatcher.on('resultClusterAddresses.graph', ({id, result}) => {
      let node = this.clusterNodes.get(id)
      result.addresses.forEach((address) => {
        address.cluster = id[0]
        let object = this.store.add(address)
        if (!object.tags) {
          this.dispatcher.call('loadTags', null, {params: [object.address, 'address']})
        }
        if (this.addressNodes.has([address, id[1]])) return
        let addressNode = new AddressNode(object, id[1], this.labelType['addressLabel'], this)
        console.log('new AddressNode', addressNode)
        this.addressNodes.set(addressNode.id, addressNode)
        node.add(addressNode.id[0])
      })
      this.clear()
      this.render()
      console.log(this)
    })
    this.dispatcher.on('changeClusterLabel', (labelType) => {
      this.labelType['clusterLabel'] = labelType
      this.clusterNodes.each((node) => {
        node.setLabelType(labelType)
        node.renderLabel()
      })
    })
    this.dispatcher.on('changeAddressLabel', (labelType) => {
      this.labelType['addressLabel'] = labelType
      this.addressNodes.each((node) => {
        node.setLabelType(labelType)
        node.renderLabel()
      })
    })
  }
  findAddressNode (address, layerId) {
    return this.addressNodes.get([address, layerId])
  }
  add (object, anchorNode, isOutgoing) {
    this.adding.remove(object.address || object.cluster)
    let layerId
    if (!anchorNode) {
      layerId = this.additionLayerBySelection(object)
      layerId = false
      if (layerId === false) layerId = this.additionLayerBySearch(object)
      layerId = layerId || 0
    } else {
      layerId = anchorNode[1] + (isOutgoing ? 1 : -1)
    }
    console.log('add', object, layerId)
    let filtered = this.layers.filter(({id}) => id === layerId)
    let layer
    if (filtered.length === 0) {
      layer = new Layer(this, layerId)
      if (isOutgoing === false) {
        this.layers.unshift(layer)
      } else {
        this.layers.push(layer)
      }
      console.log('pushing in layers', this.layers)
    } else {
      layer = filtered[0]
    }
    let node
    if (object.address) {
      if (this.addressNodes.has([object.address, layerId])) return
      let addressNode = new AddressNode(object, layerId, this.labelType['addressLabel'], this)
      console.log('new AddressNode', addressNode)
      this.addressNodes.set(addressNode.id, addressNode)
      node = this.clusterNodes.get([object.cluster, layerId])
      if (!node) {
        let cluster = this.store.get('cluster', object.cluster)
        node = new ClusterNode(cluster, layerId, this.labelType['clusterLabel'], this)
      }
      node.add(addressNode.id[0])
    } else if (object.cluster) {
      if (this.clusterNodes.has([object.cluster, layerId])) return
      node = new ClusterNode(object, layerId, this.labelType['clusterLabel'], this)
    }
    this.clusterNodes.set(node.id, node)

    layer.add(node.id[0])
    this.clear()
    this.render()
  }
  additionLayerBySelection (node) {
    if (!node.address) return false
    if (!(this.selectedNode instanceof ClusterNode)) return false
    let cluster = this.store.get('cluster', this.selectedNode.id[0])
    if (!cluster.addresses.has(node.address)) return false
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
  clear () {
    this.root.node().innerHTML = ''
  }
  render () {
    let transform = {k: 1, x: 0, y: 0}
    let tx = 0
    let ty = 0
    this.root = this.root || create('svg')
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
        this.root.attr('viewBox', (({x, y, w, h}) => `${x_} ${y_} ${w_} ${h_}`)(this.viewBox))
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
        this.root.attr('viewBox', `${x_} ${y_} ${w_} ${h_}`)
      }))
    let clusterShadowsRoot = this.root.append('g')
    let clusterRoot = this.root.append('g')
    let addressShadowsRoot = this.root.append('g')
    let linksRoot = this.root.append('g')
    let addressRoot = this.root.append('g')
    this.renderLayers(clusterRoot, addressRoot)
    this.renderLinks(linksRoot)
    this.renderShadows(clusterShadowsRoot, addressShadowsRoot)
    return this.root.node()
  }
  renderLayers (clusterRoot, addressRoot) {
    let cumX = 0
    this.layers.forEach((layer) => {
      let cRoot = clusterRoot.append('g')
      let aRoot = addressRoot.append('g')
      layer.render(cRoot, aRoot)
      let box = cRoot.node().getBBox()
      let x = cumX - box.width / 2
      let y = box.height / -2
      cRoot.attr('transform', `translate(${x}, ${y})`)
      aRoot.attr('transform', `translate(${x}, ${y})`)
      cumX = x + box.width + margin
      layer.translate(x, y)
    })
  }
  renderLinks (root) {
    const link = linkHorizontal()
      .x(([node, isSource]) => isSource ? node.getXForLinks() + node.getWidthForLinks() : node.getXForLinks())
      .y(([node, isSource]) => node.getYForLinks() + node.getHeightForLinks() / 2)
    for (let i = 0; i < this.layers.length; i++) {
      this.layers[i].nodes.each((clusterId1) => {
        let cluster1 = this.clusterNodes.get([clusterId1, this.layers[i].id])
        let c1 = this.store.get('cluster', clusterId1)
        this.linkToLayerCluster(root, link, this.layers[i + 1], c1.outgoing, cluster1, true)
        this.linkToLayerCluster(root, link, this.layers[i - 1], c1.incoming, cluster1, false)
        cluster1.nodes.each((addressId1) => {
          let address1 = this.addressNodes.get([addressId1, this.layers[i].id])
          let a1 = this.store.get('address', addressId1)
          this.linkToLayer(root, link, this.layers[i + 1], a1.outgoing, address1, true)
          this.linkToLayer(root, link, this.layers[i - 1], a1.incoming, address1, false)
        })
      })
    }
  }
  linkToLayer (root, link, layer, neighbors, source, isOutgoing) {
    if (layer) {
      layer.nodes.each((clusterId2) => {
        let cluster2 = this.clusterNodes.get([clusterId2, layer.id])
        cluster2.nodes.each((addressId2) => {
          if (!neighbors.has(addressId2)) return
          let address2 = this.addressNodes.get([addressId2, layer.id])
          let path = link({source: [source, isOutgoing], target: [address2, !isOutgoing]})
          root.append('path').classed('link', true).attr('d', path)
        })
      })
    }
  }
  linkToLayerCluster (root, link, layer, neighbors, source, isOutgoing) {
    if (layer) {
      layer.nodes.each((clusterId2) => {
        if (!neighbors.has(clusterId2)) return
        let cluster2 = this.clusterNodes.get([clusterId2, layer.id])
        let path = link({source: [source, isOutgoing], target: [cluster2, !isOutgoing]})
        root.append('path').classed('link', true).attr('d', path)
      })
    }
  }
  renderShadows (clusterRoot, addressRoot) {
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
}
