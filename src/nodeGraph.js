import {create, event} from 'd3-selection'
import {set, map} from 'd3-collection'
import {linkHorizontal} from 'd3-shape'
import {drag} from 'd3-drag'
import Layer from './nodeGraph/layer.js'
import ClusterNode from './nodeGraph/clusterNode.js'
import AddressNode from './nodeGraph/addressNode.js'

const margin = 200

export default class NodeGraph {
  constructor (dispatcher, store) {
    this.dispatcher = dispatcher
    this.store = store
    this.clusterNodes = map()
    this.addressNodes = map()
    this.adding = set()
    this.layers = []
    this.viewBox = {
      x: -300,
      y: -300,
      w: 600,
      h: 600
    }
    this.dispatcher.on('addAddress.graph', (request) => {
      let a = this.store.get('address', request.address)
      if (!a) {
        this.dispatcher.call('loadAddress', null, request)
        this.adding.add(request.address)
        return
      }
      if (!a.cluster) {
        this.dispatcher.call('loadClusterForAddress', null, request)
        this.adding.add(request.address)
        return
      }
      let c = this.store.get('cluster', a.cluster)
      if (!c) {
        throw new Error(`inconsistency in store`)
      }
      this.add(a, request.anchorNode, request.isOutgoing)
    })
    this.dispatcher.on('resultAddress.graph', (response) => {
      if (!this.adding.has(response.result.address)) return
      this.store.add(response.result)
      this.dispatcher.call('loadClusterForAddress', null, response.request)
    })
    this.dispatcher.on('resultClusterForAddress.graph', (response) => {
      if (!this.adding.has(response.request.address)) return
      this.adding.remove(response.request.address)
      // merge address into cluster object for store
      this.store.add({...response.result, ...{forAddress: response.request.address}})
      let address = this.store.get('address', response.request.address)
      this.add(address, response.request.anchorNode, response.request.isOutgoing)
    })
    this.dispatcher.on('selectAddress.graph', ([address, layerId]) => {
      console.log('graph', address, layerId)
      let sel = this.findAddressNode(address, layerId)
      if (sel) {
        sel.select()
        if (this.selectedNode && this.selectedNode !== sel) {
          this.selectedNode.deselect()
        }
        this.selectedNode = sel
      }
    })
    this.dispatcher.on('resultEgonet.graph', ({addressId, isOutgoing, result}) => {
      let a = this.store.get('address', addressId[0])
      result.nodes.forEach((node) => {
        if (node.id === addressId[0]) return
        let request = {
          anchorNode: addressId,
          isOutgoing,
          address: node.id
        }
        if (isOutgoing) {
          a.outgoing.add(node.id)
        } else {
          a.incoming.add(node.id)
        }
        this.dispatcher.call('addAddress', null, request)
      })
      this.store.add(a)
    })
  }
  findAddressNode (address, layerId) {
    return this.addressNodes.get([address, layerId])
  }
  add (object, anchorNode, isOutgoing) {
    let layerId
    if (!anchorNode) {
      layerId = 0
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
      let addressNode = new AddressNode(object, layerId, this)
      console.log('new AddressNode', addressNode)
      this.addressNodes.set(addressNode.id, addressNode)
      node = this.clusterNodes.get([object.cluster, layerId])
      if (!node) {
        node = new ClusterNode(object.cluster, layerId, this)
      }
      node.add(addressNode.id[0])
    } else if (object.cluster) {
      if (this.clusterNodes.has([object.cluster, layerId])) return
      node = new ClusterNode(object.cluster, layerId, this)
    }
    this.clusterNodes.set(node.id, node)

    layer.add(node.id[0])
    this.clear()
    this.render()
  }
  clear () {
    this.root.node().innerHTML = ''
  }
  render () {
    this.root = this.root || create('svg')
      .classed('w-full h-full', true)
      .attr('viewBox', (({x, y, w, h}) => `${x} ${y} ${w} ${h}`)(this.viewBox))
      .attr('preserveAspectRatio', 'xMidYMid meet')
      .call(drag().on('drag', () => {
        this.viewBox.x -= event.dx
        this.viewBox.y -= event.dy
        this.root.attr('viewBox', (({x, y, w, h}) => `${x} ${y} ${w} ${h}`)(this.viewBox))
      }))
    this.renderLayers()
    this.renderLinks()
    return this.root.node()
  }
  renderLayers () {
    let cumX = 0
    this.layers.forEach((layer) => {
      let g = this.root.append('g')
      layer.render(g)
      let box = g.node().getBBox()
      let x = cumX - box.width / 2
      let y = box.height / -2
      g.attr('transform', `translate(${x}, ${y})`)
      cumX = x + box.width + margin
      layer.translate(x, y)
    })
  }
  renderLinks () {
    const link = linkHorizontal()
      .x(([node, isSource]) => isSource ? node.x + node.width : node.x)
      .y(([node, isSource]) => node.y + node.height / 2)
    for (let i = 0; i < this.layers.length; i++) {
      this.layers[i].nodes.each((clusterId1) => {
        let cluster1 = this.clusterNodes.get([clusterId1, this.layers[i].id])
        cluster1.nodes.each((addressId1) => {
          console.log('adressId', addressId1)
          let address1 = this.addressNodes.get([addressId1, this.layers[i].id])
          let a1 = this.store.get('address', addressId1)
          this.linkToLayer(link, this.layers[i + 1], a1.outgoing, address1, true)
          this.linkToLayer(link, this.layers[i - 1], a1.incoming, address1, false)
        })
      })
    }
  }
  linkToLayer (link, layer, neighbors, source, isOutgoing) {
    if (layer) {
      layer.nodes.each((clusterId2) => {
        let cluster2 = this.clusterNodes.get([clusterId2, layer.id])
        cluster2.nodes.each((addressId2) => {
          if (!neighbors.has(addressId2)) return
          let address2 = this.addressNodes.get([addressId2, layer.id])
          let path = link({source: [source, isOutgoing], target: [address2, !isOutgoing]})
          this.root.append('path').classed('link', true).attr('d', path)
        })
      })
    }
  }
}
