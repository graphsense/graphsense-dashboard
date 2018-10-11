import {create} from 'd3-selection'
import {set, map} from 'd3-collection'
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
        throw new Error(`inconsistency in store: cluster referenced by address ${address} as ${a.cluster} not found`)
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
      result.nodes.forEach((node) => {
        if (node.id === addressId[0]) return
        let request = {
          anchorNode: addressId,
          isOutgoing,
          address: node.id
        }
        this.dispatcher.call('addAddress', null, request)
      })
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
      node.add(addressNode.id)
    } else if (object.cluster) {
      if (this.clusterNodes.has([object.cluster, layerId])) return
      node = new ClusterNode(object.cluster, layerId, this)
    }
    this.clusterNodes.set(node.id, node)

    layer.add(node.id)
    this.clear()
    this.renderLayers()
  }
  clear () {
    this.root.node().innerHTML = ''
  }
  render () {
    this.root = create('svg')
      .classed('w-full h-full', true)
      .attr('viewBox', (({x, y, w, h}) => `${x} ${y} ${w} ${h}`)(this.viewBox))
      .attr('preserveAspectRatio', 'xMidYMid meet')
    this.renderLayers()
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
    })
  }
}
