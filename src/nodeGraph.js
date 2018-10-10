import {create} from 'd3-selection'
import {set} from 'd3-collection'
import Layer from './nodeGraph/layer.js'
import ClusterNode from './nodeGraph/clusterNode.js'

const margin = 0.1

export default class NodeGraph {
  constructor (dispatcher, store) {
    this.dispatcher = dispatcher
    this.store = store
    this.adding = set()
    this.layers = []
    this.viewBox = {
      x: -300,
      y: -300,
      w: 600,
      h: 600
    }
    this.margin = margin * this.viewBox.w
    this.dispatcher.on('addAddress.graph', (address) => {
      let a = this.store.get('address', address)
      if (!a) {
        this.dispatcher.call('loadAddress', null, address)
        this.adding.add(address)
        return
      }
      if (!a.cluster) {
        this.dispatcher.call('loadClusterForAddress', null, address)
        this.adding.add(address)
        return
      }
      let c = this.store.get('cluster', a.cluster)
      if (!c) {
        throw new Error(`inconsistency in store: cluster referenced by address ${address} as ${a.cluster} not found`)
      }
      this.add(c)
    })
    this.dispatcher.on('resultAddress.graph', (address) => {
      if (!this.adding.has(address.address)) return
      this.store.add(address)
      this.dispatcher.call('loadClusterForAddress', null, address)
    })
    this.dispatcher.on('resultClusterForAddress.graph', (cluster) => {
      if (!this.adding.has(cluster.forAddress)) return
      this.adding.remove(cluster.forAddress)
      cluster = this.store.add(cluster)
      this.add(cluster)
    })
    this.dispatcher.on('selectAddress.graph', ([address, layerId]) => {
      let filtered = this.layers.filter(({id}) => id === layerId)
      console.log('filtered', filtered, this.layers)
      if (filtered.length === 0) return
      let sel = filtered[0].findAddressNode(address)
      if (sel.select()) {
        this.selectedNode.deselect()
        this.selectedNode = sel
      }
    })
  }
  add (cluster) {
    // only allow adding of nodes if graph is empty
    if (this.layers.length > 0) return
    let layer = new Layer(this)
    let node = new ClusterNode(cluster, layer)
    layer.add(node)
    this.layers.push(layer)
    this.clear()
    this.renderLayers()
  }
  clear () {
    this.root.innerHTML = ''
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
      cumX = x + box.width + this.margin
    })
  }
}
