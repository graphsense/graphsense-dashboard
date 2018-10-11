import layout from './config/layout.html'
import graphConfig from './config/graph.html'
import addressConfig from './config/address.html'
import filter from './config/filter.html'
import {replace} from './template_utils.js'

const availableFilters =
  [ 'value', 'date'
  ]

export default class Config {
  constructor (dispatcher, graph) {
    this.dispatcher = dispatcher
    this.root = document.createElement('div')
    this.root.className = 'h-full'
    this.view = 'graph'
    this.graph = graph
    this.dispatcher.on('selectAddress.config', ([address, layerId]) => {
      console.log('config', this.graph, address, layerId)
      this.view = 'address'
      this.addressNode = this.graph.findAddressNode(address, layerId)
      console.log('addressNode', this.addressNode)
      this.render()
    })
  }
  render () {
    this.root.innerHTML = layout
    let el = this.root.querySelector('#config')
    switch (this.view) {
      case 'graph':
        el.innerHTML = graphConfig
        break
      case 'address':
        el.innerHTML = addressConfig
        el.querySelector('#outgoing-input select')
          .addEventListener('change', (e) => {
            this.addressNode.outgoingTxsFilters.set(e.target.value, null)
            this.render()
          })
        el.querySelector('#incoming-input select')
          .addEventListener('change', (e) => {
            this.addressNode.incomingTxsFilters.set(e.target.value, null)
            this.render()
          })
        this.addressNode.outgoingTxsFilters.each((value, type) => {
          this.addFilter(true, type, value)
        })
        this.addressNode.incomingTxsFilters.each((value, type) => {
          this.addFilter(false, type, value)
        })
        el.querySelector('#outgoing-input button')
          .addEventListener('click', () => {
            this.applyFilters(true)
          })
        el.querySelector('#incoming-input button')
          .addEventListener('click', () => {
            this.applyFilters(false)
          })
        break
    }
    return this.root
  }
  addFilter (isOutgoing, type, value) {
    let sel = isOutgoing ? 'outgoing-filters' : 'incoming-filters'
    let filterSection = this.root.querySelector('#' + sel)
    let f = document.createElement('div')
    f.className = 'table'
    f.innerHTML = replace(filter, {filter: type})
    let el = f.querySelector('div div')
    switch (type) {
      case 'limit':
        this.addLimitFilter(el, isOutgoing, value)
        break
    }
    filterSection.appendChild(f)
  }
  addLimitFilter (root, isOutgoing, value) {
    let el = document.createElement('input')
    el.className = 'border w-8'
    el.setAttribute('type', 'number')
    el.setAttribute('min', '1')
    el.value = value
    el.addEventListener('input', (e) => {
      if (isOutgoing) {
        this.addressNode.outgoingTxsFilters.set('limit', e.target.value)
      } else {
        this.addressNode.incomingTxsFilters.set('limit', e.target.value)
      }
    })
    root.appendChild(el)
  }
  applyFilters (isOutgoing) {
    let filters
    if (isOutgoing) {
      filters = this.addressNode.outgoingTxsFilters
    } else {
      filters = this.addressNode.incomingTxsFilters
    }
    this.dispatcher.call('applyAddressFilters', null, [this.addressNode.id, isOutgoing, filters])
  }
}
