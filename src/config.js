import layout from './config/layout.html'
import graphConfig from './config/graph.html'
import addressConfig from './config/address.html'
import clusterConfig from './config/cluster.html'
import filter from './config/filter.html'
import {replace} from './template_utils.js'
import {firstToUpper} from './utils.js'
import Component from './component.js'

export default class Config extends Component {
  constructor (dispatcher, labelType, currency, txLabelType) {
    super()
    this.currency = currency
    this.dispatcher = dispatcher
    this.labelType = labelType
    this.txLabelType = txLabelType
    this.view = 'graph'
  }
  selectNode (node) {
    console.log('selectNode.config', node)
    this.view = node.data.type
    this.node = node
    this.shouldUpdate(true)
  }
  switchConfig (type) {
    this.view = type
    this.shouldUpdate(true)
  }
  setCurrency (currency) {
    this.currency = currency
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (!this.shouldUpdate()) return this.root
    this.root.innerHTML = layout
    this.root.querySelector('button#navbar-config')
      .addEventListener('click', () => {
        this.dispatcher('switchConfig', 'graph')
      })
    let el = this.root.querySelector('#config')
    switch (this.view) {
      case 'graph':
        el.innerHTML = graphConfig
        this.addSelectListener('currency', 'changeCurrency')
        this.addSelectListener('clusterLabel', 'changeClusterLabel')
        this.addSelectListener('addressLabel', 'changeAddressLabel')
        this.addSelectListener('transactionLabel', 'changeTxLabel')
        break
      case 'address':
        el.innerHTML = addressConfig
        this.setupTxFilters(el)
        this.setupNotes(el)
        break
      case 'cluster':
        el.innerHTML = clusterConfig
        this.setupTxFilters(el)
        this.setupNotes(el)
        el.querySelector('#address-input select')
          .addEventListener('change', (e) => {
            this.node.addressFilters.set(e.target.value, null)
            this.render()
          })
        this.node.addressFilters.each((value, type) => {
          this.addFilter('address-filters', type, value)
        })
        el.querySelector('#address-input button')
          .addEventListener('click', () => {
            this.applyAddressFilters()
          })
        break
    }
    super.render()
    return this.root
  }
  setupTxFilters (el) {
    el.querySelector('#outgoing-input select')
      .addEventListener('change', (e) => {
        this.node.outgoingTxsFilters.set(e.target.value, null)
        this.render()
      })
    el.querySelector('#incoming-input select')
      .addEventListener('change', (e) => {
        this.node.incomingTxsFilters.set(e.target.value, null)
        this.render()
      })
    this.node.outgoingTxsFilters.each((value, type) => {
      this.addFilter('outgoing-filters', type, value)
    })
    this.node.incomingTxsFilters.each((value, type) => {
      this.addFilter('incoming-filters', type, value)
    })
    el.querySelector('#outgoing-input button')
      .addEventListener('click', () => {
        this.applyTxFilters(true)
      })
    el.querySelector('#incoming-input button')
      .addEventListener('click', () => {
        this.applyTxFilters(false)
      })
  }
  addFilter (id, type, value) {
    let filterSection = this.root.querySelector('#' + id)
    let f = document.createElement('div')
    f.className = 'table'
    f.innerHTML = replace(filter, {filter: firstToUpper(type)})
    let el = f.querySelector('div div')
    switch (type) {
      case 'limit':
        this.addLimitFilter(el, id, value)
        break
    }
    filterSection.appendChild(f)
  }
  addLimitFilter (root, id, value) {
    let el = document.createElement('input')
    el.className = 'border w-8'
    el.setAttribute('type', 'number')
    el.setAttribute('min', '1')
    el.value = value
    el.addEventListener('input', (e) => {
      switch (id) {
        case 'outgoing-filters':
          this.node.outgoingTxsFilters.set('limit', e.target.value)
          break
        case 'incoming-filters':
          this.node.incomingTxsFilters.set('limit', e.target.value)
          break
        case 'address-filters':
          this.node.addressFilters.set('limit', e.target.value)
          break
      }
    })
    root.appendChild(el)
  }
  applyTxFilters (isOutgoing) {
    let filters
    if (isOutgoing) {
      filters = this.node.outgoingTxsFilters
    } else {
      filters = this.node.incomingTxsFilters
    }
    this.dispatcher('loadEgonet', {id: this.node.id, isOutgoing, type: this.view, limit: filters.get('limit')})
  }
  applyAddressFilters () {
    this.dispatcher('loadClusterAddresses', {id: this.node.id, limit: this.node.addressFilters.get('limit')})
  }
  addSelectListener (id, message) {
    let select = this.root.querySelector('select#' + id)
    let i = 0
    for (; i < select.options.length; i++) {
      let value = this.labelType[id]
      if (id === 'currency') value = this.currency
      if (id === 'transactionLabel') value = this.txLabelType
      if (select.options[i].value === value) break
    }
    console.log('selectedIndex', i)
    select.options.selectedIndex = i
    select.addEventListener('change', (e) => {
      this.dispatcher(message, e.target.value)
    })
  }
  setupNotes (el) {
    let input = el.querySelector('.notes textarea')
    input.value = this.node.data.notes || ''
    input.addEventListener('input', (e) => {
      console.log('input', e.target.value)
      this.dispatcher('inputNotes', {id: this.node.data.id, type: this.node.data.type, note: e.target.value})
    })
  }
  setAddressLabel (labelType) {
    this.labelType['address'] = labelType
  }
  setClusterLabel (labelType) {
    this.labelType['cluster'] = labelType
  }
  setTxLabel (labelType) {
    this.txLabelType = labelType
  }
}
