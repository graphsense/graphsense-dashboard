import configLayout from './config/layout.html'
import graphConfig from './config/graph.html'
import legendItem from './config/legendItem.html'
import filter from './config/filter.html'
import {addClass, removeClass, replace} from './template_utils.js'
import {firstToUpper} from './utils.js'
import Component from './component.js'
import Logger from './logger.js'

const logger = Logger.create('Config') // eslint-disable-line no-unused-vars

export default class Config extends Component {
  constructor (dispatcher, labelType, txLabelType, searchDepth, searchBreadth) {
    super()
    this.dispatcher = dispatcher
    this.labelType = labelType
    this.txLabelType = txLabelType
    this.visible = false
    this.searchDepth = searchDepth
    this.searchBreadth = searchBreadth
    this.categoryColors = {}
  }
  toggleConfig () {
    this.visible = this.visible === 'config' ? null : 'config'
    this.setUpdate(true)
  }
  toggleLegend () {
    this.visible = this.visible === 'legend' ? null : 'legend'
    this.setUpdate(true)
  }
  hide () {
    this.visible = null
    this.setUpdate(true)
  }
  setCategoryColors (colors) {
    this.categoryColors = colors
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (!this.shouldUpdate()) return this.root
    if (!this.visible) {
      removeClass(this.root, 'show')
      super.render()
      return this.root
    }
    addClass(this.root, 'show')
    this.root.innerHTML = configLayout
    let el = this.root.querySelector('#dropdown')
    if (this.visible === 'config') {
      el.innerHTML = graphConfig
      this.renderSelect('clusterLabel', 'changeClusterLabel')
      this.renderSelect('addressLabel', 'changeAddressLabel')
      this.renderSelect('transactionLabel', 'changeTxLabel')
      this.renderInput('searchDepth', 'changeSearchDepth', this.searchDepth)
      this.renderInput('searchBreadth', 'changeSearchBreadth', this.searchBreadth)
    } else if (this.visible === 'legend') {
      for (let name in this.categoryColors) {
        let itemEl = document.createElement('div')
        itemEl.className = 'flex items-center'
        itemEl.innerHTML = legendItem
        itemEl.querySelector('.legendColor').style.backgroundColor = this.categoryColors[name]
        itemEl.querySelector('.legendItem').innerHTML = name
        el.appendChild(itemEl)
      }
    }
    super.render()
    return this.root
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
  renderSelect (id, message) {
    let select = this.root.querySelector('select#' + id)
    let i = 0
    for (; i < select.options.length; i++) {
      let value = this.labelType[id]
      if (id === 'transactionLabel') value = this.txLabelType
      if (select.options[i].value === value) break
    }
    select.options.selectedIndex = i
    select.addEventListener('change', (e) => {
      this.dispatcher(message, e.target.value)
    })
  }
  renderInput (id, message, value) {
    let input = this.root.querySelector('input#' + id)
    input.value = value
    input.addEventListener('change', (e) => {
      this.dispatcher(message, e.target.value)
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
  setSearchDepth (d) {
    this.searchDepth = d
  }
  setSearchBreadth (d) {
    this.searchBreadth = d
  }
  serialize () {
    return [
      this.labelType,
      this.txLabelType,
      this.searchDepth,
      this.searchBreadth
    ]
  }
  deserialize (version, values) {
    this.labelType = values[0]
    this.txLabelType = values[1]
    this.searchDepth = values[2]
    this.searchBreadth = values[3]
  }
}
