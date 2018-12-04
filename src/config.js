import {event} from 'd3-selection'
import layout from './config/layout.html'
import graphConfig from './config/graph.html'
import notes from './config/notes.html'
import filter from './config/filter.html'
import {replace} from './template_utils.js'
import {firstToUpper} from './utils.js'
import Component from './component.js'

const menuWidth = 250
const menuHeight = 300

export default class Config extends Component {
  constructor (dispatcher, labelType, currency, txLabelType) {
    super()
    this.currency = currency
    this.dispatcher = dispatcher
    this.labelType = labelType
    this.txLabelType = txLabelType
    this.view = null
  }
  switchConfig (type) {
    this.view = type
    this.shouldUpdate(true)
  }
  setCurrency (currency) {
    this.currency = currency
  }
  showGraphConfig (x, y) {
    this.setMenuPosition(x, y)
    this.view = 'graph'
    this.shouldUpdate(true)
  }
  showNodeConfig (x, y, node) {
    this.setMenuPosition(x, y)
    this.view = node.data.type
    this.node = node
    this.shouldUpdate(true)
  }
  setMenuPosition (x, y) {
    let w = window
    let d = document
    let e = d.documentElement
    let g = d.getElementsByTagName('body')[0]
    let width = w.innerWidth || e.clientWidth || g.clientWidth
    let height = w.innerHeight || e.clientHeight || g.clientHeight
    if (x + menuWidth > width) x -= menuWidth
    if (y + menuHeight > height) y -= menuWidth
    this.menuX = x
    this.menuY = y
  }
  hideMenu () {
    this.view = null
    this.shouldUpdate(true)
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (!this.shouldUpdate()) return this.root
    if (!this.view) {
      this.root.innerHTML = ''
      super.render()
      return
    }
    this.root.innerHTML = layout
    let frame = this.root.querySelector('#config-frame')
    frame.addEventListener('click', (e) => {
      this.dispatcher('hideContextmenu')
    })
    frame.addEventListener('contextmenu', (e) => {
      e.stopPropagation()
      e.preventDefault()
      return false
    })
    let box = this.root.querySelector('#config-box')
    box.style.left = this.menuX + 'px'
    box.style.top = this.menuY + 'px'
    box.addEventListener('click', (e) => {
      e.stopPropagation()
    })
    let el = this.root.querySelector('#config')
    let title = ''
    switch (this.view) {
      case 'graph':
        title = 'Graph configuration'
        el.innerHTML = graphConfig
        this.addSelectListener('currency', 'changeCurrency')
        this.addSelectListener('clusterLabel', 'changeClusterLabel')
        this.addSelectListener('addressLabel', 'changeAddressLabel')
        this.addSelectListener('transactionLabel', 'changeTxLabel')
        break
      case 'address':
      case 'cluster':
        title = 'Notes'
        el.innerHTML = notes
        this.setupNotes(el)
        break
    }
    this.root.querySelector('.title').innerHTML = title
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
    let input = el.querySelector('textarea')
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
