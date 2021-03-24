import { t, tt } from './lang.js'
import configLayout from './config/layout.html'
import graphConfig from './config/graph.html'
import exportConfig from './config/export.html'
import importConfig from './config/import.html'
import highlightConfig from './config/highlight.html'
import highlightRow from './config/highlightRow.html'
import legendItem from './config/legendItem.html'
import filter from './config/filter.html'
import { addClass, removeClass, replace } from './template_utils.js'
import { firstToUpper } from './utils.js'
import Component from './component.js'
import Logger from './logger.js'
import $ from 'jquery'
import '../lib/jquery-ui/widgets/sortable'
import { schemeSet1 } from 'd3-scale-chromatic'
import { scaleOrdinal } from 'd3-scale'

const logger = Logger.create('Config') // eslint-disable-line no-unused-vars

const colors = []

const scale = scaleOrdinal(schemeSet1)

for (let i = 0; i < 8; i++) {
  colors.push(scale(i))
}

export default class Config extends Component {
  constructor (dispatcher, labelType, txLabelType, locale) {
    super()
    this.dispatcher = dispatcher
    this.labelType = labelType
    this.txLabelType = txLabelType
    this.visible = false
    this.categoryColors = []
    this.locale = locale
    this.concepts = new Map()
    this.highlights = []
  }

  toggleConfig () {
    this.visible = this.visible === 'config' ? null : 'config'
    this.setUpdate(true)
  }

  toggleLegend () {
    this.visible = this.visible === 'legend' ? null : 'legend'
    this.setUpdate(true)
  }

  toggleExport () {
    this.visible = this.visible === 'export' ? null : 'export'
    this.setUpdate(true)
  }

  toggleImport () {
    this.visible = this.visible === 'import' ? null : 'import'
    this.setUpdate(true)
  }

  toggleHighlight () {
    this.visible = this.visible === 'highlight' ? null : 'highlight'
    this.currentHighlight = null
    this.setUpdate(true)
    return this.visible === 'highlight'
  }

  setLocale (locale) {
    this.locale = locale
    this.setUpdate(true)
  }

  hide () {
    this.visible = null
    this.currentHighlight = null
    this.setUpdate(true)
  }

  setConcepts (concepts) {
    concepts.forEach(concept => {
      this.concepts.set(concept.id, concept)
    })
  }

  setCategoryColors (colors, ordering) {
    this.categoryColors = ordering.map(cat => ({ key: cat, value: colors.get(cat) }))
    if (this.visible === 'legend') {
      this.setUpdate(true)
    }
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
    this.root.innerHTML = tt(configLayout)
    const el = this.root.querySelector('#dropdown')
    if (this.visible === 'config') {
      el.innerHTML = tt(graphConfig)
      this.renderSelect('addressLabel', 'changeAddressLabel', this.labelType.addressLabel)
      this.renderSelect('transactionLabel', 'changeTxLabel', this.txLabelType)
      this.renderSelect('locale', 'changeLocale', this.locale)
    } else if (this.visible === 'legend') {
      const canSort = this.categoryColors.length > 1
      this.categoryColors.forEach(({ key, value }) => {
        const itemEl = document.createElement('div')
        itemEl.className = 'flex items-center'
        itemEl.innerHTML = legendItem
        itemEl.id = key
        if (canSort) {
          itemEl.title = t('Reorder to prioritize')
          addClass(itemEl, 'cursor-grab')
        }
        itemEl.querySelector('.legendColor').style.backgroundColor = value
        const concept = this.concepts.get(key)
        const output = concept ? `<a ${concept.uri ? `href="${concept.uri}" target="_blank"` : ''} title="${concept.description}">${concept.label}</a>` : key
        itemEl.querySelector('.legendItem').innerHTML = output
        el.appendChild(itemEl)
      })
      if (canSort) {
        const j = $(el)
        j.sortable({
          cursor: 'grabbing',
          axis: 'y',
          update: (e, ui) => this.dispatcher('sortCategories', j.sortable('toArray'))
        })
      }
    } else if (this.visible === 'export') {
      el.innerHTML = tt(exportConfig)
      el.querySelectorAll('button[data-msg]').forEach(button => {
        const msg = button.getAttribute('data-msg')
        if (!msg) return
        button.addEventListener('click', () => { this.dispatcher(msg) })
      })
    } else if (this.visible === 'import') {
      el.innerHTML = tt(importConfig)
      el.querySelectorAll('button[data-msg]').forEach(button => {
        const msg = button.getAttribute('data-msg')
        if (!msg) return
        button.addEventListener('click', () => { this.dispatcher(msg) })
      })
    } else if (this.visible === 'highlight') {
      el.innerHTML = tt(highlightConfig)
      let colorsEl = el.querySelector('#colors')
      logger.debug('colors', colors)
      colors.forEach(color => {
        if (this.highlights.filter(([c, _]) => color === c).length === 1) return
        const colorEl = document.createElement('button')
        colorEl.className = 'highlightColor'
        colorEl.style.backgroundColor = color
        colorEl.addEventListener('click', () => { this.dispatcher('addHighlight', color) })
        colorsEl.appendChild(colorEl)
      })
      colorsEl = el.querySelector('#highlights')
      this.highlights.forEach(([color, title]) => {
        const colorEl = document.createElement('div')
        colorEl.className = 'flex flex-row mb-2'
        colorEl.innerHTML = tt(highlightRow)
        const button = colorEl.querySelector('button')
        button.style.backgroundColor = color
        button.addEventListener('click', () => { this.dispatcher('pickHighlight', color) })
        const input = colorEl.querySelector('input')
        input.value = title
        if (this.currentHighlight === color) {
          addClass(input, 'border')
          removeClass(input, 'border-none')
          input.removeAttribute('disabled')
          input.addEventListener('input', (e) => { this.dispatcher('inputHighlight', [color, e.target.value]) })
          addClass(button, 'current')
          button.setAttribute('title', t('Click again to change'))
          const trash = colorEl.querySelector('#trash')
          trash.addEventListener('click', () => { this.dispatcher('removeHighlight', color) })
          trash.style.visibility = 'visible'
          colorEl.appendChild(trash)
          if (this.editingHighlight) {
            const wrapper = colorEl.querySelector('.highlightWrapper')
            const freeColors = document.createElement('div')
            colors.forEach(col => {
              if (this.highlights.filter(([c, _]) => col === c).length === 1 || col === color) return
              const colEl = document.createElement('button')
              colEl.className = 'highlightColor'
              colEl.style.backgroundColor = col
              colEl.addEventListener('click', () => { this.dispatcher('editHighlight', [color, col]) })
              freeColors.appendChild(colEl)
            })
            freeColors.style.position = 'absolute'
            freeColors.style.width = '300px'
            freeColors.style.left = '100%'
            freeColors.style.top = '-4px'
            wrapper.appendChild(freeColors)
          }
        }
        colorsEl.appendChild(colorEl)
      })
    }
    super.render()
    return this.root
  }

  addHighlight (color) {
    this.highlights.push([color, ''])
    this.currentHighlight = color
    this.setUpdate(true)
  }

  inputHighlight (color, title) {
    this.highlights = this.highlights.map(([c, t]) => color === c ? [c, title] : [c, t])
  }

  pickHighlight (color) {
    this.setUpdate(true)
    if (color === this.currentHighlight) {
      this.editingHighlight = !this.editingHighlight
      return
    }
    this.editingHighlight = false
    this.currentHighlight = color
  }

  removeHighlight (color) {
    if (color === this.currentHighlight) this.currentHighlight = null
    this.highlights = this.highlights.filter(([c, _]) => color !== c)
    this.setUpdate(true)
  }

  editHighlight (color, newColor) {
    this.highlights = this.highlights.map(([c, t]) => color === c ? [newColor, t] : [c, t])
    this.editingHighlight = false
    this.setUpdate(true)
  }

  addFilter (id, type, value) {
    const filterSection = this.root.querySelector('#' + id)
    const f = document.createElement('div')
    f.className = 'table'
    f.innerHTML = replace(filter, { filter: firstToUpper(type) })
    const el = f.querySelector('div div')
    switch (type) {
      case 'limit':
        this.addLimitFilter(el, id, value)
        break
    }
    filterSection.appendChild(f)
  }

  addLimitFilter (root, id, value) {
    const el = document.createElement('input')
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
    this.dispatcher('loadEgonet', { id: this.node.id, isOutgoing, type: this.view, limit: filters.get('limit') })
  }

  applyAddressFilters () {
    this.dispatcher('loadEntityAddresses', { id: this.node.id, limit: this.node.addressFilters.get('limit') })
  }

  renderSelect (id, message, selectedValue) {
    const select = this.root.querySelector('select#' + id)
    let i = 0
    for (; i < select.options.length; i++) {
      if (select.options[i].value === selectedValue) break
    }
    select.options.selectedIndex = i
    select.addEventListener('change', (e) => {
      this.dispatcher(message, e.target.value)
    })
  }

  renderInput (id, message, value) {
    const input = this.root.querySelector('input#' + id)
    input.value = value
    input.addEventListener('change', (e) => {
      this.dispatcher(message, e.target.value)
    })
  }

  setAddressLabel (labelType) {
    this.labelType.address = labelType
  }

  setEntityLabel (labelType) {
    this.labelType.entity = labelType
  }

  setTxLabel (labelType) {
    this.txLabelType = labelType
  }

  serialize () {
    return [
      this.labelType,
      this.txLabelType,
      this.highlights
    ]
  }

  deserialize (version, values) {
    this.labelType = values[0]
    this.txLabelType = values[1]
    this.highlights = values[2] || [] // backwards compat
  }
}
