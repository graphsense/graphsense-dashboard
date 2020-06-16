import { t, tt } from './lang.js'
import menuLayout from './config/menu.html'
import notes from './config/notes.html'
import tagpack from './config/tagpack.html'
import Component from './component.js'
import Logger from './logger.js'
import searchDialog from './config/searchDialog.html'
import categoryForm from './config/categoryForm.html'
import addressesForm from './config/addressesForm.html'
import minmaxForm from './config/minmaxForm.html'
import { maxSearchBreadth, maxSearchDepth } from './globals.js'
import { replace, addClass, removeClass } from './template_utils.js'
import Search from './search/search.js'

const logger = Logger.create('Menu') // eslint-disable-line

const defaultCriterion = 'category'
const defaultParams = () => ({ category: null, addresses: [], field: null, min: null, max: null })
const defaultDepth = 2
const defaultBreadth = 20

export default class Menu extends Component {
  constructor (dispatcher) {
    super()
    this.dispatcher = dispatcher
    this.view = {}
    this.categories = []
    this.abuses = []
  }

  showNodeDialog (x, y, params) {
    let menuWidth = 250
    let menuHeight = 300
    if (params.dialog === 'note') {
      this.view = { viewType: 'note', data: params.data }
    } else if (params.dialog === 'tagpack') {
      const labels = params.data.tags
        .filter(tag => tag.isUserDefined)
        .reduce((labels, tag) => {
          labels[tag.label] = { ...tag }
          return labels
        }, {})
      this.view = { viewType: 'tagpack', data: params.data, labels }
      this.search = new Search(this.dispatcher, ['labels', 'userdefinedlabels'], this.view.viewType)
    } else if (params.dialog === 'neighborsearch') {
      this.view = {
        viewType: 'neighborsearch',
        id: params.id,
        type: params.type,
        isOutgoing: params.isOutgoing,
        criterion: defaultCriterion,
        params: defaultParams(),
        depth: defaultDepth,
        breadth: defaultBreadth,
        skipNumAddresses: defaultBreadth
      }
      menuWidth = 400
      menuHeight = 400
    }
    this.setMenuPosition(x, y, menuWidth, menuHeight)
    this.setUpdate(true)
  }

  setMenuPosition (x, y, menuWidth, menuHeight) {
    const w = window
    const d = document
    const e = d.documentElement
    const g = d.getElementsByTagName('body')[0]
    const width = w.innerWidth || e.clientWidth || g.clientWidth
    const height = w.innerHeight || e.clientHeight || g.clientHeight
    if (x + menuWidth > width) x -= menuWidth
    if (y + menuHeight > height) y -= menuWidth
    this.menuX = x
    this.menuY = y
  }

  getType () {
    return this.view.viewType
  }

  setConcepts (concepts) {
    const categories = concepts
      .filter(({ taxonomy }) => taxonomy === 'entity')
      .map(({ label }) => label)
    const abuses = concepts
      .filter(({ taxonomy }) => taxonomy === 'abuse')
      .map(({ label }) => label)
    this.addCategories(categories)
    this.addAbuses(abuses)
    this.setUpdate(true)
  }

  addCategories (cats) {
    cats.forEach(cat => {
      if (this.categories.indexOf(cat) === -1) this.categories.push(cat)
    })
  }

  addAbuses (abs) {
    abs.forEach(ab => {
      if (this.abuses.indexOf(ab) === -1) this.abuses.push(ab)
    })
  }

  hideMenu () {
    this.view = {}
    this.search = null
    this.setUpdate(true)
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (!this.shouldUpdate()) {
      if (this.search) this.search.render()
      return this.root
    }
    if (this.shouldUpdate(true)) {
      if (!this.view.viewType) {
        this.root.innerHTML = ''
        super.render()
        return
      }
      this.root.innerHTML = tt(menuLayout)
      const menu = this.root.querySelector('#menu-frame')
      menu.addEventListener('click', (e) => {
        this.dispatcher('hideContextmenu')
      })
      menu.addEventListener('contextmenu', (e) => {
        e.stopPropagation()
        e.preventDefault()
        return false
      })
      const box = this.root.querySelector('#menu-box')
      box.style.left = this.menuX + 'px'
      box.style.top = this.menuY + 'px'
      box.addEventListener('click', (e) => {
        e.stopPropagation()
      })
      const el = this.root.querySelector('#config')
      let title
      if (this.view.viewType === 'note') {
        title = t('Notes')
        el.innerHTML = tt(notes)
        this.setupNotes(el)
      } else if (this.view.viewType === 'tagpack') {
        title = t('Add tag')
        el.innerHTML = tt(tagpack)
        this.setupTagpack(el)
      } else if (this.view.viewType === 'neighborsearch') {
        const dir = this.view.isOutgoing ? 'outgoing' : 'incoming'
        title = t(`Search ${dir} neighbors`)
        el.innerHTML = replace(tt(searchDialog),
          {
            searchDepth: this.view.depth,
            searchBreadth: this.view.breadth,
            maxSearchBreadth: maxSearchBreadth,
            maxSearchDepth: maxSearchDepth,
            skipNumAddresses: this.view.skipNumAddresses
          }
        )
        this.setupSearch(el)
      }
      this.root.querySelector('.title').innerHTML = title
    } else if (this.shouldUpdate('skipNumAddresses')) {
      const el = this.root.querySelector('#skipNumAddresses')
      el.value = this.view.skipNumAddresses
      el.setAttribute('min', this.view.breadth)
    } else if (this.shouldUpdate('minmax')) {
      if (this.view.params.max !== Infinity) {
        const min = this.root.querySelector('#min')
        min.setAttribute('max', this.view.params.max)
      }
      if (this.view.params.min !== 0) {
        const max = this.root.querySelector('#max')
        max.setAttribute('min', this.view.params.min)
      }
      this.renderButton(this.root)
    } else if (this.shouldUpdate('button')) {
      logger.debug('render button')
      this.renderButton(this.root)
    }
    super.render()
    return this.root
  }

  renderInput (id, message, value) {
    const input = this.root.querySelector('input#' + id)
    input.value = value
    input.addEventListener('input', (e) => {
      this.dispatcher(message, e.target.value)
    })
  }

  setupNotes (el) {
    const data = this.view.data
    const input = el.querySelector('textarea')
    input.focus()
    input.value = data.notes || ''
    input.addEventListener('input', (e) => {
      this.dispatcher('inputNotes', { id: data.id, type: data.type, keyspace: data.keyspace, note: e.target.value })
    })
  }

  setupTagpack (el) {
    const searchinput = el.querySelector('#input')
    this.search.setUpdate(true)
    this.search.render(searchinput)
    const searchLabels = el.querySelector('#labels')

    this.renderButton(el)

    const label = Object.keys(this.view.labels)[0]
    if (!label) return

    removeClass(searchLabels, 'hidden')
    addClass(searchinput, 'hidden')
    if (!this.labelTagsLoading) {
      el.querySelector('#loading').style.height = '0px'
    }
    const del = el.querySelector('#remove')
    del.addEventListener('click', () => {
      this.dispatcher('removeLabel', label)
    })
    el.querySelector('#label').innerHTML = label
    let sel = el.querySelector('#category > select')
    this.categories.forEach(category => {
      const option = document.createElement('option')
      option.innerHTML = category
      option.setAttribute('value', category)
      if (category === this.view.labels[label].category) {
        option.setAttribute('selected', 'selected')
      }
      if (this.view.labels[label].available && this.view.labels[label].available.categories.has(category)) {
        addClass(option, 'font-bold')
      }
      sel.appendChild(option)
    })
    sel.addEventListener('change', (e) => {
      this.dispatcher('changeUserDefinedTag', { data: { category: e.target.value }, label })
    })
    sel = el.querySelector('#abuse > select')
    this.abuses.forEach(category => {
      const option = document.createElement('option')
      option.innerHTML = category
      option.setAttribute('value', category)
      if (category === this.view.labels[label].abuse) {
        option.setAttribute('selected', 'selected')
      }
      if (this.view.labels[label].available && this.view.labels[label].available.abuses.has(category)) {
        addClass(option, 'font-bold')
      }
      sel.appendChild(option)
    })
    sel.addEventListener('change', (e) => {
      this.dispatcher('changeUserDefinedTag', { data: { abuse: e.target.value }, label })
    })
    sel = el.querySelector('#source > input')
    sel.value = this.view.labels[label].source || ''
    sel.addEventListener('input', (e) => {
      this.dispatcher('changeUserDefinedTag', { data: { source: e.target.value }, label })
    })
    // remove value for the time of selecting a source from datalist
    sel.addEventListener('click', function () {
      this.value = ''
    })
    const that = this
    // value after selecting a source from datalist or just blurring
    sel.addEventListener('blur', function () {
      this.value = that.view.labels[label].source || ''
    })
    if (this.view.labels[label].available && this.view.labels[label].available.sources.size > 1) {
      sel.setAttribute('list', 'sources_datalist')
      sel = el.querySelector('#source')
      const datalist = document.createElement('datalist')
      datalist.setAttribute('id', 'sources_datalist')
      this.view.labels[label].available.sources.forEach(source => {
        const option = document.createElement('option')
        option.innerHTML = source
        datalist.appendChild(option)
      })
      sel.appendChild(datalist)
    }
  }

  setupSearch (el) {
    el.querySelector('.criterion').addEventListener('change', e => {
      this.dispatcher('changeSearchCriterion', e.target.value)
    })
    this.renderInput('searchDepth', 'changeSearchDepth', this.view.depth)
    this.renderInput('searchBreadth', 'changeSearchBreadth', this.view.breadth)
    this.renderInput('skipNumAddresses', 'changeSkipNumAddresses', this.view.skipNumAddresses)
    const form = el.querySelector('.searchValue')
    if (this.view.criterion === 'category') {
      form.innerHTML = tt(categoryForm)
      const input = form.querySelector('select')
      this.categories.forEach(category => {
        const option = document.createElement('option')
        option.innerHTML = category
        option.setAttribute('value', category)
        if (category === this.view.params.category) {
          option.setAttribute('selected', 'selected')
        }
        input.appendChild(option)
      })
      input.addEventListener('change', (e) => {
        this.dispatcher('changeSearchCategory', e.target.value)
      })
      el.querySelector('input[value="category"]').setAttribute('checked', 'checked')
    } else if (this.view.criterion === 'addresses') {
      form.innerHTML = tt(addressesForm)
      const searchinput = form.querySelector('.searchinput')
      this.search.setUpdate(true)
      this.search.setKeyspaces([this.view.id[2]])
      this.search.render(searchinput)
      const searchAddresses = form.querySelector('.searchaddresses')
      this.view.params.addresses.forEach(address => {
        const li = document.createElement('li')
        li.innerHTML = address
        searchAddresses.appendChild(li)
      })
      el.querySelector('input[value="addresses"]').setAttribute('checked', 'checked')
    } else if (this.view.criterion === 'final_balance' || this.view.criterion === 'total_received') {
      form.innerHTML = tt(minmaxForm)
      el.querySelector('input[value="' + this.view.criterion + '"]').setAttribute('checked', 'checked')
      this.renderInput('min', 'changeMin', this.view.params.min)
      this.renderInput('max', 'changeMax', this.view.params.max)
    }
    this.renderButton(el)
  }

  renderButton (el) {
    let button = el.querySelector('input[type="button"]')
    // to remove existing event listeners
    const clone = button.cloneNode(false)
    button.parentNode.replaceChild(clone, button)
    button = clone
    if (this.view.viewType === 'neighborsearch') {
      if (this.validParams()) {
        button.addEventListener('click', () => {
          this.dispatcher('searchNeighbors', {
            id: this.view.id,
            type: this.view.type,
            isOutgoing: this.view.isOutgoing,
            depth: this.view.depth,
            breadth: this.view.breadth,
            skipNumAddresses: this.view.skipNumAddresses,
            params: this.view.params
          })
        })
        removeClass(button, 'disabled')
      } else {
        addClass(button, 'disabled')
      }
    } else if (this.view.viewType === 'tagpack') {
      if (this.view.dirty) {
        button.addEventListener('click', () => {
          this.dispatcher('setLabels', {
            id: this.view.data.id,
            type: this.view.data.type,
            keyspace: this.view.data.keyspace,
            labels: this.view.labels
          })
        })
        removeClass(button, 'disabled')
      } else {
        addClass(button, 'disabled')
      }
    }
  }

  validParams () {
    return this.view.params.category ||
      this.view.params.addresses.length > 0 ||
      (this.view.params.min !== undefined &&
      this.view.params.max !== undefined &&
      this.view.params.min >= 0 &&
      this.view.params.max >= 0 &&
      this.view.params.min <= this.view.params.max
      )
  }

  setSearchCriterion (criterion) {
    if (this.view.viewType !== 'neighborsearch') return
    this.view.criterion = criterion
    this.view.params = defaultParams()
    if (this.view.criterion === 'final_balance' || this.view.criterion === 'total_received') {
      this.view.params.field = this.view.criterion
    }
    if (criterion === 'addresses') {
      this.search = new Search(this.dispatcher, ['addresses'], this.view.viewType)
    }
    this.setUpdate(true)
  }

  setSearchCategory (category) {
    if (this.view.viewType === 'neighborsearch' && this.view.criterion === 'category') {
      this.view.params.category = category
      this.setUpdate(true)
    } else if (this.view.viewType === 'tagpack') {
      this.view.category = category
      this.setUpdate(true)
    }
  }

  setSearchDepth (d) {
    if (this.view.viewType !== 'neighborsearch') return
    this.view.depth = Math.min(d, maxSearchDepth)
    if (d > maxSearchDepth) {
      this.setUpdate(true)
    }
  }

  setSearchBreadth (d) {
    if (this.view.viewType !== 'neighborsearch') return
    this.view.breadth = Math.min(d, maxSearchBreadth)
    this.view.skipNumAddresses = Math.max(this.view.breadth, this.view.skipNumAddresses)
    if (d > maxSearchBreadth) {
      this.setUpdate(true)
    }
    this.setUpdate('skipNumAddresses')
  }

  setSkipNumAddresses (d) {
    if (this.view.viewType !== 'neighborsearch') return
    this.view.skipNumAddresses = Math.max(d, this.view.breadth || maxSearchBreadth)
    if (d < this.view.skipNumAddresses) {
      this.setUpdate(true)
    }
  }

  addSearchAddress (address) {
    if (this.view.viewType !== 'neighborsearch' || this.view.criterion !== 'addresses') return
    this.view.params.addresses = [...new Set(this.view.params.addresses.concat([address]))]
    this.setUpdate(true)
  }

  addSearchLabel (label, loadingTags) {
    if (this.view.viewType !== 'tagpack') return
    if (this.view.labels[label]) return
    this.labelTagsLoading = loadingTags
    this.view.labels[label] = { label, category: null, abuse: null, source: null }
    this.setDirty(true)
    this.setUpdate(true)
  }

  removeSearchLabel (label) {
    if (this.view.viewType !== 'tagpack') return
    delete this.view.labels[label]
    this.setDirty(true)
    this.setUpdate(true)
  }

  setTagpack (label, data) {
    if (this.view.viewType !== 'tagpack') return
    if (!this.view.labels[label]) return
    for (const i in data) {
      this.view.labels[label][i] = data[i]
    }
    this.setDirty(true)
  }

  labelTagsData (result) {
    if (this.view.viewType !== 'tagpack') return
    this.labelTagsLoading = false
    result.forEach(({ label, category, abuse, source }) => {
      const l = this.view.labels[label]
      if (!l) return
      if (!l.available) {
        l.available = {
          sources: new Set(),
          categories: new Set(),
          abuses: new Set()
        }
      }
      if (source) l.available.sources.add(source)
      if (category) l.available.categories.add(category)
      if (abuse) l.available.abuses.add(abuse)
    })
    for (const label in this.view.labels) {
      const l = this.view.labels[label]
      l.source = l.available.sources.values().next().value || null
      l.category = l.available.categories.values().next().value || null
      l.abuse = l.available.abuses.values().next().value || null
    }
    this.setUpdate(true)
  }

  setMin (value) {
    if (this.view.viewType !== 'neighborsearch') return
    if (this.view.criterion !== 'final_balance' && this.view.criterion !== 'total_received') return
    logger.debug('min', value, this.view.params.min, this.view.params.max)
    value *= 1
    this.view.params.min = value
    this.setUpdate('minmax')
  }

  setMax (value) {
    if (this.view.viewType !== 'neighborsearch') return
    if (this.view.criterion !== 'final_balance' && this.view.criterion !== 'total_received') return
    value *= 1
    this.view.params.max = value
    this.setUpdate('minmax')
  }

  setDirty (d) {
    if (this.view.viewType !== 'tagpack') return
    this.view.dirty = d
    this.setUpdate('button')
  }
}
