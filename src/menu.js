import menuLayout from './config/menu.html'
import notes from './config/notes.html'
import Component from './component.js'
import Logger from './logger.js'
import searchDialog from './config/searchDialog.html'
import categoryForm from './config/categoryForm.html'
import addressesForm from './config/addressesForm.html'
import {categories} from './globals.js'
import {replace, addClass} from './template_utils.js'
import Search from './search/search.js'

const logger = Logger.create('Menu') // eslint-disable-line

const defaultCriterion = 'category'
const defaultParams = () => ({category: null, addresses: []})
const defaultDepth = 2
const defaultBreadth = 20

export default class Menu extends Component {
  constructor (dispatcher, keyspaces) {
    super()
    this.dispatcher = dispatcher
    this.keyspaces = keyspaces
    this.view = {}
  }
  showNodeDialog (x, y, params) {
    let menuWidth = 250
    let menuHeight = 300
    if (params.dialog === 'note') {
      this.view = {viewType: 'node', node: params.node}
    } else if (params.dialog === 'search') {
      this.view = {
        viewType: 'search',
        id: params.id,
        type: params.type,
        isOutgoing: params.isOutgoing,
        criterion: defaultCriterion,
        params: defaultParams(),
        depth: defaultDepth,
        breadth: defaultBreadth
      }
      menuWidth = 400
      menuHeight = 400
    }
    this.setMenuPosition(x, y, menuWidth, menuHeight)
    this.setUpdate(true)
  }
  setMenuPosition (x, y, menuWidth, menuHeight) {
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
    this.view = {}
    this.setUpdate(true)
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (!this.shouldUpdate()) {
      if (this.search) this.search.render()
      return this.root
    }
    if (!this.view.viewType) {
      this.root.innerHTML = ''
      super.render()
      return
    }
    this.root.innerHTML = menuLayout
    let menu = this.root.querySelector('#menu-frame')
    menu.addEventListener('click', (e) => {
      this.dispatcher('hideContextmenu')
    })
    menu.addEventListener('contextmenu', (e) => {
      e.stopPropagation()
      e.preventDefault()
      return false
    })
    let box = this.root.querySelector('#menu-box')
    box.style.left = this.menuX + 'px'
    box.style.top = this.menuY + 'px'
    box.addEventListener('click', (e) => {
      e.stopPropagation()
    })
    let el = this.root.querySelector('#config')
    let title
    if (this.view.viewType === 'node') {
      title = 'Notes'
      el.innerHTML = notes
      this.setupNotes(el)
    } else if (this.view.viewType === 'search') {
      let dir = this.view.isOutgoing ? 'outgoing' : 'incoming'
      title = `Search ${dir} neighbors`
      el.innerHTML = replace(searchDialog,
        {
          searchDepth: this.view.depth,
          searchBreadth: this.view.breadth
        }
      )
      this.setupSearch(el)
    }
    this.root.querySelector('.title').innerHTML = title
    super.render()
    return this.root
  }
  renderInput (id, message, value) {
    let input = this.root.querySelector('input#' + id)
    input.value = value
    input.addEventListener('input', (e) => {
      this.dispatcher(message, e.target.value)
    })
  }
  setupNotes (el) {
    let node = this.view.node
    let input = el.querySelector('textarea')
    input.value = node.data.notes || ''
    input.addEventListener('input', (e) => {
      this.dispatcher('inputNotes', {id: node.data.id, type: node.data.type, keyspace: node.data.keyspace, note: e.target.value})
    })
  }
  setupSearch (el) {
    el.querySelector('.criterion').addEventListener('change', e => {
      this.dispatcher('changeSearchCriterion', e.target.value)
    })
    this.renderInput('searchDepth', 'changeSearchDepth', this.view.depth)
    this.renderInput('searchBreadth', 'changeSearchBreadth', this.view.breadth)
    let form = el.querySelector('.searchValue')
    if (this.view.criterion === 'category') {
      form.innerHTML = categoryForm
      let input = form.querySelector('select')
      categories.forEach(category => {
        let option = document.createElement('option')
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
      el.querySelector('input[value="addresses"]').removeAttribute('checked')
    } else if (this.view.criterion === 'addresses') {
      form.innerHTML = addressesForm
      let searchinput = form.querySelector('.searchinput')
      this.search = new Search(this.dispatcher, [this.view.id[2]], ['addresses'], true)
      this.search.render(searchinput)
      let searchAddresses = form.querySelector('.searchaddresses')
      this.view.params.addresses.forEach(address => {
        let li = document.createElement('li')
        li.innerHTML = address
        searchAddresses.appendChild(li)
      })
      el.querySelector('input[value="addresses"]').setAttribute('checked', 'checked')
      el.querySelector('input[value="category"]').removeAttribute('checked')
    }
    let button = el.querySelector('input[type="button"]')
    if (this.view.params.category || this.view.params.addresses.length > 0) {
      button.addEventListener('click', () => {
        this.dispatcher('searchNeighbors', {
          id: this.view.id,
          type: this.view.type,
          isOutgoing: this.view.isOutgoing,
          depth: this.view.depth,
          breadth: this.view.breadth,
          params: this.view.params
        })
      })
    } else {
      addClass(button, 'disabled')
    }
  }
  setSearchCriterion (criterion) {
    if (this.view.viewType !== 'search') return
    this.view.criterion = criterion
    this.view.params = defaultParams()
    this.setUpdate(true)
  }
  setSearchCategory (category) {
    if (this.view.viewType !== 'search' || this.view.criterion !== 'category') return
    this.view.params.category = category
    this.setUpdate(true)
  }
  setSearchDepth (d) {
    if (this.view.viewType !== 'search') return
    this.view.depth = d
  }
  setSearchBreadth (d) {
    if (this.view.viewType !== 'search') return
    this.view.breadth = d
  }
  addSearchAddress (address) {
    if (this.view.viewType !== 'search' || this.view.criterion !== 'addresses') return
    this.view.params.addresses.push(address)
    this.setUpdate(true)
  }
}
