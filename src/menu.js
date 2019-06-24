import menuLayout from './config/menu.html'
import notes from './config/notes.html'
import Component from './component.js'
import Logger from './logger.js'
import searchDialog from './config/searchDialog.html'
import categoryForm from './config/categoryForm.html'
import {categories} from './globals.js'
import {replace} from './template_utils.js'

const logger = Logger.create('Menu') // eslint-disable-line

const menuWidth = 250
const menuHeight = 300

const defaultCriterion = 'category'
const defaultParams = {category: categories[0]}
const defaultDepth = 2
const defaultBreadth = 20

export default class Menu extends Component {
  constructor (dispatcher) {
    super()
    this.dispatcher = dispatcher
  }
  showNodeDialog (x, y, params) {
    this.setMenuPosition(x, y)
    if (params.dialog === 'note') {
      this.view = {viewType: 'node', node: params.node}
    } else if (params.dialog === 'search') {
      this.view = {
        viewType: 'search',
        id: params.id,
        type: params.type,
        isOutgoing: params.isOutgoing,
        criterion: defaultCriterion,
        params: defaultParams,
        depth: defaultDepth,
        breadth: defaultBreadth
      }
    }
    this.setUpdate(true)
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
    this.setUpdate(true)
  }
  render (root) {
    logger.debug('render menu')
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (!this.shouldUpdate()) return this.root
    if (!this.view) {
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
  setupNotes (el) {
    let node = this.view.node
    let input = el.querySelector('textarea')
    input.value = node.data.notes || ''
    input.addEventListener('input', (e) => {
      this.dispatcher('inputNotes', {id: node.data.id, type: node.data.type, keyspace: node.data.keyspace, note: e.target.value})
    })
  }
  setupSearch (el) {
    let form = el.querySelector('.searchValue')
    let searchParams = {
      id: this.view.id,
      type: this.view.type,
      isOutgoing: this.view.isOutgoing,
      depth: this.view.depth,
      breadth: this.view.breadth,
      params: this.view.params
    }
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
    }
    el.querySelector('input[type="button"]').addEventListener('click', () => {
      this.dispatcher('searchNeighbors', searchParams)
    })
  }
  setSearchCategory (category) {
    if (this.view.viewType !== 'search' || this.view.criterion !== 'category') return
    this.view.params.category = category
    this.setUpdate(true)
  }
}
