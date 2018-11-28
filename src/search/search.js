import search from './search.html'
import Component from '../component.js'
import {addClass, removeClass} from '../template_utils.js'

const empty = {addresses: [], transactions: []}

export default class Search extends Component {
  constructor (dispatcher) {
    super()
    this.dispatcher = dispatcher
    this.term = ''
    this.resultTerm = ''
    this.result = empty
  }
  clear () {
    this.result = empty
    this.term = ''
    this.shouldUpdate(true)
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (!this.shouldUpdate()) return
    if (this.shouldUpdate() === 'result') {
      super.render()
      this.renderResult()
      return this.root
    }
    super.render()
    console.log('full rerendering search')
    this.root.innerHTML = search
    this.input = this.root.querySelector('input')
    this.input.value = this.term
    this.root.querySelector('form')
      .addEventListener('submit', (e) => {
      })
    this.root.querySelector('input')
      .addEventListener('input', (e) => {
        this.dispatcher('search', e.target.value)
      })
    this.root.querySelector('input')
      .addEventListener('blur', () => {
        // wrap in timeout to let possible clicksearchresult event happen
        setTimeout(() => this.dispatcher('blurSearch'), 200)
      })
    this.renderResult()
    return this.root
  }
  setSearchTerm (term, prefixLength) {
    this.term = term
    this.shouldUpdate('result')
    if (this.term.length < prefixLength) {
      this.result.addresses = []
      this.result.transactions = []
    }
  }
  needsResults (limit, prefixLength) {
    let len = this.result.addresses.length
    return !(len !== 0 && len < limit && this.term.startsWith(this.resultTerm))
  }
  renderOptions () {
    return null
  }
  renderResult () {
    console.log('addresses', this.result)
    if (!this.result || !this.result.addresses) return
    let ul = document.createElement('ol')
    ul.className = 'list-reset'
    this.result.addresses.slice(0, 10).forEach(addr => {
      if (!addr.startsWith(this.term)) return
      let li = document.createElement('li')
      li.className = 'cursor-pointer'
      li.appendChild(document.createTextNode(addr))
      li.addEventListener('click', () => {
        this.dispatcher('clickSearchResult', {id: addr, type: 'address'})
      })
      ul.appendChild(li)
    })
    let el = this.root.querySelector('#browser-search-result')
    if (this.result.addresses.length > 0) {
      addClass(el, 'block')
      removeClass(el, 'hidden')
    } else {
      removeClass(el, 'block')
      addClass(el, 'hidden')
    }
    el.innerHTML = ''
    el.appendChild(ul)
  }
  setResult (term, result) {
    if (term !== this.term) return
    this.result = {...result}
    this.resultTerm = term
    this.shouldUpdate('result')
  }
}
