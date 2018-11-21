import search from './search.html'
import BrowserComponent from './component.js'

export default class Search extends BrowserComponent {
  constructor (dispatcher, index) {
    super(dispatcher, index)
    this.term = ''
    this.resultTerm = ''
    this.result = {addresses: [], transactions: []}
  }
  render (root) {
    console.log('search', this.shouldUpdate())
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
    let ul = document.createElement('ol')
    ul.className = 'list-reset'
    console.log('addresses', this.result)
    if (!this.result || !this.result.addresses) return
    this.result.addresses.forEach(addr => {
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
    el.innerHTML = ''
    el.appendChild(ul)
  }
  setResult (term, result) {
    this.result = {...result}
    this.resultTerm = term
    this.shouldUpdate('result')
  }
}
