import search from './search.html'
import {set} from 'd3-collection'
import BrowserComponent from './component.js'

const limit = 100

export default class Search extends BrowserComponent{
  constructor (dispatcher, index) {
    super(dispatcher, index)
    this.term = ''
    this.loading = set()
  }
  render () {
    if(this.term) {
      this.renderResult()
      return
    }
    this.root.innerHTML = search
    this.input = this.root.querySelector('input')
    this.input.value = this.term
    this.root.querySelector('form')
      .addEventListener('submit', (e) => {
        this.term = this.input.value
        this.dispatcher.call('search', null, this.term)
        e.returnValue = null
        return false
      })
    this.root.querySelector('input')
      .addEventListener('input', (e) => {
        let len = this.result?.addresses?.length + 0
        this.term = e.target.value
        if(len !== 0 && len < limit && this.term.length >= this.resultTerm?.length + 0) {
          this.renderResult()
          return
        }
        this.dispatcher.call('search', null, [this.term, limit])
      })
    this.renderResult()
    return this.root
  }
  renderOptions () {
    return null
  }
  renderResult () {
    let ul = document.createElement('ol')
    ul.className = 'list-reset'
    console.log('addresses', this.result)
    this.result?.addresses?.forEach((addr => {
      if(!addr.startsWith(this.term)) return
      let li = document.createElement('li')
      li.className = 'cursor-pointer'
      li.appendChild(document.createTextNode(addr))
      li.addEventListener('click', () => {
        this.loading.add(addr)
        this.dispatcher.call('loadNode', null, {id : addr, type : 'address'})
      })
      ul.appendChild(li)
    }))
    let el = this.root.querySelector('#browser-search-result')
    el.innerHTML = ''
    el.appendChild(ul)
  }
  setResult ([result, term]) {
    this.result = {...result}
    this.resultTerm = term
  }
}
