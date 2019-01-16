import search from './search.html'
import Component from '../component.js'
import {addClass, removeClass} from '../template_utils.js'

const empty = {addresses: [], transactions: []}
const numShowResults = 10

const byPrefix = term => addr => addr.startsWith(term)

export default class Search extends Component {
  constructor (dispatcher, keyspaces) {
    super()
    this.keyspaces = keyspaces
    this.dispatcher = dispatcher
    this.term = ''
    this.resultTerm = ''
    this.clearResults()
  }
  clearResults () {
    this.result = {}
    for (let keyspace in this.keyspaces) {
      this.result[keyspace] = empty
    }
  }
  clear () {
    this.clearResults()
    this.term = ''
    this.isLoading = false
    this.shouldUpdate(true)
  }
  error (keyspace, msg) {
    this.result[keyspace] = msg
    this.shouldUpdate('result')
  }
  showLoading () {
    console.log('showLoading')
    if (!this.isLoading) {
      this.isLoading = true
      this.shouldUpdate('result')
    }
  }
  hideLoading () {
    console.log('hideLoading')
    if (this.isLoading) {
      this.isLoading = false
      this.shouldUpdate('result')
    }
  }
  renderLoading () {
    if (this.isLoading) {
      // removeClass(this.root.querySelector('#browser-search-result'), 'hidden')
      this.root.querySelector('#indicator').style.display = 'inline'
    } else {
      // addClass(this.root.querySelector('#browser-search-result'), 'hidden')
      this.root.querySelector('#indicator').style.display = 'none'
    }
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
        e.returnValue = false
        for (let keyspace in this.result) {
          if (this.result[keyspace].addresses.length > 0) {
            let addresses = this.result[keyspace].addresses.filter(byPrefix(this.term))
            this.dispatcher('clickSearchResult', {id: addresses[0], type: 'address', keyspace})
            return false
          }
        }
        return false
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
      this.clearResults()
    }
  }
  needsResults (keyspace, limit, prefixLength) {
    if (this.term.length < prefixLength) return false
    let len = (this.result[keyspace].addresses || []).length
    return !(len !== 0 && len < limit && this.term.startsWith(this.resultTerm))
  }
  renderOptions () {
    return null
  }

  renderResult () {
    console.log('addresses', this.result)
    let frame = this.root.querySelector('#browser-search-result')
    let el = frame.querySelector('#result')
    el.innerHTML = ''

    let visible = this.isLoading
    let allErrors = true
    for (let keyspace in this.keyspaces) {
      visible = visible ||
        (typeof this.result[keyspace] === 'string') ||
        this.result[keyspace].addresses.length > 0 ||
        this.result[keyspace].transactions.length > 0
      if (typeof this.result[keyspace] === 'string') {
        continue
      }
      allErrors = false

      let ul = document.createElement('ol')
      ul.className = 'list-reset'
      let count = 0
      this.result[keyspace].addresses
        .filter(byPrefix(this.term))
        .forEach(addr => {
          if (count > numShowResults) return
          count++
          let li = document.createElement('li')
          li.className = 'cursor-pointer'
          li.appendChild(document.createTextNode(addr))
          li.addEventListener('click', () => {
            this.dispatcher('clickSearchResult', {id: addr, type: 'address', keyspace})
          })
          ul.appendChild(li)
        })
      // if no results to render don't draw the title and the list at all
      if (count === 0) continue
      let title = document.createElement('div')
      title.className = 'font-bold py-1'
      title.appendChild(document.createTextNode(this.keyspaces[keyspace]))
      el.appendChild(title)
      el.appendChild(ul)
    }
    if (allErrors) {
      el.innerHTML = `Failed to fetch from any keyspaces`
      addClass(el, 'text-gs-red')
    }
    console.log('isVisible', this.isLoading, visible)
    if (visible) {
      addClass(frame, 'block')
      removeClass(frame, 'hidden')
    } else {
      removeClass(frame, 'block')
      addClass(frame, 'hidden')
    }
    this.renderLoading()
  }
  setResult (term, result) {
    if (term !== this.term) return
    this.result[result.keyspace] = {...result}
    this.resultTerm = term
    this.shouldUpdate('result')
  }
}
