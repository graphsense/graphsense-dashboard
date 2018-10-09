import {create} from 'd3-selection'
import layout from './browser/layout.html'
import {dispatch} from './dispatch.js'
import Address from './browser/address.js'
import Search from './browser/search.js'

export default class Browser {
  constructor (dispatcher) {
    this.dispatcher = dispatcher
    this.dispatcher.on('searchresult.browser', (result) => {
      this.searchresult(result)
    })
    this.root = document.createElement('div')
    this.search()
  }
  search () {
    this.activeTab = 'search'
    this.content = [ new Search((term) => {
      this.dispatcher.call('search', null, term)
    }) ]
  }
  searchresult (result) {
    if (this.activeTab !== 'search') return
    // assume search being the first in content
    let content = [this.content[0]]
    let comp = null
    if (result.address) {
      comp = new Address(result)
    }
    if (comp === null) return
    content.push(comp)
    this.content = content
    this.render()
  }
  address (address) {
    this.content = [ new Address(this.dispatcher) ]
  }
  render () {
    this.root.innerHTML = layout
    let data = this.root.querySelector('#browser-data')

    this.content.forEach((comp) => {
      data.appendChild(comp.render())
    })
    return this.root
  }
}
