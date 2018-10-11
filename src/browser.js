import layout from './browser/layout.html'
import Address from './browser/address.js'
import Search from './browser/search.js'

export default class Browser {
  constructor (dispatcher, store) {
    this.store = store
    this.dispatcher = dispatcher
    this.dispatcher.on('searchresult.browser', (result) => {
      this.searchresult(result)
      this.address(result.address)
      this.render()
    })
    this.dispatcher.on('selectAddress.browser', ([address]) => {
      this.address(address)
      this.render()
    })
    this.root = document.createElement('div')
    this.root.className = 'h-full'
    this.search()
  }
  search () {
    this.activeTab = 'search'
    this.content = [ new Search((term) => {
      this.dispatcher.call('search', null, term)
    }) ]
  }
  address (addr) {
    let address = this.store.get('address', addr)
    if (!address) {
      console.error(`browser: ${addr} not found`)
      return
    }
    this.activeTab = 'address'
    this.content = [ new Address(this.dispatcher, address) ]
  }
  searchresult (result) {
    if (this.activeTab !== 'search') return
    // assume search being the first in content
    let content = [this.content[0]]
    let comp = null
    if (result.address) {
      this.store.add(result)
      comp = new Address(this.dispatcher, result)
    }
    if (comp === null) return
    content.push(comp)
    this.content = content
  }
  render () {
    this.root.innerHTML = layout
    this.root.querySelector('#browser-nav-search-button')
      .addEventListener('click', () => {
        this.search()
        this.render()
      })
    let data = this.root.querySelector('#browser-data')
    let c = 0
    this.content.forEach((comp) => {
      c += 1
      data.appendChild(comp.render())
      let options = comp.renderOptions()
      if (!options) return
      let el = document.createElement('div')
      el.className = 'h-full mx-2 my-1 ' + (c < this.content.length ? 'browser-options-short' : '')
      el.appendChild(options)
      data.appendChild(el)
    })
    return this.root
  }
}
