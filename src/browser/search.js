import search from './search.html'

export default class Search {
  constructor (onSearch) {
    this.root = document.createElement('div')
    this.root.className = 'h-full'
    this.term = ''
    this.onSearch = onSearch
  }
  render () {
    this.root.innerHTML = search
    this.input = this.root.querySelector('input')
    this.input.value = this.term
    this.root.querySelector('button')
      .addEventListener('click', () => {
        this.onSearch(this.input.value)
        this.term = this.input.value
      })
    return this.root
  }
  renderOptions () {
    return null
  }
}
