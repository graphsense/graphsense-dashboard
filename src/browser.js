export default class Browser {
  constructor (dispatcher, root) {
    this.dispatcher = dispatcher
    this.root = root
    this.dispatcher.on('search.browser', (term) => {
      this.searching = true
      this.input.property('value', term)
    })
    this.dispatcher.on('searchresult.browser', (result) => {
      if (!this.searching) return
      this.result.text(result.noIncomingTxs)
      this.searching = false
    })
  }
  render () {
    let search = this.root.append('div')
    this.input = search.append('input')
      .attr('type', 'text')
    search.append('button')
      .text('Go')
      .on('click', () => {
        this.dispatcher.call('search', this, this.input.property('value'))
      })
    this.result = search.append('div')
    return this
  }
  remove () {
    this.root.select('div').remove()
  }
}
