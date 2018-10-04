export default class Browser {
  constructor (dispatcher, root) {
    this.dispatcher = dispatcher
    this.root = root
  }
  render () {
    this.root.append('div')
      .append('input')
      .attr('type', 'text')
    return this
  }
  remove () {
    this.root.select('div').remove()
  }
}
