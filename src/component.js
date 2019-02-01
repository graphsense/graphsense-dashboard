export default class Component {
  constructor () {
    this.update = true
  }
  shouldUpdate (update) {
    if (update === undefined) {
      return this.update
    }
    if (this.update === true) return
    this.update = update
  }
  render () {
    this.update = false
  }
}
