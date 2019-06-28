export default class Component {
  constructor () {
    this.update = true
  }
  setUpdate (update) {
    // boolean overrides strings
    if (typeof update === 'boolean') {
      this.update = update
      return
    }
    if (this.update === true) return
    if (this.update === false) {
      this.update = new Set()
    }
    this.update.add(update)
  }
  shouldUpdate (update) {
    if (typeof this.update === 'boolean') return this.update
    if (update === undefined) return true
    return this.update.has(update)
  }
  render () {
    this.update = false
  }
}
