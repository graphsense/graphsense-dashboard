export default class Component {
  constructor () {
    this.update = true
  }
  setUpdate (update, value) {
    if (this.nodes) {
      console.log('setUpdate Entity', this.id, update, value)
    }
    // boolean overrides strings
    if (typeof update === 'boolean') {
      this.update = update
      return
    }
    if (this.update === true) return
    if (this.update === false) {
      this.update = new Map()
    }
    this.update.set(update, value)
  }
  shouldUpdate (update) {
    if (typeof this.update === 'boolean') return this.update
    if (update === undefined) return this.update.size > 0
    return this.update.has(update)
  }
  getUpdate (update) {
    return this.update.get(update)
  }
  render () {
    this.update = false
  }
}
