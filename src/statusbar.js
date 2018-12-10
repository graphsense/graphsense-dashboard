import status from './status/status.html'
import Component from './component.js'

export default class Statusbar extends Component {
  constructor (dispatcher) {
    super()
    this.dispatcher = dispatcher
    this.messages = []
  }
  add (msg) {
    this.messages.unshift(msg)
    this.shouldUpdate(true)
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (!this.shouldUpdate()) return this.root
    this.root.innerHTML = status
    this.root.querySelector('#status-message').innerHTML = this.messages[0] || '&nbsp'
  }
  msg (type) {
    let args = Array.prototype.slice.call(arguments, 1)
    console.log('msg', type, args)
    switch (type) {
      case 'loading' :
        return `Loading ${args[0]} ${args[1] || ''} ...`
      case 'loaded' :
        return `Loaded ${args[0]} ${args[1] || ''}`
      case 'loadingNeighbors':
        let dir = args[2] ? 'outgoing' : 'incoming'
        return `Loading ${dir} neighbors for ${args[1]} ${args[0]} ...`
      case 'loadedNeighbors':
        let dir_ = args[2] ? 'outgoing' : 'incoming'
        return `Loaded ${dir_} neighbors for ${args[1]} ${args[0]}`
      case 'saving':
        return `Saving to file ...`
      case 'saved':
        return `Saved to file ${args[0]}`
      case 'loadFile':
        let filename = args[0]
        console.log('loadfile msg', filename)
        return `Loading file ${filename} ...`
      case 'loadedFile':
        let filename_ = args[0]
        console.log('loadedfile msg', filename_)
        return `Loaded file ${filename_}`
      case 'loadingClusterFor':
        return `Loading cluster for ${args[0]}`
      case 'loadedClusterFor':
        return `Loaded cluster for ${args[0]}`
      case 'noClusterFor':
        return `No cluster for ${args[0]}`
      case 'loadingTagsFor':
        return `Loading tags for ${args[0]} ${args[1]}`
      case 'loadedTagsFor':
        return `Loaded tags for ${args[0]} ${args[1]}`
      case 'loadingClusterAddresses':
        return `Trying to load ${args[1]} addresses for cluster ${args[0]}`
      case 'loadedClusterAddresses':
        return `Loaded ${args[1]} addresses for cluster ${args[0]}`
      case 'removeNode':
        return `Removed node of ${args[0]} ${args[1]}`
      default:
        console.warn('unhandled status message type', type)
    }
  }
  addMsg () {
    this.add(this.msg(...arguments))
  }
}
