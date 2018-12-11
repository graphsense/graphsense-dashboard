import {set} from 'd3-collection'
import status from './status/status.html'
import Component from './component.js'
import {addClass, removeClass} from './template_utils.js'

const logsDisplayLength = 100

export default class Statusbar extends Component {
  constructor (dispatcher) {
    super()
    this.dispatcher = dispatcher
    this.messages = []
    this.loading = set()
    this.visible = false
    this.logsDisplayLength = logsDisplayLength
    this.update = set()
    this.numErrors = 0
    this.showErrorsLogs = false
  }
  shouldUpdate (update) {
    if (update === undefined) {
      return this.update
    }
    if (update === true) {
      this.update.add('all')
      return
    }
    this.update.add(update)
  }
  toggleErrorLogs () {
    this.showErrorsLogs = !this.showErrorsLogs
    if (this.showErrorsLogs) this.show()
    this.shouldUpdate('logs')
  }
  show () {
    if (!this.visible) {
      this.visible = true
      this.shouldUpdate('visibility')
    }
  }
  hide () {
    if (this.visible) {
      this.visible = false
      this.shouldUpdate('visibility')
    }
  }
  add (msg) {
    this.messages.push(msg)
    this.shouldUpdate('add')
  }
  moreLogs () {
    this.logsDisplayLength += logsDisplayLength
    this.shouldUpdate('logs')
  }
  addLoading (id) {
    this.loading.add(id)
    this.shouldUpdate('loading')
  }
  removeLoading (id) {
    this.loading.remove(id)
    this.shouldUpdate('loading')
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    let s = this.shouldUpdate()
    if (!s.size() === 0) return this.root
    if (s.has('loading')) {
      this.renderLoading()
      s.remove('loading')
    }
    if (s.has('add')) {
      let i = this.messages.length - 1
      this.renderLogs(this.messages[i], i)
      console.log('render add')
      s.remove('add')
    }
    if (s.has('logs')) {
      this.renderLogs()
      s.remove('logs')
    }
    if (s.has('visibility')) {
      this.renderVisibility()
      s.remove('visibility')
    }
    if (!s.has('all')) return
    this.root.innerHTML = status

    this.root.querySelector('#hide').addEventListener('click', () => {
      this.dispatcher('hideLogs')
    })
    this.root.querySelector('#show').addEventListener('click', () => {
      this.dispatcher('showLogs')
    })
    this.root.querySelector('#errors').addEventListener('click', () => {
      this.dispatcher('toggleErrorLogs')
    })
    this.renderLoading()
    this.renderLogs()
    this.renderVisibility()
    s.remove('all')
  }
  renderLoading () {
    let top = this.root.querySelector('#topmsg')
    if (this.loading.size() === 0) {
      removeClass(this.root, 'loading')
      if (top) top.innerHTML = ''
      return
    }
    addClass(this.root, 'loading')
    let msg = 'Loading '
    let v = this.loading.values()
    msg += v.slice(0, 3).join(', ')
    msg += v.length > 3 ? ` + ${v.length - 3}` : ''
    msg += ' ...'
    top.innerHTML = msg
  }
  renderLogs (msg, index) {
    let logs = this.root.querySelector('ul#log-messages')
    let messages = this.messages
    let errorMsg = this.root.querySelector('#errorMsg')
    if (this.showErrorsLogs) {
      errorMsg.innerHTML = 'Errors only'
      messages = messages.filter(msg => typeof msg !== 'string')
    } else {
      errorMsg.innerHTML = ''
    }
    if (messages.length > this.logsDisplayLength) {
      // remove 'show more' button
      logs.removeChild(logs.lastChild)
    }
    if (msg) {
      this.renderLogMsg(logs, msg, index)
    } else {
      logs.innerHTML = ''
      messages.slice(0, this.logsDisplayLength).forEach((msg, i) => {
        this.renderLogMsg(logs, msg, i)
      })
    }
    if (messages.length > this.logsDisplayLength) {
      let more = document.createElement('li')
      more.className = 'cursor-pointer text-gs-dark'
      more.innerHTML = 'Show more ...'
      more.addEventListener('click', () => {
        this.dispatcher('moreLogs')
      })
      logs.appendChild(more)
    }
    if (this.numErrors > 0) {
      console.log('numErrors', this.numErrors)
      removeClass(this.root.querySelector('#errors i'), 'hidden')
    }
  }
  renderLogMsg (root, msg, index) {
    let el = document.createElement('li')
    el.innerHTML = this.msgToString(msg)
    if (root.lastChild && root.childNodes.length > this.logsDisplayLength) {
      root.removeChild(root.lastChild)
    }
    if (root.firstChild) {
      root.insertBefore(el, root.firstChild)
    } else {
      root.appendChild(el)
    }
  }
  msgToString (msg) {
    if (typeof msg === 'string') {
      return msg
    }
    if (msg.error) {
      return `<span class="text-gs-red">Error requesting ${msg.error.requestURL}: ${msg.error.message}`
    }
  }
  renderVisibility () {
    console.log('render visiblity', this.messages)
    if (!this.visible) {
      removeClass(this.root, 'visible')
    } else {
      addClass(this.root, 'visible')
    }
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
      case 'error':
        this.numErrors++
        return {error: args[0]}
      default:
        console.warn('unhandled status message type', type)
    }
  }
  addMsg () {
    this.add(this.msg(...arguments))
  }
}
