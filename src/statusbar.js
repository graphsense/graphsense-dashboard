import {set, map} from 'd3-collection'
import status from './status/status.html'
import Component from './component.js'
import {addClass, removeClass} from './template_utils.js'
import Logger from './logger.js'

const logger = Logger.create('Statusbar') // eslint-disable-line no-unused-vars

const logsDisplayLength = 100

export default class Statusbar extends Component {
  constructor (dispatcher) {
    super()
    this.dispatcher = dispatcher
    this.messages = []
    this.loading = set()
    this.searching = map()
    this.visible = false
    this.logsDisplayLength = logsDisplayLength
    this.numErrors = 0
    this.showErrorsLogs = false
  }
  showTooltip (type) {
    this.tooltip = type
    this.setUpdate('tooltip')
  }
  toggleErrorLogs () {
    this.showErrorsLogs = !this.showErrorsLogs
    if (this.showErrorsLogs) this.show()
    this.setUpdate('logs')
  }
  show () {
    if (!this.visible) {
      this.visible = true
      this.setUpdate('visibility')
    }
  }
  hide () {
    if (this.visible) {
      this.visible = false
      this.setUpdate('visibility')
    }
  }
  add (msg) {
    this.messages.push(msg)
    this.setUpdate('add')
  }
  moreLogs () {
    this.logsDisplayLength += logsDisplayLength
    this.setUpdate('logs')
  }
  addLoading (id) {
    this.loading.add(id)
    this.setUpdate('loading')
  }
  removeLoading (id) {
    this.loading.remove(id)
    this.setUpdate('loading')
  }
  addSearching (search) {
    this.searching.set(String(search.id) + String(search.isOutgoing), search)
    this.setUpdate('loading')
  }
  removeSearching (search) {
    this.searching.remove(String(search.id) + String(search.isOutgoing))
    this.setUpdate('loading')
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (!this.shouldUpdate()) return this.root
    if (this.shouldUpdate(true)) {
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
      super.render()
      return
    }
    if (this.shouldUpdate('loading')) {
      this.renderLoading()
    }
    if (this.shouldUpdate('add')) {
      let i = this.messages.length - 1
      this.renderLogs(this.messages[i], i)
    }
    if (this.shouldUpdate('logs')) {
      this.renderLogs()
    }
    if (this.shouldUpdate('tooltip')) {
      this.renderTooltip()
    }
    if (this.shouldUpdate('visibility')) {
      this.renderVisibility()
    }
    super.render()
  }
  renderTooltip () {
    if (this.loading.size() > 0 || this.searching.size() > 0) return
    let top = this.root.querySelector('#topmsg')
    let tip = this.makeTooltip(this.tooltip)
    top.innerHTML = tip
  }
  makeTooltip (type) {
    switch (type) {
      case 'entity':
        return 'An entity represents an entity dealing with one or more addresses.'
      case 'address':
        return 'An address which can receive and spend coins.'
      case 'link':
        return 'A link indicates that there exist one or more transactions between the nodes. Flow is always from left to right.'
      case 'shadow':
        return 'A shadow link connects identical addresses and entities'
    }
    return ''
  }
  renderLoading () {
    let top = this.root.querySelector('#topmsg')
    if (this.loading.size() > 0) {
      addClass(this.root, 'loading')
      let msg = 'Loading '
      let v = this.loading.values()
      msg += v.slice(0, 3).join(', ')
      msg += v.length > 3 ? ` + ${v.length - 3}` : ''
      msg += ' ...'
      top.innerHTML = msg
    } else if (this.searching.size() > 0) {
      addClass(this.root, 'loading')
      let search = this.searching.values()[0]
      let outgoing = search.isOutgoing ? 'outgoing' : 'incoming'
      let crit = ''
      if (search.params.category) crit = `category ${search.params.category}`
      if (search.params.addresses.length > 0) crit = 'addresses ' + search.params.addresses.join(',')
      let msg = `Searching for ${outgoing} neighbors of ${search.type} ${search.id[0]} with ${crit} (depth: ${search.depth}, breadth: ${search.breadth}, skip if more than ${search.skipNumAddresses} addresses) ...`
      top.innerHTML = msg
    } else {
      removeClass(this.root, 'loading')
      if (top) top.innerHTML = ''
    }
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
    if (!this.visible) {
      removeClass(this.root, 'visible')
    } else {
      addClass(this.root, 'visible')
    }
  }
  msg (type) {
    let args = Array.prototype.slice.call(arguments, 1)
    logger.debug('msg', type, args)
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
        logger.debug('loadfile msg', filename)
        return `Loading file ${filename} ...`
      case 'loadedFile':
        let filename_ = args[0]
        logger.debug('loadedfile msg', filename_)
        return `Loaded file ${filename_}`
      case 'loadingEntityFor':
        return `Loading entity for ${args[0]}`
      case 'loadedEntityFor':
        return `Loaded entity for ${args[0]}`
      case 'noEntityFor':
        return `No entity for ${args[0]}`
      case 'loadingTagsFor':
        return `Loading tags for ${args[0]} ${args[1]}`
      case 'loadedTagsFor':
        return `Loaded tags for ${args[0]} ${args[1]}`
      case 'loadingEntityAddresses':
        return `Trying to load ${args[1]} addresses for entity ${args[0]}`
      case 'loadedEntityAddresses':
        return `Loaded ${args[1]} addresses for entity ${args[0]}`
      case 'removeNode':
        return `Removed node of ${args[0]} ${args[1]}`
      case 'searchResult':
        return `Found ${args[0]} paths to ${args[1]} nodes`
      case 'error':
        this.numErrors++
        return {error: args[0]}
      default:
        logger.warn('unhandled status message type', type)
    }
  }
  addMsg () {
    this.add(this.msg(...arguments))
  }
}
