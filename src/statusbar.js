import status from './status/status.html'
import Component from './component.js'
import { addClass, removeClass } from './template_utils.js'
import Logger from './logger.js'
import { t, tt } from './lang.js'
import { firstToUpper } from './utils.js'

const logger = Logger.create('Statusbar') // eslint-disable-line no-unused-vars

const logsDisplayLength = 100

export default class Statusbar extends Component {
  constructor (dispatcher, rest) {
    super()
    this.dispatcher = dispatcher
    this.messages = []
    this.loading = new Set()
    this.searching = new Map()
    this.visible = false
    this.logsDisplayLength = logsDisplayLength
    this.numErrors = 0
    this.showErrorsLogs = false
    this.rest = rest
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
    if (msg[0] === 'error') this.numErrors++
    this.messages.push(msg)
    if (msg[0] === 'loading') {
      this.setUpdate('loading')
    }
    this.setUpdate('add')
  }

  moreLogs () {
    this.logsDisplayLength += logsDisplayLength
    this.setUpdate('logs')
  }

  addLoading (id) {
    this.loading.add(id + '')
    this.setUpdate('loading')
  }

  removeLoading (id) {
    this.loading.delete(id + '')
    this.setUpdate('loading')
  }

  addSearching (search) {
    this.searching.set(String(search.id) + String(search.isOutgoing), search)
    this.setUpdate('loading')
  }

  removeSearching (search) {
    this.searching.delete(String(search.id) + String(search.isOutgoing))
    this.setUpdate('loading')
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (this.shouldUpdate(true)) {
      this.root.innerHTML = tt(status)

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
      this.renderRatelimit()
      super.render()
      return
    }
    if (this.shouldUpdate('loading')) {
      this.renderLoading()
    }
    if (this.shouldUpdate('add')) {
      const i = this.messages.length - 1
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
    this.renderRatelimit()
    super.render()
  }

  renderRatelimit () {
    if (!this.rest.ratelimitLimit) return
    console.log('ratelimit ', this.rest.ratelimitLimit)
    const top = this.root.querySelector('#topmsg')
    const rlm = t('API request limit')
    if (top.innerHTML && !top.innerHTML.startsWith(rlm)) return
    const countdown = Math.max(0, this.rest.ratelimitReset - Math.floor(Date.now() / 1000))
    const remaining = countdown > 0 ? this.rest.ratelimitRemaining : this.rest.ratelimitLimit
    let msg = rlm + `: ${remaining}/${this.rest.ratelimitLimit}`
    if (remaining < 10) {
      msg += ', ' + t('reset in %0 s', countdown)
      setTimeout(() => this.dispatcher('countdownRatelimitReset'), 1000)
    }
    top.innerHTML = msg
  }

  renderTooltip () {
    if (this.loading.size > 0 || this.searching.size > 0) return
    const top = this.root.querySelector('#topmsg')
    const tip = this.makeTooltip(this.tooltip)
    top.innerHTML = tip
  }

  makeTooltip (type) {
    const key = type + '_tooltip'
    const tooltip = t(key)
    if (tooltip === key) return ''
    return tooltip
  }

  renderLoading () {
    const top = this.root.querySelector('#topmsg')
    if (this.loading.size > 0) {
      addClass(this.root, 'loading')
      const v = [...this.loading.values()]
      let thing = ''
      thing += v.slice(0, 3).join(', ')
      thing += v.length > 3 ? ` + ${v.length - 3}` : ''
      const msg = t('Loading_thing', thing) + ' ...'
      top.innerHTML = msg
    } else if (this.searching.size > 0) {
      addClass(this.root, 'loading')
      const search = [...this.searching.values()][0]
      const outgoing = 'searching ' + (search.isOutgoing ? 'outgoing' : 'incoming')
      let crit = ''
      if (search.params.category) crit = t('searching criterion category_name', search.params.category)
      if (search.params.addresses.length > 0) crit = t('searching criterion addresses_ids', search.params.addresses.join(','))
      if (search.params.field) {
        const range = 'searching ' + (search.params.min && search.params.max ? 'between' : (search.params.max ? 'max' : 'min'))
        crit = t('searching criterion ' + search.params.field, t(range, search.params.min, search.params.max))
      }
      const msg = t('Searching', t(outgoing), search.id[0], crit, search.depth, search.breadth, search.skipNumAddresses) + ' ...'
      top.innerHTML = msg
    } else {
      removeClass(this.root, 'loading')
      top.innerHTML = ''
    }
  }

  renderLogs (msg, index) {
    const logs = this.root.querySelector('ul#log-messages')
    let messages = this.messages
    const errorMsg = this.root.querySelector('#errorMsg')
    if (this.showErrorsLogs) {
      errorMsg.innerHTML = t('Errors only')
      messages = messages.filter(msg => typeof msg !== 'string' && msg[0] === 'error')
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
      const more = document.createElement('li')
      more.className = 'cursor-pointer text-gs-dark'
      more.innerHTML = t('Show more') + ' ...'
      more.addEventListener('click', () => {
        this.dispatcher('moreLogs')
      })
      logs.appendChild(more)
    }
    if (this.numErrors > 0) {
      removeClass(this.root.querySelector('#errors span'), 'hidden')
    }
  }

  renderLogMsg (root, msg, index) {
    const el = document.createElement('li')
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

    if (msg[0] === 'error') {
      let message
      if (msg[1].requestURL) {
        let m = msg[1].message || msg[1].detail
        if (m.startsWith('429')) {
          m = t('API rate limit exceeded')
        }
        message = t('Error requesting', msg[1].requestURL, m)
      } else {
        message = msg[1]
      }
      return this.printError(message)
    }

    if (Array.isArray(msg)) {
      return this.msg(...msg)
    }
  }

  printError (message) {
    return `<span class="text-gs-red">${message}</span>`
  }

  renderVisibility () {
    if (!this.visible) {
      removeClass(this.root, 'visible')
    } else {
      addClass(this.root, 'visible')
    }
  }

  msg (type) {
    const args = Array.prototype.slice.call(arguments, 1)
    switch (type) {
      case 'loading' :
        args[0] = t(args[0])
        return firstToUpper(t('loading_type_id', ...args))
      case 'loadingLinkTransactions' :
        return t('Loading transactions between', ...args) + ' ...'
      case 'loaded' :
        return t('Loaded', t(args[0]), args[1] || '')
      case 'loadingNeighbors':
      {
        const dir = 'loading msg ' + (args[2] ? 'outgoing' : 'incoming')
        return t('Loading neighbors for', t(dir), t(args[1]), args[0]) + ' ...'
      }
      case 'loadedNeighbors':
      {
        const dir_ = 'loading msg ' + (args[2] ? 'outgoing' : 'incoming')
        return t('Loaded neighbors for', t(dir_), t(args[1]), args[0])
      }
      case 'saving':
        return t('Saving to file') + ' ...'
      case 'saved':
        return t('Saved to file', args[0])
      case 'loadFile':
      {
        const filename = args[0]
        return t('Loading file', filename) + ' ...'
      }
      case 'loadedFile':
      {
        const filename_ = args[0]
        return t('Loaded file', filename_) + ' ...'
      }
      case 'loadingEntityFor':
        return t('Loading entity for', ...args)
      case 'loadedEntityFor':
        return t('Loaded entity for', ...args)
      case 'noEntityFor':
        return t('No entity for', ...args)
      case 'loadingTagsFor':
        return t('Loading tags for', ...args)
      case 'loadedTagsFor':
        return t('Loaded tags for', ...args)
      case 'loadingEntityAddresses':
        return t('Trying to load addresses for entity', ...args)
      case 'loadedEntityAddresses':
        return t('Loaded addresses for entity', ...args)
      case 'removeNode':
        return t('Removed node of', ...args)
      case 'searchResult':
        return t('Found paths to nodes', ...args)
      case 'error':
        return { error: args[0] }
      default:
        logger.warn('unhandled status message type', type)
    }
  }

  addMsg () {
    this.add([...arguments])
  }
}
