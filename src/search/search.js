import search from './search.html'
import Component from '../component.js'
import {replace, addClass, removeClass} from '../template_utils.js'
import Logger from '../logger.js'
import {firstToUpper} from '../utils.js'
import {currencies} from '../globals.js'

const logger = Logger.create('Search') // eslint-disable-line no-unused-vars

const empty = {addresses: [], transactions: [], labels: []}
const numShowResults = 7

const byPrefix = term => addr => addr.toLowerCase().startsWith(term.toLowerCase())

export default class Search extends Component {
  constructor (dispatcher, keyspaces, types, isInDialog = false) {
    super()
    this.types = types || Object.keys(empty).concat(['blocks'])
    this.dispatcher = dispatcher
    this.term = ''
    this.resultTerm = ''
    this.isInDialog = isInDialog
    this.keyspaces = keyspaces
    this.timeout = {}
    this.keyspaces.forEach(key => {
      this.timeout[key] = null
    })
    this.result = {}
    this.clearResults()
  }
  setStats (stats) {
    this.stats = stats
  }
  clearResults () {
    this.result = {}
    this.keyspaces.forEach(keyspace => {
      this.result[keyspace] = {...empty}
    })
    this.resultLabels = {labels: []}
  }
  clear () {
    this.clearResults()
    this.term = ''
    this.isLoading = false
    this.setUpdate(true)
  }
  error (keyspace, msg) {
    this.result[keyspace].error = msg
    this.setUpdate('result')
  }
  errorLabels (msg) {
    this.resultLabels.error = msg
    this.setUpdate('result')
  }
  showLoading () {
    if (!this.isLoading) {
      this.isLoading = true
      this.setUpdate('result')
    }
  }
  hideLoading () {
    if (this.isLoading) {
      this.isLoading = false
      this.setUpdate('result')
    }
  }
  renderLoading () {
    if (this.isLoading) {
      // removeClass(this.root.querySelector('#browser-search-result'), 'hidden')
      this.root.querySelector('#indicator').style.display = 'inline'
    } else {
      // addClass(this.root.querySelector('#browser-search-result'), 'hidden')
      this.root.querySelector('#indicator').style.display = 'none'
    }
  }
  typesToPlaceholder () {
    return firstToUpper(this.types.map(type => {
      switch (type) {
        case 'addresses' : return 'addresses'
        case 'transactions' : return 'transaction'
        case 'blocks' : return 'block'
        case 'labels' : return 'label'
      }
    }).join(', '))
  }
  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (!this.shouldUpdate()) return
    if (this.shouldUpdate(true)) {
      super.render()
      let placeholder = this.typesToPlaceholder()
      logger.debug('placeholder', placeholder)
      this.root.innerHTML = replace(search, {placeholder})
      this.input = this.root.querySelector('textarea')
      this.renderTerm()
      this.form = this.root.querySelector('form')
      this.form.addEventListener('submit', (e) => {
        e.returnValue = false
        e.preventDefault()
        for (let keyspace in this.result) {
          if (this.types.indexOf('addresses') !== -1 && this.result[keyspace].addresses.length > 0) {
            let addresses = this.result[keyspace].addresses.filter(byPrefix(this.term))
            this.dispatcher('clickSearchResult', {id: addresses[0], type: 'address', keyspace, isInDialog: this.isInDialog})
            return false
          }
          if (this.types.indexOf('transactions') !== -1 && this.result[keyspace].transactions.length > 0) {
            let transactions = this.result[keyspace].transactions.filter(byPrefix(this.term))
            this.dispatcher('clickSearchResult', {id: transactions[0], type: 'transaction', keyspace, isInDialog: this.isInDialog})
            return false
          }
          if (this.types.indexOf('labels') !== -1 && this.resultLabels.labels.length > 0) {
            let labels = this.resultLabels.labels.filter(byPrefix(this.term))
            this.dispatcher('clickSearchResult', {id: labels[0], type: 'label', keyspace, isInDialog: this.isInDialog})
            return false
          }
          let blocks = this.blocklist(3, keyspace, this.term)
          if (this.types.indexOf('blocks') !== -1 && blocks.length > 0) {
            this.dispatcher('clickSearchResult', {id: blocks[0], type: 'block', keyspace, isInDialog: this.isInDialog})
            return false
          }
        }
        this.term.split('\n').forEach((address) => {
          this.keyspaces.forEach(keyspace => {
            this.dispatcher('clickSearchResult', {id: address, type: 'address', keyspace, isInDialog: this.isInDialog})
          })
        })
        return false
      })
      this.input.addEventListener('keypress', (e) => {
        if (e.key !== 'Enter') return
        e.preventDefault()
        this.form.querySelector('button[type=\'submit\']').click()
      })
      this.input.addEventListener('input', (e) => {
        this.dispatcher('search', {
          term: e.target.value,
          types: this.types,
          keyspaces: this.keyspaces,
          isInDialog: this.isInDialog
        })
      })
      this.input.addEventListener('blur', () => {
      // wrap in timeout to let possible clicksearchresult event happen
        setTimeout(() => this.dispatcher('blurSearch'), 200)
      })
      this.renderResult()
      return this.root
    }
    if (this.shouldUpdate('result')) {
      this.renderResult()
    }
    if (this.shouldUpdate('term')) {
      this.renderTerm()
    }
    super.render()
    return this.root
  }
  renderTerm () {
    if (!this.input) return
    this.input.value = this.term
    let lines = this.term.split('\n')
    if (lines.length === 1) {
      this.input.style.height = '100%'
      return
    }
    this.input.style.height = (lines.length + 1) * 1.13 + 'em'
  }
  setSearchTerm (term, prefixLength) {
    this.term = term.split('\n').filter(line => line).join('\n')
    this.setUpdate('result')
    this.setUpdate('term')
    if (this.term.length < prefixLength) {
      this.clearResults()
    }
  }
  isMultiline () {
    return this.term.indexOf('\n') !== -1
  }
  needsResults (keyspace, limit, prefixLength) {
    if (this.isMultiline()) return false
    if (this.term.length < prefixLength) return false
    let alen = this.result[keyspace].addresses.length
    let tlen = this.result[keyspace].transactions.length
    return !(((alen !== 0 && alen < limit) || (tlen !== 0 && tlen < limit)) && this.term.startsWith(this.resultTerm))
  }
  needsLabelResults (limit, prefixLength) {
    if (this.term.length < prefixLength) return false
    let len = this.resultLabels.labels.length
    return !(((len !== 0 && len < limit)) && this.term.startsWith(this.resultTerm))
  }
  renderOptions () {
    return null
  }

  renderResult () {
    let frame = this.root.querySelector('#browser-search-result')
    let el = frame.querySelector('#result')
    el.innerHTML = ''

    let visible = this.isLoading
    let allErrors = true
    let searchLine = (keyspace, ul) => (type, icon) => (id) => {
      let li = document.createElement('li')
      li.className = 'cursor-pointer'
      li.innerHTML = `<i class="fas fa-${icon} pr-1 text-grey text-sm"></i>${id}`
      li.addEventListener('click', () => {
        this.dispatcher('clickSearchResult', {id, type, keyspace, isInDialog: this.isInDialog})
      })
      ul.appendChild(li)
    }
    this.keyspaces.forEach(keyspace => {
      let addresses = this.result[keyspace].addresses
        .filter(byPrefix(this.term))
        .slice(0, numShowResults)

      let transactions = this.result[keyspace].transactions
        .filter(byPrefix(this.term))
        .slice(0, numShowResults)

      let blocks = this.blocklist(3, keyspace, this.term)

      let keyspaceVisible =
        this.result[keyspace].error ||
        addresses.length > 0 ||
        transactions.length > 0 ||
        blocks.length > 0
      visible = visible || keyspaceVisible
      if (this.result[keyspace].error) {
        return
      }
      allErrors = false

      // if no results to render don't draw the title and the list at all
      if (!keyspaceVisible) return

      let ul = document.createElement('ol')
      ul.className = 'list-reset'
      let searchLine_ = searchLine(keyspace, ul)
      addresses.forEach(searchLine_('address', 'at'))
      transactions.forEach(searchLine_('transaction', 'exchange-alt'))
      blocks.forEach(searchLine_('block', 'cube'))
      let title = document.createElement('div')
      title.className = 'font-bold py-1'
      title.appendChild(document.createTextNode(currencies[keyspace]))
      el.appendChild(title)
      el.appendChild(ul)
    })
    let labels = this.resultLabels.labels
      .filter(byPrefix(this.term))
      .slice(0, numShowResults)
    logger.debug('labels', labels)
    if (labels.length > 0) {
      visible = true
      allErrors = false
      let ul = document.createElement('ol')
      ul.className = 'list-reset'
      labels.forEach(searchLine(null, ul)('label', 'tag'))
      let title = document.createElement('div')
      title.className = 'font-bold py-1'
      title.appendChild(document.createTextNode('Labels'))
      el.appendChild(title)
      el.appendChild(ul)
    }

    if (allErrors) {
      el.innerHTML = `Failed to fetch from any keyspaces`
      addClass(el, 'text-gs-red')
    } else {
      removeClass(el, 'text-gs-red')
    }
    if (visible) {
      addClass(frame, 'block')
      removeClass(frame, 'hidden')
    } else {
      removeClass(frame, 'block')
      addClass(frame, 'hidden')
    }
    this.renderLoading()
  }
  setResult (term, result) {
    if (term !== this.term) return
    this.result[result.keyspace] = {
      addresses: result.addresses || [],
      transactions: result.transactions || []
    }
    this.resultTerm = term
    this.setUpdate('result')
  }
  setResultLabels (term, result) {
    logger.debug('set result', result, term, this.term)
    if (term !== this.term) return
    this.resultLabels.labels = result.labels
    this.resultTerm = term
    this.setUpdate('result')
  }
  blocklist (limit, keyspace, prefix) {
    if (!this.stats || !this.stats[keyspace]) return []
    prefix = prefix * 1
    if (typeof prefix !== 'number') return []
    if (prefix <= 0) return []
    if (prefix < this.stats[keyspace].no_blocks) {
      return [prefix]
    }
    return []
  }
}
