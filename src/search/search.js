import search from './search.html'
import Component from '../component.js'
import {replace, addClass, removeClass} from '../template_utils.js'
import Logger from '../logger.js'
import {firstToUpper} from '../utils.js'
import {currencies} from '../globals.js'

const logger = Logger.create('Search') // eslint-disable-line no-unused-vars

const numShowResults = 7

const byPrefix = term => addr => addr.toLowerCase().startsWith(term.toLowerCase())

export default class Search extends Component {
  constructor (dispatcher, types, context) {
    super()
    this.types = types
    this.dispatcher = dispatcher
    this.term = ''
    this.resultTerm = ''
    this.context = context
    this.keyspaces = []
    this.result = []
    this.resultLabels = []
  }
  setStats (stats) {
    this.stats = stats
    this.setKeyspaces(Object.keys(this.stats))
  }
  setKeyspaces (keyspaces) {
    this.keyspaces = keyspaces
    this.clearResults()
  }
  clearResults () {
    this.result = []
    this.resultLabels = []
  }
  clear () {
    this.clearResults()
    this.term = ''
    this.isLoading = false
    this.setUpdate(true)
  }
  error (keyspace, msg) {
    this.result = msg
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
      this.root.innerHTML = replace(search, {placeholder})
      this.input = this.root.querySelector('textarea')
      this.renderTerm()
      this.form = this.root.querySelector('form')
      this.form.addEventListener('submit', (e) => {
        e.returnValue = false
        e.preventDefault()
        this.dispatcher('submitSearchResult', {term: this.term, context: this.context})
        return false
      })
      this.input.addEventListener('keypress', (e) => {
        if (e.key !== 'Enter') return
        e.preventDefault()
        this.form.querySelector('button[type=\'submit\']').click()
      })
      this.input.addEventListener('input', (e) => {
        this.dispatcher('search', {
          term: e.target.value.trim(),
          context: this.context
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
  needsResults (limit, prefixLength) {
    if (this.isMultiline()) return false
    if (this.term.length < prefixLength) return false
    let alen = Infinity
    let tlen = Infinity
    this.result.forEach(set => {
      alen = Math.min(set.addresses.length, alen)
      tlen = Math.min(set.txs.length, tlen)
    })
    return !(((alen !== 0 && alen < limit) || (tlen !== 0 && tlen < limit)) && this.term.startsWith(this.resultTerm))
  }
  renderOptions () {
    return null
  }

  renderResult () {
    let frame = this.root.querySelector('#browser-search-result')
    let el = frame.querySelector('#result')
    el.innerHTML = ''

    let visible = this.isLoading
    let searchLine = (keyspace, ul) => (type, icon) => (id) => {
      let li = document.createElement('li')
      li.className = 'cursor-pointer'
      li.innerHTML = `<i class="fas fa-${icon} pr-1 text-grey text-sm"></i>${id}`
      li.addEventListener('click', () => {
        this.dispatcher('clickSearchResult', {id, type, keyspace, context: this.context})
      })
      ul.appendChild(li)
    }
    if (typeof this.result === 'string') {
      el.innerHTML = `Failed to fetch from any keyspaces`
      addClass(el, 'text-gs-red')
      return
    } else {
      removeClass(el, 'text-gs-red')
    }
    this.keyspaces.forEach(keyspace => {
      let result = this.result.filter(({currency}) => currency === keyspace)
      let addresses = []
      let transactions = []
      if (result.length !== 0) {
        result = result[0]
        addresses = result.addresses
          .filter(byPrefix(this.term))
          .slice(0, numShowResults)

        transactions = result.txs
          .filter(byPrefix(this.term))
          .slice(0, numShowResults)
      }

      let blocks = this.blocklist(3, keyspace, this.term)

      let keyspaceVisible =
        addresses.length > 0 ||
        transactions.length > 0 ||
        blocks.length > 0
      visible = visible || keyspaceVisible
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
    let labels = this.resultLabels
      .filter(byPrefix(this.term))
      .slice(0, numShowResults)
    if (labels.length > 0) {
      visible = true
      let ul = document.createElement('ol')
      ul.className = 'list-reset'
      labels.forEach(searchLine(null, ul)('label', 'tag'))
      let title = document.createElement('div')
      title.className = 'font-bold py-1'
      title.appendChild(document.createTextNode('Labels'))
      el.appendChild(title)
      el.appendChild(ul)
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
    this.result = result['currencies']
    this.resultLabels = result['labels']
    this.resultTerm = term
    this.setUpdate('result')
  }
  getFirstResult () {
    for (let i in this.result) {
      let resultSet = this.result[i]
      if (this.types.indexOf('addresses') !== -1 && resultSet.addresses.length > 0) {
        let addresses = resultSet.addresses.filter(byPrefix(this.term))
        return {id: addresses[0], type: 'address', keyspace: resultSet['currency']}
      }
      if (this.types.indexOf('transactions') !== -1 && resultSet.txs.length > 0) {
        let transactions = resultSet.txs.filter(byPrefix(this.term))
        return {id: transactions[0], type: 'transaction', keyspace: resultSet['currency']}
      }
    }
    if (this.types.indexOf('labels') !== -1 && this.resultLabels.length > 0) {
      let labels = this.resultLabels.filter(byPrefix(this.term))
      return {id: labels[0], type: 'label'}
    }
    for (let i in this.keyspaces) {
      let keyspace = this.keyspaces[i]
      let blocks = this.blocklist(3, keyspace, this.term)
      if (this.types.indexOf('blocks') !== -1 && blocks.length > 0) {
        return {id: blocks[0], type: 'block', keyspace}
      }
    }
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
