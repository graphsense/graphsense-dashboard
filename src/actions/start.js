import { labelPrefixLength, searchlimit, prefixLength } from '../globals.js'
import Logger from '../logger.js'
const logger = Logger.create('Actions') // eslint-disable-line no-unused-vars

const stats = function () {
  this.mapResult(this.rest.stats(), 'receiveStats')
}

const receiveStats = function ({ context, result }) {
  this.keyspaces = Object.keys(result)
  this.stats = { ...result }
  this.landingpage.setStats({ ...result.currencies })
  if (this.browser) {
    this.browser.setKeyspaces(this.keyspaces)
  }
}

const search = function ({ term, context }) {
  const search = context === 'search' ? this.search : this.menu.search
  if (!search) return
  search.setSearchTerm(term, labelPrefixLength)
  search.hideLoading()
  if (search.needsResults(searchlimit, prefixLength)) {
    if (search.timeout) clearTimeout(search.timeout)
    search.showLoading()
    search.timeout = setTimeout(() => {
      this.mapResult(this.rest.search(term.trim(), searchlimit), 'searchresult', { term, dialogContext: context })
    }, 250)
  }
}

const searchresult = function ({ context, result }) {
  const search = context.dialogContext === 'search' ? this.search : this.menu.search
  if (!search) return
  search.hideLoading()
  search.setResult(context.term, result)
}

const login = function ([username, password]) {
  logger.debug('login, this is', this)
  this.login.loading(true)
  this.mapResult(this.rest.login(username, password), 'loginResult')
}

const refreshResult = function ({ result }) {
  logger.debug('refreshResult', result)
  if (result.status === 'success') return this.call('loginResult', { result })
}

const loginResult = function ({ result }) {
  logger.debug('loginResult', result)
  if (result.status === 'success') {
    if (!this.isStart) return this.call('appLoaded')
    import('../app.js').then(app => { // works despite of parsing error of eslint
      this.call('appLoaded')
      this.app = new app.default(this.locale, this.rest, this.stats, this.reportLogger) // eslint-disable-line new-cap
      if (module.hot) {
        module.hot.accept([
          '../browser.js',
          '../browser/address.html',
          '../browser/address.js',
          '../browser/addresses_table.js',
          '../browser/entity.html',
          '../browser/entity.js',
          '../browser/component.js',
          '../browser/layout.html',
          '../browser/option.html',
          '../search/search.html',
          '../search/search.js',
          '../login/login.html',
          '../login/login.js',
          '../status/status.html',
          '../statusbar.js',
          '../browser/table.html',
          '../browser/table.js',
          '../browser/tags_table.js',
          '../browser/transaction.html',
          '../browser/transaction.js',
          '../browser/transaction_addresses_table.js',
          '../browser/transactions_table.js',
          '../nodeGraph.js',
          '../nodeGraph/addressNode.js',
          '../nodeGraph/entityNode.js',
          '../nodeGraph/graphNode.js',
          '../nodeGraph/layer.js',
          '../config.js',
          '../config/filter.html',
          '../config/graph.html',
          '../config/layout.html',
          '../layout.js',
          '../layout/layout.html',
          '../component.js',
          '../app.js',
          '../config.js',
          '../start.js',
          '../rest.js',
          '../store.js',
          '../template_utils.js',
          '../utils.js'
        ], () => {
          // dispatcher.history = [debugHistory[0]]

          this.app = new app.default(this.locale, this.rest, this.stats) // eslint-disable-line new-cap
          this.app.replay()
          this.app.render(document.body)
        })
      }
    })
    return
  }
  this.login.error(result.message || 'Something went wrong')
  this.login.loading(false)
}

const appLoaded = function () {
  this.login.loading(false)
  if (this.showLandingpage) {
    this.landingpage.setSearch(this.search)
  } else {
    this.layout.showLogin(false)
  }
}

const fetchError = function ({ context, msg, error }) {
  if (error.message.startsWith('401')) {
    this.login.loading(false)
    this.login.clear()
    this.login.error('Please fill in your credentials')
    if (this.showLandingpage) {
      this.landingpage.setLogin(this.login)
    } else {
      this.layout.showLogin(true)
    }
    return
  }
  switch (msg) {
    case 'loginResult':
      this.login.error(error.message || 'Something went wrong')
      this.login.loading(false)
      break
    case 'searchresult':
      {
        const search = context && context.isInDialog ? this.menu.search : this.search
        if (!search) return
        search.hideLoading()
        search.error(error.keyspace, error.message)
      }
      // this.statusbar.addMsg('error', error)
      break
    case 'resultSearchNeighbors':
      this.statusbar.removeSearching(context)
      this.statusbar.addMsg('error', error)
      break
    case 'resultNode':
      this.statusbar.removeLoading((context && context.data && context.data.id) || context)
      this.statusbar.addMsg('error', error)
      break
    case 'resultTransactionForBrowser':
      this.statusbar.removeLoading(context)
      break
    case 'resultBlockForBrowser':
      this.statusbar.removeLoading(context)
      break
    case 'resultLabelForBrowser':
      this.statusbar.removeLoading(context)
      break
    case 'resultEgonet':
      this.statusbar.removeLoading(`neighbors of ${context.type} ${context.id[0]}`)
      break
    case 'resultEntityAddresses':
      this.statusbar.removeLoading('addresses of entity ' + context[0])
      break
    default:
      this.statusbar.addMsg('error', error)
  }
}

const jumpToApp = function () {
  this.showLandingpage = false
  this.layout.setUpdate(true)
}

export default {
  stats,
  receiveStats,
  search,
  searchresult,
  login,
  loginResult,
  appLoaded,
  fetchError,
  refreshResult,
  jumpToApp
}
