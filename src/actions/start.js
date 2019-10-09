import {labelPrefixLength, searchlimit, prefixLength} from '../globals.js'
import Logger from '../logger.js'
const logger = Logger.create('Actions') // eslint-disable-line no-unused-vars

const stats = function () {
  this.mapResult(this.rest.stats(), 'receiveStats')
}

const receiveStats = function ({context, result}) {
  this.keyspaces = Object.keys(result)
  this.stats = {...result}
  this.landingpage.setStats({...result})
}

const search = function ({term, types, keyspaces, isInDialog}) {
  let search = isInDialog ? this.menu.search : this.search
  if (!search) return
  search.setSearchTerm(term, labelPrefixLength)
  search.hideLoading()
  keyspaces.forEach(keyspace => {
    if (search.needsResults(keyspace, searchlimit, prefixLength)) {
      if (search.timeout[keyspace]) clearTimeout(search.timeout[keyspace])
      search.showLoading()
      search.timeout[keyspace] = setTimeout(() => {
        if (types.indexOf('addresses') !== -1 || types.indexOf('transactions') !== -1) {
          this.mapResult(this.rest.search(keyspace, term, searchlimit), 'searchresult', {term, isInDialog})
        }
      }, 250)
    }
  })
  if (search.needsLabelResults(searchlimit, labelPrefixLength)) {
    if (search.timeoutLabels) clearTimeout(search.timeoutLabels)
    search.showLoading()
    search.timeoutLabels = setTimeout(() => {
      if (types.indexOf('labels') !== -1) {
        this.mapResult(this.rest.searchLabels(term, searchlimit), 'searchresultLabels', {term, isInDialog})
      }
    }, 250)
  }
}

const searchresult = function ({context, result}) {
  let search = context.isInDialog ? this.menu.search : this.search
  if (!search) return
  search.hideLoading()
  search.setResult(context.term, result)
}

const searchresultLabels = function ({context, result}) {
  let search = context.isInDialog ? this.menu.search : this.search
  logger.debug('search', search)
  if (!search) return
  search.hideLoading()
  search.setResultLabels(context.term, result)
}

const login = function ([username, password]) {
  logger.debug('login, this is', this)
  this.login.loading(true)
  this.mapResult(this.rest.login(username, password), 'loginResult')
}

const refreshResult = function ({result}) {
  logger.debug('refreshResult', result)
  if (result.refreshed) return this.call('loginResult', {result : {loggedin: true}})
}

const loginResult = function ({result}) {
  logger.debug('loginResult', result)
  if (result.loggedin) {
    if (!this.isStart) return this.call('appLoaded')
    import('../app.js').then(app => { // works despite of parsing error of eslint
      this.call('appLoaded')
      this.app = new app.default(this.locale, this.rest, this.stats)
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
          '../config/address.html',
          '../config/entity.html',
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

          this.app = new app.default(this.locale, this.rest, this.stats)
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

const fetchError = function ({context, msg, error}) {
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
      let search = context && context.isInDialog ? this.menu.search : this.search
      if (!search) return
      search.hideLoading()
      search.error(error.keyspace, error.message)
      // this.statusbar.addMsg('error', error)
      break
    case 'searchresultLabels': {
      let search = context && context.isInDialog ? this.menu.search : this.search
      if (!search) return
      search.hideLoading()
      search.errorLabels(error.message)
      // this.statusbar.addMsg('error', error)
    }
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

export default {
  stats,
  receiveStats,
  search,
  searchresult,
  searchresultLabels,
  login,
  loginResult,
  appLoaded,
  fetchError,
  refreshResult
}
