import {labelPrefixLength, searchlimit, prefixLength} from '../globals.js'
import Logger from '../logger.js'
const logger = Logger.create('Actions') // eslint-disable-line no-unused-vars

const stats = function () {
  this.mapResult(this.rest.stats(), 'receiveStats')
}

const receiveStats = function ({context, result}) {
  this.keyspaces = Object.keys(result)
  this.landingpage.setStats({...result})
}

const search = function ({term, types, keyspaces, isInDialog}) {
  logger.debug('this is', this)
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
  this.login.loading(true)
  this.mapResult(this.rest.login(username, password), 'loginResult')
}

const loginResult = function ({result}) {
  logger.debug('loginResult', result)
  if (result.access_token && result.refresh_token) {
    this.rest.setAccessToken(result.access_token)
    this.rest.setRefreshToken(result.refresh_token)
    import('../app.js').then(app => { // works despite of parsing error of eslint
      this.app = new app.default(this.locale, this.rest, this.login, this.search, this.landingpage)
      this.app.root = this.root
      this.call('appLoaded')
    })
    return
  }
  this.login.error(result.message || 'Something went wrong')
  this.login.loading(false)
}

const appLoaded = function() {
  this.login.loading(false)
  this.landingpage.setSearch(this.search)
}

const fetchError = function({context, msg, error}) {
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
  fetchError
}
