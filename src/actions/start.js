import { searchlimit, prefixLength } from '../globals.js'
import { text } from 'd3-fetch'
import YAML from 'yaml'
import Logger from '../logger.js'
import { setLanguagePack, setDTLanguagePack } from '../lang.js'
import numeral from 'numeral'
import moment from 'moment'
/* develblock:start */
import Model from '../app.js'
/* develblock:end */

const logger = Logger.create('Actions') // eslint-disable-line no-unused-vars

const stats = function () {
  this.mapResult(this.rest.stats(), 'receiveStats')
}

const receiveStats = function ({ context, result }) {
  if (!result) return
  this.keyspaces = (result.currencies || []).map(c => c.name)
  this.stats = { ...result }
  this.landingpage.setStats([...result.currencies])
  if (this.browser) {
    this.browser.setKeyspaces(this.keyspaces)
  }
}

const search = function ({ term, context }) {
  const search = context === 'search' ? this.search : this.menu.search
  if (!search) return
  search.setSearchTerm(term, prefixLength)
  search.hideLoading()
  if (search.needsResults(searchlimit, prefixLength)) {
    if (search.timeout) clearTimeout(search.timeout)
    if (search.abortController) {
      search.abortController.abort()
      search.abortController = null
    }
    search.showLoading()
    search.timeout = setTimeout(() => {
      const resp = this.rest.search(term.trim(), searchlimit)
      search.abortController = resp[0]
      const promise = resp[1]
      this.mapResult(promise, 'searchresult', { term, dialogContext: context })
    }, 250)
    if (this.search.types.indexOf('labels') === -1) return
    const labels = this.store.searchUserDefinedLabels(term)
    searchresult.call(this, { result: { currencies: [], labels }, context: { term, dialogContext: context, local: true } })
  }
}

const searchresult = function ({ context, result }) {
  logger.debug('searchresult', result)
  const search = context.dialogContext === 'search' ? this.search : this.menu.search
  if (!search) return
  if (!context.local) search.hideLoading()
  search.setResult(context.term, result, context.local)
}

const login = function (apiKey) {
  this.login.loading(true)
  this.mapResult(this.rest.login(apiKey), 'loginResult')
}

const refreshResult = function ({ result }) {
  logger.debug('refreshResult', result)
  if (result.status === 'success') return this.call('loginResult', { result })
}

const loginResult = function ({ result }) {
  logger.debug('loginResult', result)
  if (result.status === 'success') {
    if (!this.isStart) return this.call('appLoaded')
    if (IS_DEV) { // eslint-disable-line no-undef
      this.call('appLoaded')
      this.app = new Model(this.locale, this.rest, this.stats, this.reportLogger, this.statusbar) // eslint-disable-line new-cap
      return
    }
    import('../app.js').then(app => {
      this.call('appLoaded')
      this.app = new app.default(this.locale, this.rest, this.stats, this.reportLogger, this.statusbar) // eslint-disable-line new-cap
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
    this.layout.hideModal()
  }
}

const fetchError = function ({ context, msg, error }) {
  if (error.message && error.message.startsWith('401')) {
    this.login.loading(false)
    this.login.clear()
    this.login.error('Please fill in your credentials')
    if (this.showLandingpage) {
      this.landingpage.setLogin(this.login)
    } else {
      this.layout.showModal(this.login)
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
        if (error.name === 'AbortError') return
        search.hideLoading()
      }
      if (this.statusbar) this.statusbar.addMsg('error', error)
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

const changeLocale = function (locale) {
  this.mapResult(
    text(`./lang/${locale}.yaml`)
      .then(YAML.parse)
      .then(yaml =>
        text(`./lang/datatables/${locale}.lang`)
          .then(JSON.parse)
          .then(json => ({ locale: yaml, dtLocale: json }))
      ),
    'localeLoaded', locale
  )
}

const localeLoaded = function ({ result, context }) {
  const locale = context
  setLanguagePack(result.locale)
  setDTLanguagePack(result.dtLocale)
  logger.debug('pack', result)
  moment.locale(locale)
  // fix thousands
  numeral.locale(locale)
  const l = numeral.localeData(locale)
  if (l.delimiters.thousands === ' ') l.delimiters.thousands = '.'
  this.locale = locale
  if (this.config) this.config.setLocale(locale)
  if (this.landingpage) this.landingpage.setUpdate(true)
  if (this.layout) this.layout.setUpdate(true)
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
  jumpToApp,
  changeLocale,
  localeLoaded
}
