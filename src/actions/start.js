import {labelPrefixLength, searchlimit, prefixLength} from '../globals.js'
import Logger from '../logger.js'
const logger = Logger.create('Actions') // eslint-disable-line no-unused-vars

const stats = function () {
  this.mapResult(this.rest.stats(), 'receiveStats')
}

const receiveStats = function ({context, result}) {
  this.keyspaces = Object.keys(result)
  this.landingpage.setStats({...result})
  this.search.setStats({...result})
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
export default {
  stats,
  receiveStats,
  search,
  searchresult,
  searchresultLabels
}
