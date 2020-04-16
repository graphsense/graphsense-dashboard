import { t, tt } from '../lang.js'
import link from './link.html'
import { replace } from '../template_utils'
import BrowserComponent from './component.js'
import Logger from '../logger.js'
import { maxTransactionListSize } from '../globals.js'

const logger = Logger.create('Link') // eslint-disable-line no-unused-vars

export default class Link extends BrowserComponent {
  constructor (dispatcher, data, index, currency) {
    super(dispatcher, index, currency)
    this.data = data
    this.template = link
    this.options =
      [
        { icon: 'exchange-alt', optionText: 'Transactions', message: 'initLinkTransactionsTable' }
      ]
    if (this.data.type !== 'address') {
      this.options = []
    }
  }

  render (root) {
    logger.debug('render')
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    super.render()
    const flat = {
      ...this.data,
      note: this.data.no_txs > maxTransactionListSize ? `(${t('show at most in links table', maxTransactionListSize)})` : '',
      estimated_value: this.formatCurrency(this.data.estimated_value[this.currency], this.data.keyspace)
    }
    this.root.innerHTML = replace(tt(this.template), flat)
    return this.root
  }
}
