import link from './link.html'
import { replace } from '../template_utils'
import BrowserComponent from './component.js'
import Logger from '../logger.js'

const logger = Logger.create('Link') // eslint-disable-line no-unused-vars

export default class Link extends BrowserComponent {
  constructor (dispatcher, data, index, currency) {
    super(dispatcher, index, currency)
    this.data = data
    this.template = link
    this.options =
      [
        { icon: 'exchange-alt', optionText: 'Transactions', message: 'initBlockTransactionsTable' }
      ]
  }

  render (root) {
    logger.debug('render')
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    super.render()
    this.root.innerHTML = replace(this.template, this.data)
    return this.root
  }
}
