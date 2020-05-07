import moment from 'moment'
import numeral from 'numeral'
import { currencies } from '../globals.js'
import statistics from './statistics.html'
import currency from './currency.html'
import { t } from '../lang.js'
import { nbsp } from '../utils.js'
import { replace } from '../template_utils.js'

function statsHtml (body) {
  let stats = ''
  const imageContext = require.context('../style/img/currencies/', false)
  Object.keys(body).forEach((keyspace) => {
    const s = body[keyspace]
    if (!s) return
    const format = '0,000,000'
    const time = moment.unix(s.timestamp)
    const flat =
        {
          keyspace: keyspace,
          lastUpdate: (time.format('L') + ' ' + time.format('LT')).replace(/ /g, '&nbsp;'),
          latestBlock: s.no_blocks - 1,
          no_addresses: numeral(s.no_addresses).format(format),
          no_entities: numeral(s.no_entities).format(format),
          no_txs: numeral(s.no_txs).format(format),
          no_labels: numeral(s.no_labels).format(format),
          currency: currencies[keyspace],
          t_lastUpdate: nbsp(t('Last update')),
          t_latestBlock: nbsp(t('Latest block')),
          t_transactions: nbsp(t('Transactions')),
          t_addresses: nbsp(t('Addresses')),
          t_entities: nbsp(t('Entities')),
          t_tags: nbsp(t('Tags'))
        }
    try {
      flat.imageUrl = imageContext(`./${keyspace}.svg`)
    } catch (e) {
      console.error(e.message)
    }
    stats += replace(currency, flat)
  })
  return replace(statistics, { stats, supported_currencies: t('Supported currencies') })
}

export { statsHtml }
