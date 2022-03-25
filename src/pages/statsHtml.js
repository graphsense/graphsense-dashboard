import moment from 'moment'
import numeral from 'numeral'
import { currencies } from '../globals.js'
import statistics from './statistics.html'
import currency from './currency.html'
import entities from './entities.html'
import { t } from '../lang.js'
import { nbsp } from '../utils.js'
import { replace } from '../template_utils.js'

function statsHtml (body) {
  let stats = ''
  const imageContext = require.context('../style/img/currencies/', false)
  body.forEach((s) => {
    const format = '0,000,000'
    const time = moment.unix(s.timestamp)
    const keyspace = s.name
    const flat =
        {
          keyspace,
          lastUpdate: (time.format('L') + ' ' + time.format('LT')).replace(/ /g, '&nbsp;'),
          latestBlock: s.no_blocks - 1,
          no_addresses: numeral(s.no_addresses).format(format),
          no_txs: numeral(s.no_txs).format(format),
          no_labels: numeral(s.no_labels).format(format),
          no_tagged_addresses: numeral(s.no_tagged_addresses).format(format),
          tag_coverage: numeral(s.no_tagged_addresses / s.no_addresses).format('0.0%'),
          currency: currencies[keyspace],
          t_lastUpdate: nbsp(t('Last update')),
          t_latestBlock: nbsp(t('Latest block')),
          t_transactions: nbsp(t('Transactions')),
          t_addresses: nbsp(t('Addresses')),
          t_tagged_addresses: nbsp(t('Tagged addresses')),
          t_labels: nbsp(t('Labels')),
          entitiesPart: ''
        }
    if (keyspace !== 'eth') {
      flat.entitiesPart =
        replace(entities, {
          t_entities: nbsp(t('Entities')),
          no_entities: numeral(s.no_entities).format(format)
        })
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
