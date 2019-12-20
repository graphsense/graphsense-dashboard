import moment from 'moment'
import numeral from 'numeral'
import {currencies} from '../globals.js'
import statistics from './statistics.hbs'

function statsHtml (body) {
  const imageContext = require.context('../style/img/currencies/', false)
  let stats = []
  Object.keys(body).forEach((keyspace) => {
    let s = body[keyspace]
    if (!s) return
    let format = '0,000,000'
    let t = moment.unix(s.timestamp)
    let flat =
        { lastUpdate: (t.format('L') + ' ' + t.format('LT')).replace(/ /g, '&nbsp;'),
          latestBlock: s.no_blocks - 1,
          no_addresses: numeral(s.no_addresses).format(format),
          no_entities: numeral(s.no_entities).format(format),
          no_txs: numeral(s.no_txs).format(format),
          no_labels: numeral(s.no_labels).format(format),
          currency: currencies[keyspace]
        }
    try {
      flat.imageUrl = imageContext(`./${keyspace}.svg`)
    } catch (e) {
      console.error(e.message)
    }
    stats.push(flat)
  })
  return statistics({stats})
}

export {statsHtml}
