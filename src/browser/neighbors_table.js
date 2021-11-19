import { t } from '../lang.js'
import Table from './table.js'
import { maxAddableNodes } from '../globals.js'

export default class NeighborsTable extends Table {
  constructor (dispatcher, index, total, id, type, isOutgoing, currency, keyspace, nodeIsInGraph) {
    super(dispatcher, index, total, currency, keyspace)
    this.isOutgoing = isOutgoing
    this.columns = [
      {
        name: t((isOutgoing ? 'Outgoing ' : 'Incoming ') + type),
        data: 'id',
        render: this.formatIsInGraph(nodeIsInGraph, type, keyspace)
      },
      {
        name: t('Labels'),
        data: 'labels',
        render: (value, type) => {
          const maxCount = 30
          let charCount = 0
          let output = ''
          const labels = (value || []).sort()
          for (let i = 0; i < labels.length; i++) {
            const label = labels[i]
            charCount += label.length
            if (i > 0 && charCount > maxCount) {
              output += ` + ${labels.length - i}`
              break
            } else {
              if (i > 0) output += ', '
              output += label.length < maxCount ? label : (label.substr(0, maxCount) + '...')
            }
          }
          return `<span title="${value}">${output}</span>`
        }
      },
      {
        name: t('Final balance'),
        data: row => row.balance.value,
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true), value[keyspace])(value, type)
      },
      {
        name: t('Total received'),
        data: row => row.received.value,
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true), value[keyspace])(value, type)
      },
      {
        name: t('No. transactions'),
        data: 'no_txs'
      },
      {
        // just to enable full search of lables
        name: 'Labels',
        data: 'labels',
        render: (value) => (value || []).join(' '),
        visible: false
      }
    ]
    this.loadMessage = 'loadNeighbors'
    this.resultField = 'neighbors'
    this.selectMessage = 'selectNeighbor'
    this.loadParams = [id, type, isOutgoing]
    this.addOption(this.downloadOption())
    if (total < maxAddableNodes) this.options.push(this.addAllOption())
  }

  getParams () {
    return {
      id: this.loadParams[0],
      type: this.loadParams[1],
      isOutgoing: this.loadParams[2],
      keyspace: this.keyspace
    }
  }
}
