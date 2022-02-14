import { t } from '../lang.js'
import Table from './table.js'
import { maxAddableNodes } from '../globals.js'
import { getValueByCurrencyCode } from '../utils.js'

export default class NeighborsTable extends Table {
  constructor (dispatcher, index, total, id, type, isOutgoing, currency, keyspace, nodeIsInGraph) {
    super(dispatcher, index, total, currency, keyspace)
    this.isOutgoing = isOutgoing
    const io = (isOutgoing ? 'Outgoing' : 'Incoming')
    this.columns = [
      {
        name: t(`${io} ${type}`),
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
        name: t('address/entity balance', t(type)),
        data: row => this.getValueByCurrencyCode(row.balance),
        className: 'text-right',
        render: (value, type) =>
          this.formatCurrencyInTable(type, value, keyspace, true)
      },
      {
        name: t('address/entity received', t(type)),
        data: row => this.getValueByCurrencyCode(row.received),
        className: 'text-right',
        render: (value, type) =>
          this.formatCurrencyInTable(type, value, keyspace, true)
      },
      {
        name: t('No. transactions'),
        data: 'no_txs',
        className: 'text-right'
      },
      {
        name: t('Estimated value'),
        data: row => this.getValueByCurrencyCode(row.value),
        className: 'text-right',
        render: (value, type) =>
          this.formatCurrencyInTable(type, value, keyspace, true)
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
    this.addOption(this.downloadOption(t('Neighbors file', `${io} neighbors`, t(type), id) + ` (${keyspace.toUpperCase()})`))
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
