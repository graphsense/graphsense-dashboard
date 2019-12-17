import Table from './table.js'
import {maxAddableNodes} from '../globals.js'

export default class NeighborsTable extends Table {
  constructor (dispatcher, index, total, id, type, isOutgoing, currency, keyspace, nodeIsInGraph) {
    super(dispatcher, index, total, currency, keyspace)
    this.isOutgoing = isOutgoing
    this.columns = [
      { name: (isOutgoing ? 'Outgoing ' : 'Incoming ') + type,
        data: 'id',
        render: this.formatIsInGraph(nodeIsInGraph, type, keyspace)
      },
      { name: 'Balance',
        data: 'balance',
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true))(value[this.currency], type)
      },
      { name: 'Received',
        data: 'received',
        className: 'text-right',
        render: (value, type) =>
          this.formatValue(value => this.formatCurrency(value, keyspace, true))(value[this.currency], type)
      },
      { name: 'No. Tx',
        data: 'no_txs'
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
