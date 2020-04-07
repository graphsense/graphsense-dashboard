import Table from './table.js'
import downloadTags from '../icons/downloadTags.html'

export default class TagsTable extends Table {
  constructor (dispatcher, index, total, data, nodeId, nodeType, currency, keyspace, nodeIsInGraph, supportedKeyspaces) {
    super(dispatcher, index, total, currency, keyspace)
    this.nodeId = nodeId
    this.data = data || []
    this.supportedKeyspaces = supportedKeyspaces
    this.nodeType = nodeType
    this.columns = [
      {
        name: 'Address',
        data: 'address',
        render: (value, type, row) => {
          return this.formatActive(row, this.formatIsInGraph(nodeIsInGraph, 'address', keyspace)(value, type))
        }
      },
      {
        name: 'Label',
        data: 'label',
        render: (value, type, row) => this.formatActive(row, value)
      },
      {
        name: 'Currency',
        data: 'currency',
        render: (value, type, row) => this.formatActive(row, value)
      },
      {
        name: 'Source',
        data: 'source',
        render: (value, type, row) => this.formatActive(row, this.formatLink(value))
      },
      {
        name: 'TagPack',
        data: 'tagpack_uri',
        render: (value, type, row) => this.formatActive(row, this.formatLink(value))
      },
      {
        name: 'Category',
        data: 'category',
        render: (value, type, row) => this.formatActive(row, value)
      },
      {
        name: 'Abuse',
        data: 'abuse',
        render: (value, type, row) => this.formatActive(row, value)
      },
      {
        name: 'Last modified',
        data: 'lastmod',
        render: (value, type, row) => this.formatActive(row, this.formatTimestamp(value, type, row))
      }
    ]
    this.loadMessage = 'loadTags'
    this.selectMessage = ['clickAddress', 'clickLabel']
    this.resultField = null
    this.loadParams = [this.nodeId, this.nodeType]
    this.addOption(this.downloadOption())
    this.addOption({ html: downloadTags, optionText: 'Download tags as JSON', message: 'downloadTagsAsJSON' })
    if (nodeType === 'label') this.options = []
  }

  isSmall () {
    return true
  }

  getParams () {
    return {
      id: this.loadParams[0],
      type: this.loadParams[1],
      keyspace: this.keyspace
    }
  }

  formatActive (row, content) {
    if (!this.isActiveRow(row)) {
      return `<span class="unsupported-keyspace">${content || ''}</span>`
    }
    return content
  }

  isActiveRow (row) {
    return row.keyspace && this.supportedKeyspaces.indexOf(row.keyspace) !== -1 && row.active
  }
}
