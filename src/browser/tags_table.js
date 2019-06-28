import Table from './table.js'

export default class TagsTable extends Table {
  constructor (dispatcher, index, total, data, nodeId, nodeType, currency, keyspace, nodeIsInGraph, supportedKeyspaces) {
    super(dispatcher, index, total, currency, keyspace)
    this.nodeId = nodeId
    this.data = data || []
    this.supportedKeyspaces = supportedKeyspaces
    this.nodeType = nodeType
    this.columns = [
      { name: 'Address',
        data: 'address',
        render: (value, type, row) => {
          return this.formatSupportedKeyspace(row.keyspace, this.formatIsInGraph(nodeIsInGraph, 'address', keyspace)(value, type))
        }
      },
      { name: 'Label',
        data: 'label',
        render: (value, type, row) => this.formatSupportedKeyspace(row.keyspace, value)
      },
      { name: 'Currency',
        data: 'currency',
        render: (value, type, row) => this.formatSupportedKeyspace(row.keyspace, value)
      },
      { name: 'Source',
        data: 'source',
        render: (value, type, row) => this.formatSupportedKeyspace(row.keyspace, this.formatLink(value))
      },
      { name: 'TagPack',
        data: 'tagpack_uri',
        render: (value, type, row) => this.formatSupportedKeyspace(row.keyspace, this.formatLink(value))
      },
      { name: 'Category',
        data: 'category',
        render: (value, type, row) => this.formatSupportedKeyspace(row.keyspace, value)
      },
      { name: 'Last modified',
        data: 'lastmod',
        render: (value, type, row) => this.formatSupportedKeyspace(row.keyspace, this.formatTimestamp(value, type, row))
      }
    ]
    this.loadMessage = 'loadTags'
    this.selectMessage = ['clickAddress', 'clickLabel']
    this.resultField = null
    this.loadParams = [this.nodeId, this.nodeType]
    this.addOption(this.downloadOption())
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
  formatSupportedKeyspace (keyspace, content) {
    if (!keyspace || this.supportedKeyspaces.indexOf(keyspace) === -1) {
      return `<span class="unsupported-keyspace">${content}</span>`
    }
    return content
  }
}
