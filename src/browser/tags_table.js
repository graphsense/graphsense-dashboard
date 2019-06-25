import Table from './table.js'

export default class TagsTable extends Table {
  constructor (dispatcher, index, total, data, nodeId, nodeType, currency, keyspace) {
    super(dispatcher, index, total, currency, keyspace)
    this.nodeId = nodeId
    this.data = data || []
    this.nodeType = nodeType
    this.columns = [
      { name: 'Address',
        data: 'address'
      },
      { name: 'Label',
        data: 'label'
      },
      { name: 'Currency',
        data: 'currency'
      },
      { name: 'Source',
        data: 'source',
        render: (value) => this.formatLink(value)
      },
      { name: 'TagPack',
        data: 'tagpack_uri',
        render: (value) => this.formatLink(value)
      },
      { name: 'Category',
        data: 'category'
      },
      { name: 'Last modified',
        data: 'lastmod',
        render: this.formatTimestamp
      }
    ]
    this.loadMessage = 'loadTags'
    this.selectMessage = 'clickAddress'
    this.resultField = null
    this.loadParams = [this.nodeId, this.nodeType]
    this.options =
      [
        this.downloadOption()
      ]
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
}
