import Table from './table.js'

export default class TagsTable extends Table {
  constructor (dispatcher, index, data, nodeId, nodeType, currency, keyspace) {
    super(dispatcher, index, data.length, currency, keyspace)
    this.nodeId = nodeId
    this.data = data || []
    this.nodeType = nodeType
    this.columns = [
      { name: 'Tag',
        data: 'tag',
        render: (value, type, row) => {
          if (row['tagUri'].startsWith('http')) {
            return `<a href="${row['tagUri']}" target=_blank>${value}</a>`
          }
          return value
        }
      },
      { name: 'Description',
        data: 'description',
        render: (value) => {
          return this.truncateValue(value)
        }
      },
      { name: 'Actor Category',
        data: 'actorCategory'
      },
      { name: 'Source',
        data: 'source',
        render: (value) => {
          return this.truncateValue(value)
        }
      },
      { name: 'Timestamp',
        data: 'timestamp',
        render: this.formatTimestamp
      }
    ]
    this.loadMessage = 'loadTags'
    this.resultField = 'tags'
    this.loadParams = [this.nodeId, this.nodeType]
    this.options =
      [
        this.downloadOption()
      ]
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
