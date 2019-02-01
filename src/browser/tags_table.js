import Table from './table.js'

export default class TagsTable extends Table {
  constructor (dispatcher, index, data, nodeId, nodeType, keyspace) {
    super(dispatcher, index, data.length, keyspace)
    this.nodeId = nodeId
    this.data = data || []
    this.nodeType = nodeType
    this.columns = [
      { name: 'Tag',
        data: 'tag',
        render: (value, type, row) => {
          return `<a href="${row['tagUri']}" target=_blank>${value}</a>`
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
  }
  isSmall () {
    return true
  }
}
