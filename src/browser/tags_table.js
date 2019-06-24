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
          if (row['source'].startsWith('http')) {
            return `<a href="${row['source']}" target=_blank>${value}</a>`
          }
          return value
        }
      },
      { name: 'Actor Category',
        data: 'actorCategory'
      },
      { name: 'Last modified',
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
