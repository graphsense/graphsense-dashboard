import Table from './table.js'

export default class TagsTable extends Table {
  constructor (dispatcher, index, data, nodeId, nodeType) {
    super(dispatcher, index, data.length)
    this.nodeId = nodeId
    this.data = data
    this.nodeType = nodeType
    this.columns = [
      { name: 'Tag',
        data: 'tag'
      },
      { name: 'Tag URI',
        data: 'tagUri'
      },
      { name: 'Description',
        data: 'description'
      },
      { name: 'Actor Category',
        data: 'actorCategory'
      },
      { name: 'Source',
        data: 'source'
      },
      { name: 'Timestamp',
        data: 'timestamp'
      }
    ]
    this.loadMessage = 'loadTags'
    this.resultMessage = 'resultTags'
    this.resultField = 'tags'
    this.loadParams = [this.nodeId, this.nodeType]
  }
  isSmall () {
    return true
  }
}
