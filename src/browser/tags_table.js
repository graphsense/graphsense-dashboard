import { t } from '../lang.js'
import Table from './table.js'
import downloadTags from '../icons/downloadTags.html'

export default class TagsTable extends Table {
  constructor (dispatcher, index, total, data, nodeId, nodeType, currency, keyspace, nodeIsInGraph, supportedKeyspaces, categories) {
    super(dispatcher, index, total, currency, keyspace)
    this.nodeId = nodeId
    this.data = data || []
    this.supportedKeyspaces = supportedKeyspaces
    this.nodeType = nodeType
    this.categories = categories
    this.columns = [
      {
        name: t('Address'),
        data: 'address',
        render: (value, type, row) => {
          return this.formatActive(row, this.formatIsInGraph(nodeIsInGraph, 'address', keyspace)(value, type))
        }
      },
      {
        name: t('Label'),
        data: 'label',
        render: (value, type, row) => this.formatActive(row, value)
      },
      {
        name: t('Currency'),
        data: 'currency',
        render: (value, type, row) => this.formatActive(row, value)
      },
      {
        name: t('Source'),
        data: 'source',
        render: (value, type, row) => this.formatActive(row, this.formatLink(value))
      },
      {
        name: t('TagPack'),
        data: 'tagpack_uri',
        render: (value, type, row) => this.formatActive(row, this.formatLink(value))
      },
      {
        name: t('Category'),
        data: 'category',
        defaultContent: '',
        render: (value, type, row) => this.formatActive(row, this.categories[value] ? this.formatLink(this.categories[value], value) : value)
      },
      {
        name: t('Abuse'),
        data: 'abuse',
        defaultContent: '',
        render: (value, type, row) => this.formatActive(row, this.categories[value] ? this.formatLink(this.categories[value], value) : value)
      },
      {
        name: t('Last modified'),
        data: 'lastmod',
        render: (value, type, row) => this.formatActive(row, this.formatTimestamp(value, type, row))
      },
      {
        name: t('Active'),
        data: 'active',
        visible: false
      }
    ]
    this.order = [[8, 'desc']]
    this.loadMessage = 'loadTags'
    this.selectMessage = ['clickAddress', 'clickLabel']
    this.resultField = null
    this.loadParams = [this.nodeId, this.nodeType]
    this.addOption(this.downloadOption())
    this.addOption({ html: downloadTags, optionText: t('Download tags as JSON'), message: 'downloadTagsAsJSON' })
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
