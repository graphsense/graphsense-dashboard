import { t } from '../lang.js'
import TagsTable from './tags_table.js'
import downloadTagpack from '../icons/downloadTagpack.html'
import downloadTags from '../icons/downloadTags.html'

export default class MyTagsTable extends TagsTable {
  constructor (dispatcher, index, total, data, nodeType, nodeIsInGraph, supportedKeyspaces, categories) {
    super(dispatcher, index, total, data, null, nodeType, nodeIsInGraph, supportedKeyspaces, categories)
    this.data = data || []
    this.nodeType = nodeType
    this.supportedKeyspaces = supportedKeyspaces
    this.categories = categories
    this.columns = [
      {
        name: t(nodeType.charAt(0).toUpperCase() + nodeType.slice(1)),
        data: nodeType,
        render: (value, type, row) => {
          return this.formatActive(row, this.formatIsInGraph(nodeIsInGraph, nodeType, row.keyspace)(value, type))
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
        name: t('Category'),
        data: 'category',
        defaultContent: '',
        render: (id, type, row) => {
          const v = this.categories[id]
          return this.formatActive(
            row, v ? this.formatLink(v.uri, v.label, v.description) : id)
        }
      },
      {
        name: t('Abuse'),
        data: 'abuse',
        defaultContent: '',
        render: (id, type, row) => {
          const v = this.categories[id]
          return this.formatActive(
            row, v ? this.formatLink(v.uri, v.label, v.description) : id)
        }
      },
      {
        name: t('Last modified'),
        data: 'lastmod',
        render: (value, type, row) => this.formatActive(row, this.formatTimestamp(value, type, row))
      }
    ]
    this.order = [[6, 'desc']]
    this.options = []
    this.addOption({ html: downloadTags, optionText: t('Download tags as JSON'), message: 'downloadTagsAsJSON' })
    this.addOption({ html: downloadTagpack, optionText: t('Download tags as TagPack'), message: 'exportYAML' })
  }
}
