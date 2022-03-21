import { t } from '../lang.js'
import Table from './table.js'
import $ from 'jquery'

export default class TagsTable extends Table {
  constructor (dispatcher, index, total, data, nodeId, nodeType, currency, keyspace, nodeIsInGraph, supportedKeyspaces, categories, colors, level, entityTag) {
    super(dispatcher, index, total, currency, keyspace, colors)
    this.nodeId = nodeId
    this.data = data || []
    this.supportedKeyspaces = supportedKeyspaces
    this.nodeType = nodeType
    this.categories = categories
    this.entityTag = entityTag
    this.dom = 'Bft'
    this.total = total
    this.realTotal = -1 // unknown real total
    this.level = nodeType === 'address' ? 'address' : (level || nodeType)
    this.columns = [
      {
        name: t(this.level.charAt(0).toUpperCase() + this.level.slice(1)),
        data: this.level,
        render: (value, type, row) => {
          return this.formatActive(row, this.formatIsInGraph(nodeIsInGraph, this.level, keyspace)(value, type))
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
    this.resultField = this.level + '_tags'
    this.loadParams = [this.nodeId, this.nodeType, this.level]
    this.addOption(this.downloadOption(t('Tags file', t(this.level), t(nodeType), nodeId) + ` (${keyspace ? keyspace.toUpperCase() : ''})`))
    if (nodeType === 'label') this.options = []
    this.rowCallback = (row, data) => {
      if (this.entityTag && data.address === this.entityTag.address && data.category) {
        const color = this.colors[data.category]
        $('td', row).css('background-color', color)
      }
    }
  }

  isSmall () {
    return false
  }

  getParams () {
    return {
      id: this.loadParams[0],
      type: this.loadParams[1],
      keyspace: this.keyspace,
      level: this.loadParams[2]
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

  serverSide () {
    return true
  }
}
