import Callable from './callable.js'
import { text } from 'd3-fetch'
import Store from './store.js'
import Login from './login/login.js'
import Search from './search/search.js'
import Browser from './browser.js'
import Rest from './rest.js'
import Layout from './layout.js'
import NodeGraph from './nodeGraph.js'
import Config from './config.js'
import Menu from './menu.js'
import Statusbar from './statusbar.js'
import Landingpage from './landingpage.js'
import moment from 'moment'
import FileSaver from 'file-saver'
import { pack, unpack } from 'lzwcompress'
import { Base64 } from 'js-base64'
import Logger from './logger.js'
import startactions from './actions/start.js'
import appactions from './actions/app.js'
import { prefixLength } from './globals.js'
import YAML from 'yaml'
import { SHA256 } from 'sha2'
import ReportLogger from './reportLogger.js'
import { v4 as uuidv4 } from 'uuid'

const logger = Logger.create('Model') // eslint-disable-line no-unused-vars

const baseUrl = REST_ENDPOINT // eslint-disable-line no-undef

const defaultLabelType =
      {
        entityLabel: 'category',
        addressLabel: 'id'
      }

const defaultCurrency = 'value'

const defaultTxLabel = 'no_txs'

const allowedUrlTypes = ['address', 'entity', 'transaction', 'block', 'label']

const fromURL = (url, keyspaces) => {
  const hash = url.split('#!')[1]
  if (!hash) return { id: '', type: '', keyspace: '' } // go home
  const split = hash.split('/')
  let id = split[2]
  let type = split[1]
  let keyspace = split[0]
  if (split[0] === 'label') {
    keyspace = null
    type = split[0]
    id = split[1]
  } else if (keyspaces.indexOf(keyspace) === -1) {
    logger.warn(`invalid keyspace ${keyspace}?`)
  }
  if (allowedUrlTypes.indexOf(type) === -1) {
    logger.error(`invalid type ${type}`)
    return
  }
  return { keyspace, id, type }
}

export default class Model extends Callable {
  constructor (locale, rest, stats, reportLogger) {
    super()
    this.locale = locale
    this.isReplaying = false
    this.showLandingpage = true
    this.stats = stats || { currencies: {} }
    this.reportLogger = reportLogger || new ReportLogger()
    this.keyspaces = Object.keys(this.stats.currencies)
    logger.debug('keyspaces', this.keyspaces)
    this.snapshotTimeout = null

    this.statusbar = new Statusbar(this.call)
    this.rest = rest || new Rest(baseUrl, prefixLength)
    this.createComponents()
    this.registerDispatchEvents(startactions)
    this.registerDispatchEvents(appactions)

    window.onhashchange = (e) => {
      const params = fromURL(e.newURL, this.keyspaces)
      logger.debug('hashchange', e, params)
      if (!params) return
      this.paramsToCall(params)
    }
    const that = this
    window.addEventListener('beforeunload', function (evt) {
      if (IS_DEV) return // eslint-disable-line no-undef
      if (!that.showLandingpage) {
        const message = 'You are about to leave the site. Your work will be lost. Sure?'
        if (typeof evt === 'undefined') {
          evt = window.event
        }
        if (evt) {
          evt.returnValue = message
        }
        return message
      }
    })
    const initParams = fromURL(window.location.href, this.keyspaces)
    if (initParams && initParams.id) {
      this.paramsToCall(initParams)
    }
    if (!stats) this.call('stats')
    this.loadCategories()
    this.loadAbuses()
  }

  loadCategories () {
    this.mapResult(text('./categoryColors.yaml').then(YAML.parse), 'receiveCategoryColors')
    this.mapResult(this.rest.categories(), 'receiveCategories')
  }

  loadAbuses () {
    this.mapResult(this.rest.abuses(), 'receiveAbuses')
  }

  storeRelations (relations, anchor, keyspace, isOutgoing) {
    relations.forEach((relation) => {
      if (relation.nodeType !== anchor.type) return
      const src = isOutgoing ? relation.id : anchor.id
      const dst = isOutgoing ? anchor.id : relation.id
      this.store.linkOutgoing(src, dst, keyspace, relation)
    })
  }

  promptUnsavedWork (msg) {
    if (!this.isDirty) return true
    return confirm('You have unsaved changes. Do you really want to ' + msg + '?') // eslint-disable-line no-undef
  }

  paramsToCall ({ id, type, keyspace }) {
    this.reportLogger.log('__fromURL', { id, type, keyspace })
    appactions.clickSearchResult.call(this, { id, type, keyspace })
  }

  createComponents () {
    this.isDirty = false
    this.store = new Store()
    this.browser = new Browser(this.call, defaultCurrency, this.keyspaces)
    this.config = new Config(this.call, defaultLabelType, defaultTxLabel, this.locale)
    this.menu = new Menu(this.call, this.keyspaces)
    this.graph = new NodeGraph(this.call, defaultLabelType, defaultCurrency, defaultTxLabel)
    this.browser.setNodeChecker(this.graph.getNodeChecker())
    this.login = new Login(this.call)
    this.search = new Search(this.call, ['addresses', 'transactions', 'labels', 'blocks'], 'search')
    this.search.setStats(this.stats.currencies)
    this.layout = new Layout(this.call, this.browser, this.graph, this.config, this.menu, this.search, this.statusbar, this.login, defaultCurrency)
    this.layout.disableButton('undo', !this.graph.thereAreMorePreviousSnapshots())
    this.layout.disableButton('redo', !this.graph.thereAreMoreNextSnapshots())
    this.landingpage = new Landingpage(this.call, this.keyspaces)
    this.landingpage.setStats(this.stats.currencies)
    this.landingpage.setSearch(this.search)
  }

  compress (data) {
    return new Uint32Array(
      pack(
        // convert to base64 (utf-16 safe)
        Base64.encode(
          JSON.stringify(data)
        )
      )
    ).buffer
  }

  decompress (data) {
    return JSON.parse(
      Base64.decode(
        unpack(
          [...new Uint32Array(data)]
        )
      )
    )
  }

  serialize () {
    return this.compress([
      VERSION, // eslint-disable-line no-undef
      this.store.serialize(),
      this.graph.serialize(),
      this.config.serialize(),
      this.layout.serialize()
    ])
  }

  serializeNotes () {
    return this.compress([
      VERSION, // eslint-disable-line no-undef
      this.store.serializeNotes()
    ])
  }

  generateTagpack () {
    return YAML.stringify({
      title: 'Tagpack exported from GraphSense ' + VERSION, // eslint-disable-line no-undef
      creator: this.rest.username,
      lastmod: moment().format('YYYY-MM-DD'),
      tags: this.store.getUserDefinedTags()
    })
  }

  generateTagsJSON () {
    return JSON.stringify(this.store.allAddressTags().map(this.tagToJSON), null, 2)
  }

  generateReport () {
    const keyspaces = new Set()
    this.store.entities.each(entity => {
      keyspaces.add(entity.keyspace)
    })
    const time = moment().format('YYYY-MM-DD HH-mm-ss')
    const uuid = uuidv4()
    const report = {
      visible_name: 'Investigation',
      timestamp: time,
      user: '',
      uuid: uuid,
      institution: '',
      summary: '',
      output: []
    }
    const concat = (keyspace, key) => {
      report[key] = report[key].concat(this.stats.currencies[keyspace][key])
    }
    report.data_sources = [...this.stats.data_sources]
    report.tools = [...this.stats.tools]
    report.notes = [...this.stats.notes]

    report.tools.forEach(tool => {
      if (tool.id !== 'ait:graphsense') return
      tool.responsible_for = tool.responsible_for || []
      tool.responsible_for = [...tool.responsible_for, uuid]
    })

    keyspaces.forEach(keyspace => {
      concat(keyspace, 'data_sources')
      concat(keyspace, 'tools')
      concat(keyspace, 'notes')
    })
    /*
      report.data_sources.forEach(ds => {
        ds.version = {nr: null, hash: null, timestamp: null, file: 'bla'}
        ds.report_uuid = 'bla'
      })
      */

    report.recordings = [
      {
        label: '',
        description: '',
        user: '',
        timestamp: time,
        processing_steps: this.reportLogger.getLogs()
      }
    ]
    const output = JSON.stringify(report, null, 2)
    return import('jszip').then(jszip => {
      jszip = new jszip.default() // eslint-disable-line new-cap
      jszip.file('report.json', output)
      return jszip.generateAsync({ type: 'blob' })
    }).then(zipfile => {
      const formData = new FormData() // eslint-disable-line no-undef
      formData.append('file', zipfile)
      return fetch(TITANIUM_REPORT_GENERATION_URL + '/generate_report', { // eslint-disable-line no-undef
        method: 'POST',
        body: formData
      }).then(response => response.blob())
    })
  }

  loadTagsJSON (data) {
    try {
      data = JSON.parse(data)
      if (!data) throw new Error('result is empty')
      if (!Array.isArray(data)) data = [data]
      this.store.addTagpack(this.keyspaces, { tags: data.map(this.tagJSONToTagpackTag) })
      this.graph.setUpdate('layers')
    } catch (e) {
      const msg = 'Could not parse JSON file'
      this.statusbar.addMsg('error', msg + ': ' + e.message)
      console.error(msg)
    }
  }

  loadTagpack (yaml) {
    let data
    try {
      data = YAML.parse(yaml)
      if (!data) throw new Error('result is empty')
    } catch (e) {
      const msg = 'Could not parse YAML file'
      this.statusbar.addMsg('error', msg + ': ' + e.message)
      console.error(msg)
      return
    }
    this.store.addNotes(data.tags)
    this.store.addTagpack(this.keyspaces, data)
    this.graph.setUpdate('layers')
  }

  tagToJSON (tag) {
    return {
      uuid: SHA256([tag.address, tag.currency, tag.label, tag.source, tag.tagpack_uri].join(',')).toString('hex'),
      version: 1,
      key_type: 'a',
      key: tag.address,
      tag: tag.label,
      contributor: 'GraphSense',
      tag_optional: {
        actor_type: null,
        currency: tag.currency,
        tag_source_uri: tag.source,
        tag_source_label: null,
        post_date: null,
        post_author: null
      },
      contributor_optional: {
        contact_details: 'contact@graphsense.info',
        insertion_date: moment.unix(tag.lastmod).format(),
        software: 'GraphSense ' + VERSION, // eslint-disable-line no-undef
        collection_type: 'm'
      }
    }
  }

  tagJSONToTagpackTag (tagJSON) {
    return {
      address: tagJSON.key,
      label: tagJSON.tag,
      currency: tagJSON.tag_optional && tagJSON.tag_optional.currency,
      source: tagJSON.tag_optional && tagJSON.tag_optional.tag_source_uri,
      lastmod: tagJSON.contributor_optional && tagJSON.contributor_optional.insertion_date,
      category: null,
      tagpack_uri: null
    }
  }

  deserialize (buffer) {
    const data = this.decompress(buffer)
    this.createComponents()
    this.store.deserialize(data[0], data[1])
    this.graph.deserialize(data[0], data[2], this.store)
    this.config.deserialize(data[0], data[3])
    this.layout.deserialize(data[0], data[4])
    this.layout.setUpdate(true)
  }

  deserializeNotes (buffer) {
    const data = this.decompress(buffer)
    this.store.deserializeNotes(data[0], data[1])
    this.graph.setUpdate('layers')
  }

  download (filename, buffer) {
    var blob = new Blob([buffer], { type: 'application/octet-stream' }) // eslint-disable-line no-undef
    logger.debug('saving to file', filename)
    FileSaver.saveAs(blob, filename)
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (this.showLandingpage) {
      return this.landingpage.render(this.root)
    }
    logger.debug('model render')
    logger.debug('model', this)
    return this.layout.render(this.root)
  }
}
