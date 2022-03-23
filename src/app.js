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
import ReportLogger from './reportLogger.js'
import { v4 as uuidv4 } from 'uuid'
import notes from './notes.json'

const logger = Logger.create('Model') // eslint-disable-line no-unused-vars

const baseUrl = REST_ENDPOINT // eslint-disable-line no-undef

const defaultLabelType =
      {
        entityLabel: 'category',
        addressLabel: 'tag'
      }

const defaultCurrency = 'value'

const defaultTxLabel = 'value'

const allowedUrlTypes = ['address', 'entity', 'transaction', 'block', 'label', 'addresslink', 'entitylink']

const fromURL = (url, keyspaces) => {
  const hash = url.split('#!')[1]
  if (!hash) return { id: '', type: '', keyspace: '' } // go home
  const split = hash.split('/')
  let id = split[2]
  let type = split[1]
  let keyspace = split[0]
  let target = null
  if (split[0] === 'label') {
    keyspace = null
    type = split[0]
    id = split[1]
  } else if (split[1].substr(-4, 4) === 'link') {
    target = split[3]
    type = split[1].substr(0, split[1].length - 4)
  } else if (keyspaces.indexOf(keyspace) === -1) {
    logger.warn(`invalid keyspace ${keyspace}?`)
  }
  if (allowedUrlTypes.indexOf(type) === -1) {
    logger.error(`invalid type ${type}`)
    return
  }
  return { keyspace, id, type, target }
}

const shiftKey = 16

export default class Model extends Callable {
  constructor (locale, rest, stats, reportLogger, statusbar) {
    super()
    this.locale = locale
    this.isReplaying = false
    this.showLandingpage = true
    this.stats = stats || { currencies: [] }
    this.reportLogger = reportLogger || new ReportLogger()
    this.keyspaces = (this.stats.currencies || []).map(c => c.name)
    logger.debug('keyspaces', this.keyspaces)
    this.snapshotTimeout = null

    this.rest = rest || new Rest(baseUrl, prefixLength)
    this.statusbar = statusbar || new Statusbar(this.call, this.rest)
    this.debouncing = {}
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
    window.onbeforeunload = function (evt) {
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
    }
    window.onkeydown = (e) => {
      if (e.keyCode !== shiftKey) return
      logger.debug('keydown', e)
      this.call('pressShift')
    }
    window.onkeyup = (e) => {
      if (e.keyCode !== shiftKey) return
      logger.debug('keyup', e)
      this.call('releaseShift')
    }
    this.shiftPressed = false
    const initParams = fromURL(window.location.href, this.keyspaces)
    if (initParams && initParams.id) {
      this.paramsToCall(initParams)
    }
    if (!stats) this.call('stats')
    this.call('changeLocale', locale)
    this.meta =
      {
        investigation: '',
        investigator: '',
        institution: '',
        summary: ''
      }
  }

  loadTaxonomies () {
    this.mapResult(text('./config/conceptsColors.yaml').then(YAML.parse), 'receiveConceptsColors')
    this.mapResult(this.rest.taxonomies(), 'receiveTaxonomies')
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

  paramsToCall ({ id, type, keyspace, target }) {
    this.reportLogger.log('__fromURL', { id, type, keyspace, target })
    appactions.clickSearchResult.call(this, { id, type, keyspace })
    if (target) {
      appactions.clickSearchResult.call(this, { id: target, type, keyspace })
    }
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
    this.layout = new Layout(this.call, this.browser, this.graph, this.config, this.menu, this.search, this.statusbar, defaultCurrency)
    this.layout.disableButton('undo', !this.graph.thereAreMorePreviousSnapshots())
    this.layout.disableButton('redo', !this.graph.thereAreMoreNextSnapshots())
    this.landingpage = new Landingpage(this.call, this.keyspaces)
    this.landingpage.setStats(this.stats.currencies)
    this.landingpage.setSearch(this.search)
    this.loadTaxonomies()
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
    let v = VERSION.split('-')[0] // eslint-disable-line no-undef
    v = v.split(' ')[0]
    return this.compress([
      v,
      this.store.serialize(),
      this.graph.serialize(),
      this.config.serialize(),
      this.layout.serialize()
    ])
  }

  generateTagpack (nodeType) {
    let tags = this.store.getUserDefinedTags2()
    if (nodeType) {
      tags = tags.filter(t => t[nodeType])
    }

    tags.forEach(tag => { tag.lastmod = moment.unix(tag.lastmod).format('YYYY-MM-DD HH:mm:ss') })

    const sets = {}

    tags.forEach(tag => {
      for (const key in tag) {
        if (key === 'isUserDefined' || key === 'active' || key === 'keyspace' || key === 'entity' || key === 'tagpack_uri') {
          delete tag[key]
          continue
        }
        if (!tag.abuse) {
          delete tag.abuse
        }
        if (!tag.category) {
          delete tag.category
        }
        if (sets[key] === undefined) {
          sets[key] = new Set()
        }
        sets[key].add(tag[key])
      }
    })

    const yaml = {
      title: 'Tagpack exported from GraphSense ' + VERSION, // eslint-disable-line no-undef
      creator: this.meta.creator
    }

    for (const key in sets) {
      if (key !== 'entity' && key !== 'address' && sets[key].size === 1) {
        yaml[key] = sets[key].values().next().value
        tags.forEach(tag => { delete tag[key] })
      }
    }

    yaml.tags = tags

    return YAML.stringify(yaml)
  }

  generateReportPDF () {
    return import('./pdf.js').then((PDFGenerator) => {
      const json = this.generateReportJSON()
      PDFGenerator = PDFGenerator.default
      const doc = new PDFGenerator()
      doc.titlepage(json.visible_name, json.user, json.institution, json.timestamp)
      doc.heading('Summary')
      doc.paragraph(json.summary)
      doc.heading('Data sources')
      doc.paragraph('The following data sources were used in the investigation:\n')
      json.data_sources.forEach(ds => {
        if (!ds) ds = { version: null }
        if (!ds.version) ds.version = { nr: null, timestamp: null }
        if (!ds.visible_name) return
        doc.bulletpoint(ds.visible_name, `version: ${ds.version.nr || ''}; time: ${ds.version.timestamp || ''}`)
      })
      doc.heading('Tools used')
      doc.paragraph('This section lists tools that were used in the context of this investigation:\n')
      json.tools.forEach(tool => {
        doc.bulletpoint(tool.visible_name, `version: ${tool.version || ''}`)
      })
      doc.heading('Processing steps taken in the investigation')
      json.recordings[0].processing_steps.forEach(step => {
        doc.paragraph(step.timestamp, { style: 'bold' })
        doc.paragraph(step.visible_data, { margin: 10 })
      })
      doc.heading('Notes')
      json.notes.forEach((note) => {
        if (!note) return
        if (!note.note) return
        doc.paragraph(note.note)
      })
      return doc.blob()
    })
  }

  generateReportJSON () {
    const keyspaces = new Set()
    this.store.entities.each(entity => {
      keyspaces.add(entity.keyspace)
    })
    const time = moment().format('YYYY-MM-DD HH:mm:ss')
    const uuid = uuidv4()
    const report = {
      visible_name: this.meta.investigation || 'Investigation',
      timestamp: time,
      user: this.meta.investigator || 'Unknown Investigator',
      uuid: uuid,
      institution: this.meta.institution || 'Unknown Institution',
      summary: this.meta.summary || 'No summary provided',
      output: []
    }
    report.data_sources = [{
      visible_name: baseUrl,
      version: { nr: VERSION, timestamp: moment().format() } // eslint-disable-line no-undef
    }]
    report.tools = [{
      visible_name: 'GraphSense Dashboard',
      version: VERSION // eslint-disable-line no-undef
    }]
    report.notes = notes

    report.tools.forEach(tool => {
      if (tool.id !== 'ait:graphsense') return
      tool.responsible_for = [uuid]
    })

    report.recordings = [
      {
        label: 'rec1',
        description: 'Recording',
        user: this.meta.investigator,
        timestamp: time,
        processing_steps: this.reportLogger.getLogs()
      }
    ]
    return report
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
    this.store.addTagpack(this.keyspaces, data)
    this.graph.setUpdate('layers')
  }

  deserialize (buffer) {
    const data = this.decompress(buffer)
    this.createComponents()
    data[0] = data[0].split(' ')[0]
    data[0] = data[0].split('-')[0]
    logger.debug('Importing from version', data[0], data[1])
    this.store.deserialize(data[0], data[1])
    this.graph.deserialize(data[0], data[2], this.store)
    this.config.deserialize(data[0], data[3])
    this.layout.deserialize(data[0], data[4])
    this.layout.setUpdate(true)
  }

  download (filename, buffer) {
    var blob = new Blob([buffer], { type: 'application/octet-stream' }) // eslint-disable-line no-undef
    logger.debug('saving to file', filename)
    FileSaver.saveAs(blob, filename)
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    logger.debug('model', this)
    logger.debug('exchange cat', this.menu.categories[15])
    if (this.showLandingpage) {
      return this.landingpage.render(this.root)
    }
    return this.layout.render(this.root)
  }

  updateCategoriesByTags (tags) {
    let cats = new Set()
    let abs = new Set()
    if (tags.address_tags && tags.entity_tags) {
      tags = tags.address_tags.concat(tags.entity_tags)
    }
    tags.forEach(({ category, abuse }) => {
      if (category) cats.add(category)
      if (abuse) abs.add(abuse)
    })
    cats = [...cats]
    abs = [...abs]
    this.store.addCategories(cats)
    this.graph.addCategories(cats)
    this.config.setCategoryColors(this.graph.getCategoryColors(), this.store.getCategories())
  }
}
