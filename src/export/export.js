import exportHTML from './export.html'
import Component from '../component.js'
import Logger from '../logger.js'

const logger = Logger.create('Export') // eslint-disable-line no-unused-vars

export default class Export extends Component {
  constructor (dispatcher, meta) {
    super()
    this.dispatcher = dispatcher
    this.meta = { ...meta }
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (!this.shouldUpdate()) return this.root
    this.root.innerHTML = exportHTML
    let el = this.root.querySelector('.investigation')
    el.value = this.meta.investigation
    el.addEventListener('input', (e) => {
      this.dispatcher('inputMetaData', { investigation: e.target.value })
    })
    el = this.root.querySelector('.investigator')
    el.value = this.meta.investigator
    el.addEventListener('input', (e) => {
      this.dispatcher('inputMetaData', { investigator: e.target.value })
    })
    el = this.root.querySelector('.institution')
    el.value = this.meta.institution
    el.addEventListener('input', (e) => {
      this.dispatcher('inputMetaData', { institution: e.target.value })
    })
    el = this.root.querySelector('.summary')
    el.value = this.meta.summary
    el.addEventListener('input', (e) => {
      this.dispatcher('inputMetaData', { summary: e.target.value })
    })
    this.root.querySelector('.exportPDF').addEventListener('click', (e) => {
      this.dispatcher('saveReport')
    })
    this.root.querySelector('.exportJSON').addEventListener('click', (e) => {
      this.dispatcher('saveReportJSON')
    })
  }
}
