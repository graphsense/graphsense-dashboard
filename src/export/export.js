import reportHTML from './report.html'
import tagpackHTML from './tagpack.html'
import Component from '../component.js'
import Logger from '../logger.js'

const logger = Logger.create('Export') // eslint-disable-line no-unused-vars

export default class Export extends Component {
  constructor (dispatcher, meta, type) {
    super()
    this.dispatcher = dispatcher
    this.meta = { ...meta }
    this.type = type
  }

  render (root) {
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (!this.shouldUpdate()) return this.root
    if (this.type === 'report') {
      this.root.innerHTML = reportHTML
    } else if (this.type === 'tagpack') {
      this.root.innerHTML = tagpackHTML
    } else {
      return this.root
    }
    for (const key in this.meta) {
      const el = this.root.querySelector(`.${key}`)
      el.value = this.meta[key]
      el.addEventListener('input', (e) => {
        const obj = {}
        obj[key] = e.target.value
        this.dispatcher('inputMetaData', obj)
      })
    }
    this.root.querySelectorAll('input[data-msg]').forEach(input => {
      const msg = input.getAttribute('data-msg')
      input.addEventListener('click', (e) => {
        this.dispatcher(msg)
      })
    })
    this.root.querySelector('#abort').addEventListener('click', (e) => {
      this.dispatcher('abortExport')
    })
    return this.root
  }
}
