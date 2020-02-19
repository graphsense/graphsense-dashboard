import moment from 'moment'
import Logger from './logger.js'

const logger = Logger.create('ReportLogger') // eslint-disable-line no-unused-vars

const messages = {
  'clickSearchResult': (payload) => 'Auswahl eines Ergebnisses der ' + (payload.isInDialog ? 'Nachbarsuche' : 'Suchleiste')
}

export default class ReportLogger {
  constructor () {
    this.logs = []
  }
  log (eventName, eventData) {
    if (!messages[eventName]) return
    this.logs.push({
      visible_data: messages[eventName](eventData),
      timestamp: moment().format(),
      data: {eventName, eventData}
    })
    logger.debug('logs', this.logs)
  }
  getLogs () {
    return this.logs
  }
}
