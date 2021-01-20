import { jsPDF } from 'jspdf'
import Logger from './logger.js'

const logger = Logger.create('PDF') // eslint-disable-line no-unused-vars

const fontName = 'Helvetica'
const pageWidth = 210
const pageMargin = 20
const headingSize = 16
const titleSize = 24
const headingPadding = 5
const textSize = 12
const ptsPerMm = 2.83
const lineHeight = 1.5
const maxLineWidth = pageWidth - 2 * pageMargin
const maxPageHeight = 270 - pageMargin * 2
const positionStart = pageMargin

const oneLineHeight = fontSize => {
  return (fontSize * lineHeight) / ptsPerMm
}

const toLines = function (fontSize, body) {
  return this.doc
    .setFontSize(fontSize)
    .splitTextToSize(body, maxLineWidth)
}

export default class PDFGenerator {
  constructor () {
    this.position = positionStart
    this.headingCount = 1
    this.doc = new jsPDF()
  }

  checkNewPage (addition) {
    logger.debug('addition', addition)
    if (this.position + addition > maxPageHeight) {
      logger.debug('NEW PAGE')
      this.position = positionStart
      this.doc.addPage()
    }
  }

  titlepage (title, author, institution, timestamp) {
    this.doc.setFont(fontName, 'bold')
    this.doc.setFontSize(headingSize)
    this.position = maxPageHeight / 2.3
    this.doc.text('INVESTIGATION REPORT', pageMargin, this.position)
    this.position += oneLineHeight(headingSize) + headingPadding
    const lines = toLines.call(this, titleSize, title)
    logger.debug('title', title, lines)
    this.doc.setFontSize(titleSize)
    this.doc.text(lines, pageMargin, this.position)
    this.doc.setFontSize(textSize)
    this.position += titleSize
    const line = author + (institution ? ', ' + institution : '')
    this.doc.text(line, pageMargin, this.position)
    this.position += titleSize
    this.doc.text(timestamp || '', pageMargin, this.position)
    this.doc.addPage()
    this.position = positionStart
  }

  heading (body) {
    body = this.headingCount + ' ' + body
    const lines = toLines.call(this, headingSize, body)
    this.doc.setFont(fontName, 'bold')
    this.doc.setFontSize(headingSize)
    const addition = oneLineHeight(headingSize) * lines.length + headingPadding
    this.checkNewPage(addition)
    if (this.position > positionStart) {
      this.position += headingPadding
    }
    this.doc.text(lines, pageMargin, this.position)
    this.position += addition
    this.headingCount++
  }

  paragraph (body, options = { style: 'normal', margin: 0 }) {
    const lines = toLines.call(this, textSize, body)
    logger.debug('body', body, lines)
    const addition = oneLineHeight(textSize) * lines.length
    this.checkNewPage(addition)
    this.doc.setFont(fontName, options.style || 'normal')
    this.doc.setFontSize(textSize)
    this.doc.text(lines, pageMargin + (options.margin || 0), this.position)
    this.position += addition
  }

  blob () {
    return this.doc.output('blob')
  }
}
