import PDFGenerator from '../src/pdf.js'
import FileSaver from 'file-saver'
import Logger from '../src/logger.js'

const logger = Logger.create('PDF') // eslint-disable-line no-unused-vars

const json =
  {
    recordings:
    [{
      processing_steps:
        [{
          visible_data: 'load address 1Archive1n2C579dMsAu3iC6tWzuQJz8dN of keyspace btc from URL',
          timestamp: '2021-01-20T11:14:27+01:00'
        },
        {
          visible_data: 'load address 1Archive1n2C579dMsAu3iC6tWzuQJz8dN of keyspace btc from URL',
          timestamp: '2021-01-20T11:14:27+01:00'
        }
        ]
    }
    ]

  }

for (let i = 0; i < 40; i++) {
  json.recordings[0].processing_steps.push({
    visible_data: 'load address 1Archive1n2C579dMsAu3iC6tWzuQJz8dN of keyspace btc from URL  klasjdf lkajsd lfkja sldk',
    timestamp: '2021-01-20T11:14:27+01:00'
  })
}

const doc = new PDFGenerator()
doc.titlepage('Investigation of case xy on the matter of something really criminal', 'John Doe', 'Police', '2019')
doc.heading('Summary')
doc.paragraph('This is a summary of the Investigation conducted.')
doc.heading('Data sources')
doc.bulletpoint('data source', 'x y z')
doc.bulletpoint('a long name of a data source that has a very long name and never ends', 'data source data that has a very long name and never ends')
doc.heading('Recordings')
json.recordings[0].processing_steps.forEach(step => {
  doc.paragraph(step.timestamp, { style: 'bold' })
  doc.paragraph(step.visible_data, { margin: 10 })
})
let blob = doc.blob()

const filename = 'test.pdf'

blob = new Blob([blob], { type: 'application/octet-stream' }) // eslint-disable-line no-undef
logger.debug('saving to file', filename)
FileSaver.saveAs(blob, filename)
