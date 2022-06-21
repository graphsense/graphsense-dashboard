import { Elm } from "./Main.elm";
import FileSaver from 'file-saver'
import { pack, unpack } from 'lzwcompress'
import { Base64 } from 'js-base64'
import { fileDialog } from 'file-select-dialog'


const getNavigatorLanguage = () => {
  if (navigator.languages && navigator.languages.length) {
    return navigator.languages[0]
  } else {
    return navigator.userLanguage || navigator.language || navigator.browserLanguage || 'en'
  }
}

const locale = getNavigatorLanguage().split('-')[0]

const docElem = document.documentElement
const body = document.getElementsByTagName('body')[0]
const width = window.innerWidth || docElem.clientWidth || body.clientWidth
const height = window.innerHeight || docElem.clientHeight || body.clientHeight

const now = +(new Date())

const app = Elm.Main.init({flags: {locale, width, height, now}});


app.ports.console.subscribe(console.error)

app.ports.exportGraphics.subscribe((filename) => {
  const classMap = new Map()
  let sheets = ([...document.styleSheets]).filter(({href}) => !href)
  if (!sheets) return
  for (let i = 0; i < sheets.length; i++) {
    console.log(sheets[i])
    const rules = sheets[i].cssRules
    for (let j = 0; j < rules.length; j++) {
      const selectorText = rules[j].selectorText
      const cssText = rules[j].cssText
      if (!selectorText) continue
      const s = selectorText.replace('.', '').trim()
      classMap.set(s, cssText.split('{')[1].replace('}', ''))
    }
  }
  classMap.set('rectLabel', 'fill: white')
  console.log('classMap', classMap)
  let svg = document.querySelector('#graph svg').outerHTML
  // replace classes by inline styles
  svg = svg.replace(new RegExp('class="(.+?)"', 'g'), (_, classes) => {
    const repl = classes.split(' ')
      .map(cls => classMap.get(cls) || '')
      .join('')
    if (repl.trim() === '') return ''
    return 'style="' + repl.replace(/"/g, '\'').replace('"', '\'') + '"'
  })
  // replace double quotes and quot (which was created by innerHTML)
  svg = svg.replace(new RegExp('style="(.+?)"', 'g'), (_, style) => 'style="' + style.replace(/&quot;/g, '\'') + '"')
  // merge double style definitions
  svg = svg.replace(new RegExp('style="([^"]+?)"([^>]+?)style="([^"]+?)"', 'g'), 'style="$1$3" $2')
  console.log('svg', svg)
  download(filename, svg)
})

const download = (filename, buffer) => {
  var blob = new Blob([buffer], { type: 'application/octet-stream' }) // eslint-disable-line no-undef
  console.log('saving', filename)
  FileSaver.saveAs(blob, filename)
}


const compress = (data) => {
    return new Uint32Array(
      pack(
        // convert to base64 (utf-16 safe)
        Base64.encode(
          JSON.stringify(data)
        )
      )
    ).buffer
  }

const decompress = (data) => {
    return JSON.parse(
      Base64.decode(
        unpack(
          [...new Uint32Array(data)]
        )
      )
    )
  }

app.ports.deserialize.subscribe(() => {
    fileDialog({ strict: true })
      .then(file => {
        const reader = new FileReader() // eslint-disable-line no-undef
        reader.onload = () => {
          let data = reader.result
          data = decompress(data)
          data[0] = data[0].split(' ')[0]
          data[0] = data[0].split('-')[0]
          console.log(data)
          app.ports.deserialized.send([file.name, data])
        }
        reader.readAsArrayBuffer(file)

      })
})


