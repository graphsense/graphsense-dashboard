import { Elm } from './Main.elm'
import FileSaver from 'file-saver'
import { pack, unpack } from 'lzwcompress'
import { Base64 } from 'js-base64'
import { fileDialog } from 'file-select-dialog'
import plugins from '../generated/plugins/index.js'

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

const pluginFlags = {}

for (const plugin in plugins) {
  pluginFlags[plugin] = plugins[plugin].flags()
}

const app = Elm.Main.init(
  { flags: 
    { localStorage: {...localStorage}
    , width
    , height
    , now
    , pluginFlags 
    } 
  })

let isDirty = false

window.onbeforeunload = function (evt) {
  if (!isDirty) return
  const message = 'You are about to leave the site. Your work will be lost. Sure?'
  if (typeof evt === 'undefined') {
    evt = window.event
  }
  if (evt) {
    evt.returnValue = message
  }
  return message
}

app.ports.console.subscribe(console.error)

app.ports.exportGraphPNG.subscribe((filename) => {
    let svg = document.querySelector('svg#graph')
    let canvas = document.createElement("canvas");
    const svgData = new XMLSerializer().serializeToString(svg)
    const svgDataBase64 = btoa(unescape(encodeURIComponent(svgData)))

    var width = svg.innerWidth
    || window.innerWidth
    || document.documentElement.clientWidth
    || document.body.clientWidth;

    var height = svg.innerHeight 
    || window.innerHeight
    || document.documentElement.clientHeight
    || document.body.clientHeight;

    canvas.width = width; // Set the canvas width
    canvas.height = height; // Set the canvas height
    let img = new Image();
    img.onload = function () {
      let ctx = canvas.getContext("2d");
      ctx.drawImage(img, 0, 0);
      canvas.toBlob((blob) => download(filename, blob))
    };
    img.src = "data:image/svg+xml;base64," + svgDataBase64;
 }
)

app.ports.exportGraphics.subscribe((filename) => {
  const classMap = new Map()
  const sheets = ([...document.styleSheets]).filter(({ href }) => !href)
  if (!sheets) return
  for (let i = 0; i < sheets.length; i++) {
    try {
      const rules = sheets[i].cssRules
      for (let j = 0; j < rules.length; j++) {
        const selectorText = rules[j].selectorText
        const cssText = rules[j].cssText
        if (!selectorText) continue
        const s = selectorText.replace('.', '').trim()
        classMap.set(s, cssText.split('{')[1].replace('}', ''))
      }
    } catch (e) {
      if (!(e instanceof DOMException)) {
        throw e
      }
    }
  }
  classMap.set('rectLabel', 'fill: white')
  let svg = document.querySelector('svg#graph').outerHTML
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
  svg = svg.replace('<svg', '<?xml version="1.0" encoding="UTF-8" standalone="no"?><svg xmlns="http://www.w3.org/2000/svg"')
  download(filename, svg)
})

const download = (filename, buffer) => {
  const blob = new Blob([buffer], { type: 'application/octet-stream' }) // eslint-disable-line no-undef
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

app.ports.serialize.subscribe(([filename, body]) => {
  console.log(body)
  download(filename, compress(body))
})

app.ports.pluginsOut.subscribe(packetWithKey => {
  if (!packetWithKey.length || packetWithKey.length !== 2) {
    console.error('invalid plugin packet', packetWithKey)
    return
  }
  const key = packetWithKey[0]
  const packet = packetWithKey[1]
  if (!plugins[key]) {
    console.error(`plugin ${key} not found`)
    return
  }
  plugins[key].sendPacket(packet, value => {
    app.ports.pluginsIn.send([key, value])
  })
})

class CopyIcon extends HTMLElement {
  constructor () {
    super()
    this.addEventListener('click', (ev) => {
      ev.stopPropagation()
      navigator.clipboard.writeText(this.getAttribute('data-value'))
    })
  }
}

if(!customElements.get('copy-icon')) {
  customElements.define('copy-icon', CopyIcon) 
}

app.ports.newTab.subscribe( url => window.open(url, '_blank'));

app.ports.setDirty.subscribe(dirty => {
  isDirty = dirty
});

app.ports.saveToLocalStorage.subscribe(data => {
  for(let k in data) {
    localStorage.setItem(k, data[k]);
  }
});
