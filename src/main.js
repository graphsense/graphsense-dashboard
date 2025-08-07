import { Elm } from './Main.elm'
import FileSaver from 'file-saver'
import { pack, unpack } from 'lzwcompress'
import { Base64 } from 'js-base64'
import { fileDialog } from 'file-select-dialog'
import plugins from '../generated/plugins/index.js'
import robotoBase64 from "../public/fonts/roboto/fonts/Regular/Roboto-Regular.woff2?raw-base64"
import robotoBoldBase64 from "../public/fonts/roboto/fonts/Bold/Roboto-Bold.woff2?raw-base64"

function measureCharacterDimensions() {
    // Create a temporary canvas element for text measurement
    const canvas = document.createElement('canvas');
    const context = canvas.getContext('2d');

    // Set the font to match your SVG text styling
    // You'll need to adjust this to match your actual font
    context.font = '12px Roboto'; // Adjust size and family as needed

    // Characters to measure (you can expand this list)
    const characters = [
        // Letters
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
        // Numbers
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
        // Common symbols
        ' ', '.', ',', ':', ';', '!', '?', '-', '+', '=', '(', ')', '[', ']', '{', '}', '|', '/', '\\',
        // Currency symbols
        '$', '€', '£', '¥', '¢'
    ];

    const measurements = {};

    characters.forEach(char => {
        const metrics = context.measureText(char);
        const w = Math.round(metrics.width * 10) / 10 // Round to 1 decimal place
        const h = Math.round((metrics.actualBoundingBoxAscent + metrics.actualBoundingBoxDescent) * 10) / 10 // Round to 1 decimal place
        measurements[char] = {width: w, height: h};
    });

    return measurements;
}

const getTheme = () => {
  const rootStyles = window.getComputedStyle(document.documentElement);
  return Object.fromEntries(
    Array.from(document.styleSheets)
      .flatMap((styleSheet) => {
        try {
          return Array.from(styleSheet.cssRules);
        } catch (error) {
          return [];
        }
      })
      .filter((cssRule) => cssRule instanceof CSSStyleRule)
      .flatMap((cssRule) => Array.from(cssRule.style))
      .filter((style) => style.startsWith("--"))
      .map((variable) => [variable, rootStyles.getPropertyValue(variable)]),
  );
};


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

const characterDimensions = measureCharacterDimensions()

for (const plugin in plugins) {
  pluginFlags[plugin] = plugins[plugin].flags()
}

const app = Elm.Main.init(
  { flags: 
    { localStorage: {...localStorage}
    , characterDimensions
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


app.ports.exportGraphImage.subscribe((filename) => {
    let svg = document.querySelector('svg#graph')
    let canvas = document.createElement("canvas");
    var svgData = new XMLSerializer().serializeToString(svg)

    // replace css variables with actual values, since
    // currently css variables are not supported in canvas
    const cssVariables = getTheme()
    for (const [key, value] of Object.entries(cssVariables)) {
      svgData = svgData.replaceAll("var(" + key + ")", value)
    }

    // Looks like loading external files on file rendering
    // eg. link to our fonts does not work
    // thus i embedded the data itself
    // Embed fonts into svg as Base64Encoded string
    let fontStyle = `
      <style>
        @font-face {
          font-family: 'Roboto';
          font-style: normal;
          font-weight: 400;
          src:url(data:application/font-woff;charset=utf-8;base64,${robotoBase64}) format('woff');
        }
        @font-face {
          font-family: 'Roboto';
          font-style: bold;
          font-weight: 600;
          src: url(data:application/font-woff;charset=utf-8;base64,${robotoBoldBase64}) format('woff');
        }
      svg {
        font-family: Roboto;
      }
      </style>
    `
    
    svgData = svgData.replace("<defs>", "<defs>" + fontStyle)
    const svgDataBase64 = btoa(unescape(encodeURIComponent(svgData)))
    
    const bgColor = cssVariables["--c-white"]

    const pixelScaleFactor = 2;
    var width = (svg.innerWidth
    || window.innerWidth
    || document.documentElement.clientWidth
    || document.body.clientWidth) * pixelScaleFactor; 

    var height = (svg.innerHeight 
    || window.innerHeight
    || document.documentElement.clientHeight
    || document.body.clientHeight) * pixelScaleFactor;

    canvas.width = width; // Set the canvas width
    canvas.height = height; // Set the canvas height
    let img = new Image();

    img.onload = function () {
      let ctx = canvas.getContext("2d");
      ctx.fillStyle = bgColor;
      ctx.fillRect(0, 0, canvas.width, canvas.height, 0, 0, canvas.width /2, canvas.height/2 );

      ctx.drawImage(img, 0, 0, width, height);
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
        app.ports.deserialized.send([file.name, data])
      }
      reader.readAsArrayBuffer(file)
    })
})

app.ports.serialize.subscribe(([filename, body]) => {
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
    let label, original
    setTimeout(() => {
      label = this.querySelector('[data-label]')
      original = label?.innerText
    }, 0)
    super()
    this.addEventListener('click', (ev) => {
      ev.stopPropagation()
      navigator.clipboard.writeText(this.getAttribute('data-value'))
      if(!label) return
      label.innerHTML = this.getAttribute('data-copied-label')
      setTimeout(() => {
        label.innerHTML = original
      }, 3000)
    })
    this.addEventListener('mouseover', () => {
      let hint = this.querySelector('[data-hint]');
      if(!hint) return
      hint.style.display = 'flex'
    })
    this.addEventListener('mouseleave', () => {
      let hint = this.querySelector('[data-hint]');
      if(!hint) return
      hint.style.display = 'none'
    })
  }
}

if(!customElements.get('copy-icon')) {
  customElements.define('copy-icon', CopyIcon) 
}

app.ports.newTab.subscribe( url => window.open(url, '_blank'));

app.ports.toClipboard.subscribe(text => {
  navigator.clipboard.writeText(text);
})

app.ports.setDirty.subscribe(dirty => {
  isDirty = dirty
});

app.ports.saveToLocalStorage.subscribe(data => {
  for(let k in data) {
    localStorage.setItem(k, data[k]);
  }
});

window.onerror = (message) => {
  app.ports.uncaughtError.send({message})
  console.error(message)
  return true
}
