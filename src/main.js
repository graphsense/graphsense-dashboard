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


let maxDimensions = null
const getMaxCanvasDimensions = async () => {
  if(maxDimensions) return maxDimensions
  let options = {
    max: 32767,
    min: 1,
    step: 1024,
    useWorker: true
  }
  const canvasSize = await import('canvas-size')
  return await Promise.all([
      canvasSize.default.maxArea(options),
      canvasSize.default.maxWidth(options),
      canvasSize.default.maxHeight(options)
    ]).then(([maxLength, maxWidth, maxHeight]) => {
      maxDimensions = {
        maxArea : maxLength.width * maxLength.height,
        maxWidth: maxWidth.width,
        maxHeight: maxHeight.height
      }
      return maxDimensions
    })
}

const getGraphBBox = (svg, selector) => {
    // Get the bounding box of the entire graph content by checking all elements
    let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity
    // Iterate through all rendered elements to find true bounds
    const allElements = svg.querySelectorAll(selector)
    allElements.forEach(el => {
      try {
        // Get the transformation matrix of the element
        const matrix = element.transform.baseVal.consolidate();

        // Apply the transformation to the bounding box
        const points = [
            { x: bbox.x, y: bbox.y },
            { x: bbox.x + bbox.width, y: bbox.y },
            { x: bbox.x, y: bbox.y + bbox.height },
            { x: bbox.x + bbox.width, y: bbox.y + bbox.height }
        ];

        const transformedPoints = points.map(point => {
            const x = matrix.a * point.x + matrix.c * point.y + matrix.e;
            const y = matrix.b * point.x + matrix.d * point.y + matrix.f;
            return { x, y };
        });

        // Update min and max coordinates
        transformedPoints.forEach(point => {
            minX = Math.min(minX, point.x);
            minY = Math.min(minY, point.y);
            maxX = Math.max(maxX, point.x);
            maxY = Math.max(maxY, point.y);
        });
      } catch (e) {
        // Skip elements that can't compute bbox
      }
    })
    
    if (isFinite(minX)) {
      return {
        x : minX,
        y : minY,
        width : maxX - minX,
        height : maxY - minY
      }
    }
    // Fallback if no elements found
    return svg.getBBox()
}

app.ports.getBBox.subscribe(([handle, graphSelector, subSelector]) => {
  const graph = document.querySelector(graphSelector)
  if(!graph) app.ports.sendBBox.send([handle, null])
  const bbox = getGraphBBox(graph, subSelector)
  app.ports.sendBBox.send([handle, bbox])
})

app.ports.exportGraph.subscribe(async ({filename, graphId, viewbox}) => {
  let svg = document.querySelector('svg#' + graphId)
  if (!svg) {
    console.error('SVG element not found')
    return
  }

  if(!viewbox) {
    viewbox = svg.getAttribute('viewBox')
    // Split the viewBox string into an array of numbers
    const viewBoxArray = viewbox.split(' ').map(Number);

    // Create an object with x, y, width, and height properties
    viewbox = {
      x: viewBoxArray[0],
      y: viewBoxArray[1],
      width: viewBoxArray[2],
      height: viewBoxArray[3]
    }
  }

  const {maxArea, maxWidth, maxHeight} = await getMaxCanvasDimensions()
  const aspect_ratio = viewbox.width / viewbox.height

  const pixelScaleFactor = 4;
  let svgWidth = viewbox.width * pixelScaleFactor
  let svgHeight = viewbox.height * pixelScaleFactor

  if(svgWidth > maxWidth || svgHeight > maxHeight) {
    if(svgWidth > svgHeight) {
        svgWidth = maxWidth
        svgHeight = svgWidth / aspect_ratio
    } else {
        svgHeight = maxHeight
        svgWidth = svgHeight * aspect_ratio
    }
  }
  if(svgWidth * svgHeight > maxArea) {
    svgWidth = Math.sqrt(maxArea * aspect_ratio)
    svgHeight = Math.sqrt(maxArea / aspect_ratio)
  }
  // Replace the viewBox to show entire content with padding
  const newViewBox = `${viewbox.x} ${viewbox.y} ${viewbox.width} ${viewbox.height}`
  var svgData = new XMLSerializer().serializeToString(svg)
    .replace(/viewBox="[^"]*"/, `viewBox="${newViewBox}"`)
    .replace(/(<svg[^>]*)\swidth="[^"]*"/, `$1 width="${svgWidth}"`)
    .replace(/(<svg[^>]*)\sheight="[^"]*"/, `$1 height="${svgHeight}"`)

  // replace css variables with actual values
  const cssVariables = getTheme()
  for (const [key, value] of Object.entries(cssVariables)) {
    svgData = svgData.replaceAll("var(" + key + ")", value)
  }

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
  const blobSvgData = new Blob([svgData], { type: 'image/svg+xml;charset=utf-8' });
  const url = URL.createObjectURL(blobSvgData);
  
  let img = new Image();

  img.onerror = function(e) {
    app.ports.uncaughtError.send({message: e + "img"})
    console.error('Failed to load SVG image', e)
  }

  img.onload = async function () {
      let imgData 
      try {
        URL.revokeObjectURL(url)
        const bgColor = cssVariables["--c-white"]

        // Use scale factor of 4 for high quality
        let canvas = new OffscreenCanvas(svgWidth, svgHeight)

        let ctx = canvas.getContext("2d");
        ctx.fillStyle = bgColor;
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        ctx.drawImage(img, 0, 0, svgWidth, svgHeight);

        // Use PNG for better quality, with compression
        imgData = await canvas.convertToBlob({type: 'image/png'})
      } catch (e) {
        const error = 'Image generation failed'
        console.error(error, e)
        app.ports.exportGraphResult.send({filename, error})
        return
      }

      if(filename.endsWith(".pdf")) {
        let imgDataUrl
        try {
          imgDataUrl = URL.createObjectURL(imgData)
          const worker = new Worker('/src/svg-to-pdf-worker.js', {type:'module'});
          const error = 'PDF generation failed'

          worker.onmessage = function(e) {
            if (e.data.error) {
              app.ports.exportGraphResult.send({filename, error: e.data.error})
              console.error(e.data.error, e.data.details);
            } else {
              app.ports.exportGraphResult.send({filename, error: null})
              FileSaver.saveAs(e.data.pdfBlob, e.data.filename)
            }
            worker.terminate()
            URL.revokeObjectURL(imgDataUrl)
          };

          worker.onerror = function(e) {
            app.ports.exportGraphResult.send({filename, error})
            URL.revokeObjectURL(imgDataUrl)
            console.error('Worker error:', e);
          };

          worker.postMessage({
            imgDataUrl,
            width: svgWidth,
            height: svgHeight,
            filename
          });
        } catch (e) {
          console.error(error, e)
          app.ports.exportGraphResult.send({filename, error})
          URL.revokeObjectURL(imgDataUrl)
        }
    } else if (filename.endsWith(".png")) {
      FileSaver.saveAs(imgData, filename)
      app.ports.exportGraphResult.send({filename, error: null})
    }
  };
  img.src = url 
})

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

class WithHint extends HTMLElement {
  constructor () {
    let label, original
    setTimeout(() => {
      label = this.querySelector('[data-label]')
      original = label?.innerText
    }, 0)
    super()
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

if(!customElements.get('with-hint')) {
  customElements.define('with-hint', WithHint) 
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
  app.ports.uncaughtError.send({message: message + 'win'})
  console.error(message)
  return true
}
