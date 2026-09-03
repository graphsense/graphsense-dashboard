import { Elm } from './Main.elm'
import plugins from '../generated/plugins/index.js'

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
        '&', '%', '@', '#', '*', '_', '"', '\'', '`', '~', '^', '<', '>', '°',
        '®', '©', '™',
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

// Only decides whether shortcut hints read Cmd or Ctrl; the chords themselves
// accept either modifier.
const isMac = /Mac|iPhone|iPad/.test(navigator.platform || navigator.userAgent)

for (const plugin in plugins) {
  pluginFlags[plugin] = plugins[plugin].flags()
}

// Sub-graphs parked for a tab that never got to boot (closed before its
// scripts ran) must not pile up, nor travel into the flags below. Runs at boot
// and before every new hand-over, so a leftover lives at most TTL past the
// next use of the app on this origin.
const HANDOFF_PREFIX = 'gs-handoff-'
const HANDOFF_TTL_MS = 5 * 60 * 1000
function purgeStaleHandoffs () {
  for (const k of Object.keys(localStorage)) {
    if (!k.startsWith(HANDOFF_PREFIX)) continue
    try {
      if (Date.now() - JSON.parse(localStorage.getItem(k)).at > HANDOFF_TTL_MS) localStorage.removeItem(k)
    } catch (_) {
      localStorage.removeItem(k)
    }
  }
}
purgeStaleHandoffs()

const app = Elm.Main.init(
  { flags: 
    { localStorage: {...localStorage}
    , characterDimensions
    , width
    , height
    , now
    , pluginFlags 
    , locale
    , isMac
    } 
  })

!!document.body.elmTree || console.warn('safe virtual dom not installed!')

// Pathfinder mod-key chords the browser would otherwise claim (find bar, save
// page, focus search bar, open file, K = browser search bar, D = bookmark). The shortcuts themselves are handled in
// Elm (Sub/Pathfinder.elm), on the same keydown — here we only suppress the
// browser default.
const shortCutKeys = ['f', 'k', 's', 'e', 'o', 'd']

// Same, but these have a meaning of their own inside a text input (select all,
// undo, redo), so there we leave them to the browser. Elm skips them in inputs too.
const shortCutKeysOutsideTextInput = ['a', 'z', 'y']

window.addEventListener('keydown', (e) => {
  if (!(e.ctrlKey || e.metaKey) || e.shiftKey || e.altKey) return
  if (!window.location.pathname.startsWith('/pathfinder')) return
  const key = e.key.toLowerCase()
  const activeElement = document.activeElement
  const isInputField = activeElement && (activeElement.tagName === 'INPUT' || activeElement.tagName === 'TEXTAREA')
  const isShortCut =
    shortCutKeys.indexOf(key) !== -1 ||
    (!isInputField && shortCutKeysOutsideTextInput.indexOf(key) !== -1)
  if (!isShortCut) return
  e.preventDefault()
  // Ctrl/Cmd+O: open a .gs file. Handled here (in JS) rather than in Elm,
  // because the native file picker needs a transient user-activation, which only
  // a trusted keydown grants. See openGsFile for the Firefox cold-load caveat.
  if (key === 'o') {
    openGsFile()
  }
})

// Reset modifier-key state in Elm when the window loses focus, so multi-select
// doesn't get stuck if the user releases Ctrl/Cmd while another window has focus
// (Alt+Tab, DevTools open, OS context menu, etc.). Browser.Events.onVisibilityChange
// only covers tab visibility, not window-level focus loss.
window.addEventListener('blur', () => {
  if (app.ports.windowBlurred) app.ports.windowBlurred.send(null)
})

let isDirty = false

window.onbeforeunload = (evt) => {
  if (import.meta.env.DEV) return
  if (!isDirty) return
  evt.preventDefault()
  evt.returnValue = true
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
  const canvasSize = (await import('canvas-size')).default
  return await Promise.all([
      canvasSize.maxArea(options),
      canvasSize.maxWidth(options),
      canvasSize.maxHeight(options)
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
    const bounds = 
      { minX : Infinity, 
        minY : Infinity, 
        maxX : -Infinity, 
        maxY : -Infinity
    }
    // Iterate through all rendered elements to find true bounds
    const allElements = svg.querySelectorAll(selector)
    allElements.forEach(el => {
      try {
        // Get the transformation matrix of the element
        const matrix = el.transform.baseVal.consolidate().matrix;

        const bbox = el.getBBox()

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
            bounds.minX = Math.min(bounds.minX, point.x);
            bounds.minY = Math.min(bounds.minY, point.y);
            bounds.maxX = Math.max(bounds.maxX, point.x);
            bounds.maxY = Math.max(bounds.maxY, point.y);
        });
      } catch (e) {
        // Skip elements that can't compute bbox
      }
    })
    if (isFinite(bounds.minX)) {
      return {
        x : bounds.minX,
        y : bounds.minY,
        width : bounds.maxX - bounds.minX,
        height : bounds.maxY - bounds.minY
      }
    }
    // Fallback if no elements found
    return svg.getBBox()
}

app.ports.getBBox.subscribe(([graphSelector, subSelector]) => {
  const graph = document.querySelector(graphSelector)
  if(!graph) app.ports.sendBBox.send(null)
  const bbox = getGraphBBox(graph, subSelector)
  app.ports.sendBBox.send(bbox)
})

app.ports.exportGraph.subscribe(async ({filename, graphId, viewbox, transparentBackground}) => {
  const FileSaver = (await import('file-saver')).default
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

  const robotoBase64 = (await import("../public/fonts/roboto/fonts/Regular/Roboto-Regular.woff2?raw-base64")).default
  const robotoBoldBase64 = (await import("../public/fonts/roboto/fonts/Bold/Roboto-Bold.woff2?raw-base64")).default

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
    app.ports.renderedImageForExport.send(false)
    console.error('Failed to load SVG image', e)
  }

  img.onload = async function () {
      let imgData 
      try {
        URL.revokeObjectURL(url)
        const bgColor = transparentBackground ? 'transparent' : cssVariables["--c-white"]

        // Use scale factor of 4 for high quality
        let canvas = new OffscreenCanvas(svgWidth, svgHeight)

        let ctx = canvas.getContext("2d");
        ctx.fillStyle = bgColor;
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        ctx.drawImage(img, 0, 0, svgWidth, svgHeight);
        app.ports.renderedImageForExport.send(true)

        // Use PNG for better quality, with compression
        imgData = await canvas.convertToBlob({type: 'image/png'})
      } catch (e) {
        const error = 'Image generation failed'
        console.error(error, e)
        app.ports.exportGraphResult.send(error)
        return
      }

      if(filename.endsWith(".pdf")) {
        let imgDataUrl
        try {
          imgDataUrl = URL.createObjectURL(imgData)
          const worker = new Worker(
            new URL('./svg-to-pdf-worker.js', import.meta.url),
            {type: 'module'}
          )
          const error = 'PDF generation failed'

          worker.onmessage = function(e) {
            if (e.data.error) {
              app.ports.exportGraphResult.send(e.data.error)
              console.error(e.data.error, e.data.details);
            } else {
              app.ports.exportGraphResult.send(null)
              FileSaver.saveAs(e.data.pdfBlob, e.data.filename)
            }
            worker.terminate()
            URL.revokeObjectURL(imgDataUrl)
          };

          worker.onerror = function(e) {
            app.ports.exportGraphResult.send(error)
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
          app.ports.exportGraphResult.send(error)
          URL.revokeObjectURL(imgDataUrl)
        }
    } else if (filename.endsWith(".png")) {
      FileSaver.saveAs(imgData, filename)
      app.ports.exportGraphResult.send(null)
    }
  };
  img.src = url 
})

const download = async (filename, buffer) => {
  const blob = new Blob([buffer], { type: 'application/octet-stream' }) // eslint-disable-line no-undef
  const FileSaver = (await import('file-saver')).default
  FileSaver.saveAs(blob, filename)
}

const compress = async (data) => {
  const lzw = (await import('lzwcompress')).default
  const { Base64 } = await import('js-base64')
  return new Uint32Array(
    lzw.pack(
      // convert to base64 (utf-16 safe)
      Base64.encode(
        JSON.stringify(data)
      )
    )
  ).buffer
}

const decompress = async (data) => {
  if (!(data instanceof ArrayBuffer) || data.byteLength === 0 || data.byteLength % 4 !== 0) {
    throw new Error('unsupported-file-format')
  }

  const lzw = (await import('lzwcompress')).default
  const { Base64 } = await import('js-base64')
  try {
    const unpacked = lzw.unpack([...new Uint32Array(data)])
    if (typeof unpacked !== 'string' || unpacked.length === 0) {
      throw new Error('unsupported-file-format')
    }

    const decoded = Base64.decode(unpacked)
    if (typeof decoded !== 'string' || decoded.length === 0) {
      throw new Error('unsupported-file-format')
    }

    return JSON.parse(decoded)
  } catch (_) {
    throw new Error('unsupported-file-format')
  }
}

const unsupportedImportFileMessage = 'Unsupported file format. Please import a valid GraphSense .gs file.'

const reportImportFileError = (error, fileName = '') => {
  const message = fileName
    ? unsupportedImportFileMessage + ' File: ' + fileName
    : unsupportedImportFileMessage

  app.ports.uncaughtError.send({
    message,
    messageKey: 'unsupported-gs-file-format',
    titleKey: 'data error',
    variables: [],
    moreInfo: fileName ? [ fileName ] : []
  })

  const detail =
    error && typeof error.message === 'string'
      ? error.message
      : String(error)
  console.warn('Failed to import GraphSense file', fileName, detail)
}


// Opens the native file picker and loads the selected .gs file. Used by both
// the toolbar open button (deserialize port) and the Ctrl/Cmd+O shortcut.
//
// Firefox caveat: opening the picker needs *transient user activation*, which
// Firefox does NOT grant for a Ctrl/Cmd-modified keydown on a freshly loaded
// page (no prior interaction). So a cold Ctrl/Cmd+O does nothing in Firefox
// until the user has clicked/interacted once; after that the shortcut works.
// Chrome grants the activation from the keydown, so it always works there. The
// toolbar button (a real click) always works in both. This cannot be worked
// around in code — activation the browser withholds cannot be synthesized.
async function openGsFile () {
  const { fileDialog } = await import('file-select-dialog')
  let file
  try {
    file = await fileDialog({ accept: '.gs', strict: true })
  } catch (_) {
    return
  }
  if (!file) {
    return
  }
  loadGsFile(file)
}

// Validates and loads a .gs File object. Used by the file picker (openGsFile)
// and by drag-and-drop (deserializeFile port).
function loadGsFile (file) {
  if (!file.name.toLowerCase().endsWith('.gs')) {
    reportImportFileError(new Error('unsupported-extension'), file.name)
    return
  }
  const reader = new FileReader() // eslint-disable-line no-undef
  reader.onload = async () => {
    try {
      let data = reader.result
      data = await decompress(data)

      if (!Array.isArray(data) || data.length === 0 || typeof data[0] !== 'string') {
        throw new Error('invalid-import-payload')
      }

      data[0] = data[0].split(' ')[0]
      data[0] = data[0].split('-')[0]
      app.ports.deserialized.send([file.name, data])
    } catch (error) {
      reportImportFileError(error, file.name)
    }
  }
  reader.onerror = () => reportImportFileError(new Error('file-read-error'), file.name)
  reader.readAsArrayBuffer(file)
}

// Toolbar "open graph" button -> open the file picker.
app.ports.deserialize.subscribe(openGsFile)

// File dropped onto the landing page load box.
app.ports.deserializeFile.subscribe(loadGsFile)

// A file dropped anywhere else would make the browser navigate to the file,
// replacing the app. Swallow such drops and show the no-drop cursor. Drops on
// real drop targets are unaffected: their handlers preventDefault first.
window.addEventListener('dragover', (e) => {
  if (!e.defaultPrevented) {
    e.preventDefault()
    e.dataTransfer.dropEffect = 'none'
  }
})
window.addEventListener('drop', (e) => e.preventDefault())

// Download a .gs graph by its API download id and load it through the same path
// as the file picker. Used by the ?import=<id> deep link. We take an opaque id
// (not a full URL) so a link can only ever fetch from the fixed REST download
// endpoint. Note: the REST host must serve CORS headers for this origin.
async function importByApiDownloadId (id) {
  if (!/^[A-Za-z0-9._-]+$/.test(id)) {
    reportImportFileError(new Error('invalid-api-download-id'), id)
    return
  }
  // Use the same VITE_GS_REST_URL placeholder as the Elm API client
  // (openapi/src/Api.elm), replaced at build time by the envReplacePlugin in
  // vite.config.mjs. This reads the value via Vite's loadEnv(), which (unlike
  // import.meta.env) also picks up the var from the shell environment, so the
  // import endpoint always matches the REST host the Elm app talks to. The
  // trailing replace strips the placeholder if the env var was never set,
  // falling back to a relative URL rather than a literal "{{...}}".
  const restUrl = '{{VITE_GS_REST_URL}}'.replace(/^\{\{.*\}\}$/, '')
  const url = restUrl.replace(/\/?$/, '/') + 'download/' + encodeURIComponent(id)
  const displayName = id + '.gs'
  try {
    const resp = await fetch(url)
    if (!resp.ok) throw new Error('http-' + resp.status)
    const data = await decompress(await resp.arrayBuffer())
    if (!Array.isArray(data) || data.length === 0 || typeof data[0] !== 'string') {
      throw new Error('invalid-import-payload')
    }
    // Same format-tag normalization as openGsFile.
    data[0] = data[0].split(' ')[0].split('-')[0]
    app.ports.deserialized.send([displayName, data])
  } catch (error) {
    reportImportFileError(error, displayName)
  }
}

// Loads the sub-graph a sibling tab parked for us (openGraphInNewTab port).
// One-shot: the entry is removed and the query parameter stripped, so a reload
// leaves a plain Pathfinder tab, as after opening a .gs file.
function importHandoff (key) {
  const raw = localStorage.getItem(HANDOFF_PREFIX + key)
  localStorage.removeItem(HANDOFF_PREFIX + key)
  const url = new URL(window.location)
  url.searchParams.delete('handoff')
  history.replaceState(null, '', url)
  const displayName = 'selection.gs'
  try {
    if (!raw) throw new Error('handoff-expired')
    const data = JSON.parse(raw).body
    if (!Array.isArray(data) || data.length === 0 || typeof data[0] !== 'string') {
      throw new Error('invalid-import-payload')
    }
    data[0] = data[0].split(' ')[0].split('-')[0]
    app.ports.deserialized.send([displayName, data])
  } catch (error) {
    reportImportFileError(error, displayName)
  }
}

const handoffKey = new URLSearchParams(window.location.search).get('handoff')
if (handoffKey) importHandoff(handoffKey)

// Deep link: <app>/pathfinder?import=<id> downloads and opens that graph on load.
const apiDownloadId = new URLSearchParams(window.location.search).get('import')
if (apiDownloadId) importByApiDownloadId(apiDownloadId)

app.ports.serialize.subscribe(async ([filename, body]) => {
  await download(filename, await compress(body))
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

// Invisible sensor that reports size changes of its parent element as
// 'sensor-resize' CustomEvents. Used by Components.InfiniteTable to keep its
// measured container height fresh (scroll containers resize with the window
// or layout changes, which Elm cannot observe by itself).
class ResizeSensor extends HTMLElement {
  connectedCallback () {
    if (!this.parentElement) return
    this._observer = new ResizeObserver(entries => {
      const rect = entries[entries.length - 1].contentRect
      this.dispatchEvent(new CustomEvent('sensor-resize', {
        detail: { width: rect.width, height: rect.height }
      }))
    })
    this._observer.observe(this.parentElement)
  }

  disconnectedCallback () {
    if (this._observer) {
      this._observer.disconnect()
      this._observer = undefined
    }
  }
}

if(!customElements.get('resize-sensor')) {
  customElements.define('resize-sensor', ResizeSensor)
}

app.ports.newTab.subscribe( url => window.open(url, '_blank'));

// "Open selection in new tab": the sub-graph arrives as a .gs payload (the same
// JSON the save button compresses). It is parked in localStorage -- same-origin,
// shared across tabs, big enough -- under a one-time key, and the new tab picks
// it up at boot via ?handoff=<key> (see importHandoff). A URL fragment would
// make the link shareable, but a few hundred nodes already make it unwieldy.
app.ports.openGraphInNewTab.subscribe(body => {
  purgeStaleHandoffs()
  const key = (crypto.randomUUID && crypto.randomUUID()) || (Date.now().toString(36) + Math.random().toString(36).slice(2))
  const storageKey = HANDOFF_PREFIX + key
  try {
    localStorage.setItem(storageKey, JSON.stringify({ at: Date.now(), body }))
  } catch (error) {
    // quota exceeded, or storage disabled: nothing to hand over, so no tab
    console.warn('Could not hand the selection over to a new tab', error)
    return
  }
  const tab = window.open('/pathfinder?handoff=' + encodeURIComponent(key), '_blank')
  if (!tab) {
    // popup blocked: the entry would never be consumed
    localStorage.removeItem(storageKey)
  }
})

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

window.onerror = (message, source, lineno, colno, error) => {
  app.ports.uncaughtError.send({message: message + 'win'})
  // log the full error so the stack trace of crashes in compiled Elm code is
  // not swallowed (message alone gives no hint where a crash originated)
  console.error(error || message)
  return true
}

app.ports.blur.subscribe(id => {
  const el = document.getElementById(id)
  if (!el) return
  if (typeof el.blur !== 'function') return
  el.blur()
})

