var fs = require('fs')
var fse = require('fs-extra')
var mustache = require('mustache')
var path = require('path')
var yaml = require('yaml')
const { execSync } = require("child_process");

function parseNamespace(filePath) {
  // Read the content of the file
  const content = fs.readFileSync(filePath, 'utf-8');
  
  // Define a regular expression to capture the value after 'namespace ='
  const regex = /namespace\s*=\s*"([^"]+)"/;
  
  // Search for the pattern in the file content
  const match = content.match(regex);
  
  if (match) {
    return match[1];  // Return the captured value (namespace)
  } else {
    return null;  // Return null if no match is found
  }
}

const isDir = fileName => {
  try {
    return fs.lstatSync(fileName).isDirectory() || fs.lstatSync(fileName).isSymbolicLink();
  } catch(e) {
    if (e.code === 'ENOENT') {
      return false
    }
  }
};

const pluginsFolder = './plugins'
const templatesFolder = './plugin_templates'
const genFolder = './generated'
const genPluginsFolder = path.join(genFolder, pluginsFolder)
const langFolder = './lang'
const publicFolder = './public'
const genPublicFolder = path.join(genFolder, publicFolder)
const genLangFolder = path.join(genFolder, publicFolder, langFolder)
fse.copySync(publicFolder, genPublicFolder, {recursive: true})
fse.copySync(langFolder, genLangFolder, {recursive: true})

console.log('Generating glue code for plugins:')
let plugins = fs.readdirSync(pluginsFolder)
  .filter(fileName => isDir(path.join(pluginsFolder, fileName)))

plugins.sort((a, b) => {
  // Check if strings end with '_preview'
  const aEndsWithPreview = a.endsWith("_preview");
  const bEndsWithPreview = b.endsWith("_preview");

  // If both or neither strings end with '_preview', sort them lexicographically
  if (aEndsWithPreview === bEndsWithPreview) {
    return a.localeCompare(b);
  }

  // If only one string ends with '_preview', place it after the other
  return aEndsWithPreview ? 1 : -1;
});


plugins = plugins.map(plugin => {
    console.log(plugin)
    const packageName = plugin.charAt(0).toUpperCase() + plugin.slice(1)
    const namespace = parseNamespace(path.join(pluginsFolder, plugin, 'src', packageName, 'Model.elm'))
    return { 
      raw_name : plugin,
      name : plugin.toLowerCase(),
      namespace : namespace, 
      package : packageName
    }
  })

if(plugins.length === 0) {
  console.log('No plugins found')
} else {
  plugins[plugins.length - 1].last = true
}

console.log("")


const transform = (folder) => {
  fs.readdirSync(path.join(templatesFolder, folder))
    .map(fileName => {
      if(isDir(path.join(templatesFolder, folder, fileName))) {
        transform(path.join(folder, fileName))
        return
      }
      if (path.extname(fileName) !== '.mustache') return
      const file = fs.readFileSync(path.join(templatesFolder, folder, fileName), 'utf8')
      const newFileName = path.join(genPluginsFolder, folder, fileName.replace('.mustache', ''))
      console.log('Generating ', newFileName)
      const gen = mustache.render(file, {plugins})
      const pf = path.join(genPluginsFolder, folder)
      if(!isDir(pf)) {
        fs.mkdirSync(pf, {recursive: true})
      }
      fs.writeFileSync(newFileName, gen)
    })
}

const appendLang = (plugin) => {
  console.log('Merge translation files for',  plugin)
  const pluginLangFolder = path.join(pluginsFolder, plugin, langFolder)
  fs.readdirSync(pluginLangFolder)
    .map(fileName => {
      let coreLangFilename = path.join(langFolder, fileName)
      let genLangFilename = path.join(genLangFolder, fileName)
      let pluginLangFilename = path.join(pluginLangFolder, fileName)
      if(!fs.existsSync(coreLangFilename)) {
        console.err(`Ignoring ${pluginLangFilename}.`)
        return
      }
      let strings = yaml.parse(fs.readFileSync(genLangFilename, 'utf8'))

      let pluginStrings = yaml.parse(fs.readFileSync(pluginLangFilename, 'utf8'))
      strings = yaml.stringify({...strings, ...pluginStrings})
      fs.writeFileSync(genLangFilename, strings, {flag: 'w+'})
      console.log('Merged', fileName)
    })
}

transform('./')

for(const plugin in plugins) {
  appendLang(plugins[plugin].raw_name)
}


const elmJson = JSON.parse(fs.readFileSync('./elm.json'))

// remove all plugin src directories first
elmJson['source-directories'] = elmJson['source-directories'].filter(s => !s.startsWith(path.join(pluginsFolder)))

// add the installed plugin src directories 
plugins.forEach(plugin => {
  const p = path.join(pluginsFolder, plugin.raw_name, 'src')
  if(elmJson['source-directories'].indexOf(p) === -1) {
    elmJson['source-directories'].push(p)
  }
})

fs.writeFileSync('./elm.json', JSON.stringify(elmJson, null, 4))

console.log("\nUpdated src directories in elm.json")

