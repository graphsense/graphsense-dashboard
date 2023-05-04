var fs = require('fs')
var fse = require('fs-extra')
var mustache = require('mustache')
var path = require('path')
var yaml = require('yaml')
const { spawn } = require("child_process");

const isDir = fileName => {
  try {
    return fs.lstatSync(fileName).isDirectory();
  } catch(e) {
    if (e.code === 'ENOENT') {
      return false
    }
  }
};

const pluginsFolder = './plugins'
const templatesFolder = './plugin_templates'
const generatedFolder = './plugin_generated'
const langFolder = 'lang'
const publicFolder = './public'

console.log('Installing plugins:')
const plugins = fs.readdirSync(pluginsFolder)
  .filter(fileName => isDir(path.join(pluginsFolder, fileName)))
  .map(plugin => {
    console.log(plugin)
    return { 
      name : plugin,
      package : plugin.charAt(0).toUpperCase() + plugin.slice(1)
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
      const newFileName = path.join(generatedFolder, folder, fileName.replace('.mustache', ''))
      console.log('Generating ', newFileName)
      const gen = mustache.render(file, {plugins})
      const pf = path.join(generatedFolder, folder)
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
      let publicLangFilename = path.join(publicFolder, langFolder, fileName)
      let pluginLangFilename = path.join(pluginLangFolder, fileName)
      if(!fs.existsSync(publicLangFilename)) {
        console.err(`Ignoring ${pluginLangFilename}.`)
        return
      }
      let file = fs.readFileSync(pluginLangFilename, 'utf8')
      let pluginStrings = yaml.parse(file)
      file = fs.readFileSync(publicLangFilename, 'utf8')
      let strings = yaml.parse(file)
      strings = {...strings, ...pluginStrings}
      strings = yaml.stringify(strings)
      fs.writeFileSync(publicLangFilename, strings, {flag: 'w+'})
      console.log('Merged', fileName)
    })
}

const copyPublic = (plugin) => {
  const pluginPublicFolder = path.join(pluginsFolder, plugin, publicFolder)
  if (!fs.existsSync(pluginPublicFolder)) return
  fse.copySync(pluginPublicFolder, publicFolder)
  console.log('Copied public folder', pluginPublicFolder)
}

transform('./')

for(const plugin in plugins) {
  appendLang(plugins[plugin].name)
  copyPublic(plugins[plugin].name)
}


const elmJson = JSON.parse(fs.readFileSync('./elm.json'))

plugins.forEach(plugin => {
  const p = path.join(pluginsFolder, plugin.name, 'src')
  if(elmJson['source-directories'].indexOf(p) === -1) {
    elmJson['source-directories'].push(p)
  }
})

fs.writeFileSync('./elm.json', JSON.stringify(elmJson, null, 4))

console.log("\nUpdated src directories in elm.json")

console.log("\nNow please run:")

plugins.forEach(plugin => {
  const deps = fs.readFileSync(path.join(pluginsFolder, plugin.name, 'dependencies.txt'), 'utf8')
  deps.split("\n").forEach(dep => {
    if(!dep) return
    console.log('elm install ' + dep)
  })

})

