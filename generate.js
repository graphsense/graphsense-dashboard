var fs = require('fs')
var mustache = require('mustache')
var path = require('path')
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
        fs.mkdirSync(pf)
      }
      fs.writeFileSync(newFileName, gen)
    })
}

transform('./')


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

