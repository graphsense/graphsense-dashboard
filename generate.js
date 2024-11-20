var fs = require('fs')
var fse = require('fs-extra')
var mustache = require('mustache')
var path = require('path')
var yaml = require('yaml')
var codegen = require('elm-codegen')
const { execSync } = require("child_process");

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
const themeFolder = './theme'
const publicFolder = './public'
const genPublicFolder = path.join(genFolder, publicFolder)
const genLangFolder = path.join(genFolder, publicFolder, langFolder)
fse.copySync(publicFolder, genPublicFolder, {recursive: true})
fse.copySync(langFolder, genLangFolder, {recursive: true})

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

const copyPublic = (plugin) => {
  const pluginPublicFolder = path.join(pluginsFolder, plugin, publicFolder)
  if (!fs.existsSync(pluginPublicFolder)) return
  fse.copySync(pluginPublicFolder, genPublicFolder)
  console.log('Copied public folder', pluginPublicFolder)
}

const makeTheme = (plugin) => {
  const pluginThemeFile = path.join(pluginsFolder, plugin, themeFolder, 'figma.json')
  console.log("Making theme from " + pluginThemeFile)
  if (!fs.existsSync(pluginThemeFile)) return
    /*
  codegen.run("Generate.elm", {
    debug: true,
    output: "theme",
    flags: JSON.parse(fs.readFileSync(pluginThemeFile, 'utf8')),
    cwd: "./codegen",
  })
  */
  try {
    execSync(`./node_modules/.bin/elm-codegen run --debug --output theme --flags-from=${pluginThemeFile}`)
  } catch(e) {
    console.log(e.message)
  }
}

transform('./')

for(const plugin in plugins) {
  appendLang(plugins[plugin].name)
  copyPublic(plugins[plugin].name)
  makeTheme(plugins[plugin].name)
}


const elmJson = JSON.parse(fs.readFileSync('./elm.json.base'))

plugins.forEach(plugin => {
  const p = path.join(pluginsFolder, plugin.name, 'src')
  if(elmJson['source-directories'].indexOf(p) === -1) {
    elmJson['source-directories'].push(p)
  }
})

fs.writeFileSync('./elm.json', JSON.stringify(elmJson, null, 4))

console.log("\nUpdated src directories in elm.json")

plugins.forEach(plugin => {
  const deps = fs.readFileSync(path.join(pluginsFolder, plugin.name, 'dependencies.txt'), 'utf8')
  deps.split("\n").forEach(dep => {
    if(!dep) return
    let cmd = 'yes | elm install ' + dep
    console.log(cmd)
    try {
      console.log(execSync(cmd).toString('utf-8'))
    } catch(e) {
      console.error('ERROR:')
      console.error(e.message)
      process.exit(1)
    }
  })
})
