const DEBUG = 0
const ERROR = 1
let logLevel = DEBUG
export default {
  create: (name) => {
    const formatArgs = function (args) {
      args.unshift(`${name.toUpperCase()}:`)
      return args
    }
    return {
      debugObject: function (string, object) {
        if (logLevel > DEBUG) return
        let str = ''
        for (let key in object) {
          str += key + ': ' + (typeof object[key] === 'string' || typeof object[key] === 'number' ? object[key] : '<not a string>') + '\n'
        }
        let args = formatArgs([string, str])
        console.log.apply(null, args)
      },
      debug: function (string, object) {
        if (logLevel <= DEBUG) {
          let args = formatArgs([...arguments])
          console.log.apply(null, args)
        }
      },
      error: function (string, object) {
        if (logLevel <= ERROR) {
          let args = formatArgs([...arguments])
          console.error.apply(null, args)
        }
      },
      warn: function (string, object) {
        if (logLevel <= ERROR) {
          let args = formatArgs([...arguments])
          console.warn.apply(null, args)
        }
      }
    }
  },
  setLogLevel: level => {
    logLevel = level
  },
  LogLevels: {
    DEBUG,
    ERROR
  }
}
