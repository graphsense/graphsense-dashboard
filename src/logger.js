const DEBUG = 0
const ERROR = 1
let logLevel = DEBUG
export default {
  create: (name) => {
    const formatArgs = function (args, bold) {
      let title = `${name.toUpperCase()}:`
      if (bold) {
        title = '%c' + title
        args.unshift('background-color: rgba(0, 255, 255, 0.5)')
      }
      args.unshift(title)
      return args
    }
    let debugFunction = function (bold) {
      return function (string, object) {
        if (logLevel <= DEBUG) {
          let args = formatArgs([...arguments], bold)
          console.log.apply(null, args)
        }
      }
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
      boldDebug: debugFunction(true),
      debug: debugFunction(false),
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
