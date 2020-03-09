/** Adapted from d3-dispatch */

var noop = { value: function () {} }

function dispatch () {
  for (var i = 1, n = arguments.length, _ = {}, t; i < n; ++i) {
    if (!(t = arguments[i] + '') || (t in _)) throw new Error('illegal type: ' + t)
    _[t] = []
  }
  return new Dispatch(arguments[0], _)
}

function Dispatch (storeHistory, _) {
  this._ = _
  this.history = storeHistory ? [] : null
}

function parseTypenames (typenames, types) {
  return typenames.trim().split(/^|\s+/).map(function (t) { return parseTypename(t, types) })
}

function parseTypename (t, types) {
  var name = ''; var i = t.indexOf('.')
  if (i >= 0) name = t.slice(i + 1), t = t.slice(0, i)
  if (t && !types.hasOwnProperty(t)) throw new Error('unknown type: ' + t)
  return { type: t, name: name }
}

function add (history, type, context, data) {
  if (history !== null) {
    history.push({ type, context, data })
  }
}

Dispatch.prototype = dispatch.prototype = {
  constructor: Dispatch,
  on: function (typename, callback) {
    var _ = this._
    var T = parseTypenames(typename + '', _)
    var t
    var i = -1
    var n = T.length

    // If no callback was specified, return the callback of the given type and name.
    if (arguments.length < 2) {
      while (++i < n) if ((t = (typename = T[i]).type) && (t = get(_[t], typename.name))) return t
      return
    }

    // If a type was specified, set the callback for the given type and name.
    // Otherwise, if a null callback was specified, remove callbacks of the given name.
    if (callback != null && typeof callback !== 'function') throw new Error('invalid callback: ' + callback)
    while (++i < n) {
      if (t = (typename = T[i]).type) _[t] = set(_[t], typename.name, callback)
      else if (callback == null) for (t in _) _[t] = set(_[t], typename.name, null)
    }

    return this
  },
  copy: function () {
    var copy = {}; var _ = this._
    for (var t in _) copy[t] = _[t].slice()
    return new Dispatch(copy)
  },
  call: function (type, that) {
    if (this.replaying) {
      return
    }
    if ((n = arguments.length - 2) > 0) for (var args = new Array(n), i = 0, n, t; i < n; ++i) args[i] = arguments[i + 2]
    if (!this._.hasOwnProperty(type)) throw new Error('unknown type: ' + type)
    add(this.history, type, that, Array.prototype.slice.call(arguments, 2))
    for (t = this._[type], i = 0, n = t.length; i < n; ++i) t[i].value.apply(that, args)
  },
  apply: function (type, that, args) {
    var tt = parseTypename(type, this._)
    if (!this._.hasOwnProperty(tt.type)) throw new Error('unknown type: ' + tt.type)
    for (var t = this._[tt.type], i = 0, n = t.length; i < n; ++i) {
      if (tt.name === '') {
        t[i].value.apply(that, args)
        continue
      }
      if (tt.name === t[i].name) {
        t[i].value.apply(that, args)
      }
    }
  },
  replay: function (name) {
    var that = this
    this.replaying = true
    this.history.forEach(function (h) {
      that.apply(h.type + (name ? '.' + name : ''), h.context, h.data)
    })
    this.replaying = false
  }
}

function get (type, name) {
  for (var i = 0, n = type.length, c; i < n; ++i) {
    if ((c = type[i]).name === name) {
      return c.value
    }
  }
}

function set (type, name, callback) {
  for (var i = 0, n = type.length; i < n; ++i) {
    if (type[i].name === name) {
      type[i] = noop, type = type.slice(0, i).concat(type.slice(i + 1))
      break
    }
  }
  if (callback != null) type.push({ name: name, value: callback })
  return type
}

export { dispatch }
