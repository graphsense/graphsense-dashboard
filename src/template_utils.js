let replace = function (template, data) {
  for (let field in data) {
    template = template.replace(new RegExp('\{\{' + field + '\}\}', 'g'), data[field])
  }
  return template
}

let addClass = function (el, cl) {
  let classes = new Set(el.className.split(' '))
  classes.add(cl)
  el.className = [...classes].join(' ')
}

let removeClass = function (el, cl) {
  let classes = new Set(el.className.split(' '))
  classes.delete(cl)
  el.className = [...classes].join(' ')
}
export {replace, addClass, removeClass}
