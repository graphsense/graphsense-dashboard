const replace = function (template, data) {
  for (const field in data) {
    template = template.replace(new RegExp('{{' + field + '}}', 'g'), data[field])
  }
  return template
}

const addClass = function (el, cl) {
  const classes = new Set(el.className.split(' '))
  classes.add(cl)
  el.className = [...classes].join(' ')
}

const removeClass = function (el, cl) {
  const classes = new Set(el.className.split(' '))
  classes.delete(cl)
  el.className = [...classes].join(' ')
}
export { replace, addClass, removeClass }
