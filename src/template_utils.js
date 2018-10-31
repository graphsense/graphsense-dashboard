let replace = function (template, data) {
  for (let field in data) {
    template = template.replace(new RegExp('\{\{' + field + '\}\}', 'g'), data[field])
  }
  return template
}
export {replace}
