let replace = function (template, data) {
  for (let field in data) {
    template = template.replace('{{' + field + '}}', data[field])
  }
  return template
}
export {replace}
