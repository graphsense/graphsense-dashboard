export default class GraphNode {
  constructor (labelType, graph) {
    this.labelType = labelType
    this.graph = graph
    this.labelHeight = 25
  }
  rerenderLabel () {
    let label = this.getLabel()
    this.root.select('text').text(label)
  }
  translate (x, y) {
    this.x += x
    this.y += y
  }
  getX () {
    return this.x
  }
  getY () {
    return this.y
  }
  getWidth () {
    return this.width
  }
  getHeight () {
    return this.height
  }
  setLabelType (labelType) {
    this.labelType = labelType
  }
}
