export default class GraphNode {
  constructor (labelType, graph) {
    this.labelType = labelType
    this.graph = graph
    this.labelHeight = 25
    this.padding = 10
    this.numLetters = 8
  }
  renderLabel (root) {
    if (!root) {
      root = this.root.select('g.label')
    } else {
      root.classed('label', true)
    }
    let label = this.getLabel()
    console.log(root, label, label.length)
    let size
    if (label.length > this.numLetters) {
      if (label.length > this.numLetters * 2) {
        size = this.labelHeight * 0.5
        label = label.substring(0, this.numLetters * 2)
      } else {
        size = this.labelHeight * this.numLetters / label.length
      }
    } else {
      size = this.labelHeight
    }
    console.log('size', size)
    root.node().innerHTML = ''
    root.append('text')
      .style('font-size', size + 'px')
      .text(label)
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
  getTag (object) {
    if (object.userDefinedTags) {
      return object.userDefinedTags[0] || ''
    }
    return this.findTag(object)?.tag || ''
  }
  getActorCategory(object) {
    return this.findTag(object)?.actorCategory || ''
  }
  findTag (object) {
    let tags = object?.tags || []
    tags.sort((a,b) => {
      return a - b
    })
    for(let i = 0; i < tags.length; i++ ){
      if(tags[i].actorCategory) return tags[i]
    }
  }
}
