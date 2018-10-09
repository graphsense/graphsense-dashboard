import address from './address.html'

export default class Address {
  constructor (data) {
    this.root = document.createElement('div')
    this.data = data
  }
  render () {
    this.root.innerHTML = address
    let el = this.root.querySelector('.address-value.id')
    if (el) el.innerHTML = this.data.address
    return this.root
  }
}
