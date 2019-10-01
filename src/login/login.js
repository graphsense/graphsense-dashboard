import Logger from '../logger.js'
import login from './login.html'
import Component from '../component.js'
const logger = Logger.create('Login') // eslint-disable-line no-unused-vars

export default class Login extends Component {
  constructor (dispatcher) {
    super()
    this.dispatcher = dispatcher
  }
  loading (isLoading) {
    this.isLoading = isLoading
    this.setUpdate('loading')
  }
  error (msg) {
    this.errorMessage = msg
    this.setUpdate('error')
  }
  render (root) {
    logger.debug('render')
    if (root) this.root = root
    if (!this.root) throw new Error('root not defined')
    if (!this.shouldUpdate()) return
    if (this.shouldUpdate(true)) {
      this.root.innerHTML = login
      this.root.querySelector('form').addEventListener('submit', (e) => {
        e.preventDefault()
        console.log(e)
        this.dispatcher('login', [e.target.elements['username'].value, e.target.elements['password'].value])
      })
      super.render()
      return this.root
    }
    if (this.shouldUpdate('loading')) {
      this.root.querySelectorAll('input').forEach((input) => {
        input.disabled = this.isLoading || false
        if (input.type === 'submit') {
          if (this.isLoading) {
            input.value = 'Signing in ...'
          } else {
            input.value = 'Sign in'
          }
        }
      })
    }
    if (this.shouldUpdate('error')) {
      this.root.querySelector('#error').innerHTML = this.errorMessage
    }
    super.render()
    return this.root
  }
}
