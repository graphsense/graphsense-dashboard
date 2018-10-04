import {select} from 'd3-selection'
import './style.css'
import {dispatch} from './dispatch.js'
import Browser from './browser.js'

const dispatcher = dispatch(IS_DEV, 'load', 'statechange')

let browserRoot = select('body').append('div')
let browser = new Browser(dispatcher, browserRoot).render()

if (module.hot) {
  module.hot.accept('./browser.js', () => {
    console.log('Updating browser module')
    browser.remove()
    dispatcher.on('.browser', null)
    browser = new Browser(dispatcher, browserRoot).render()
    dispatcher.replay('browser')
  })
}
