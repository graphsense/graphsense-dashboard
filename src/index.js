import {select} from 'd3-selection'
import './style.css'
import {dispatch} from './dispatch.js'
import Browser from './browser.js'
import Rest from './rest.js'

const dispatcher = dispatch(IS_DEV, 'search', 'searchresult')
const baseUrl = 'http://localhost:8000'

let browserRoot = select('body').append('div')
let browser = new Browser(dispatcher, browserRoot).render()

let rest = new Rest(dispatcher, baseUrl)

if (module.hot) {
  module.hot.accept('./browser.js', () => {
    console.log('Updating browser module')
    browser.remove()
    dispatcher.on('.browser', null)
    browser = new Browser(dispatcher, browserRoot).render()
    dispatcher.replay('browser')
  })
  module.hot.accept('./rest.js', () => {
    console.log('Updating rest module')
    rest = new Rest(dispatcher, baseUrl)
    dispatcher.replay('rest')
  })
}
