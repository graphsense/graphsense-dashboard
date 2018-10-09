import {select} from 'd3-selection'
import './style.css'
import {dispatch} from './dispatch.js'
import Browser from './browser.js'
import Search from './browser/search.js'
import Address from './browser/address.js'
import Rest from './rest.js'
import Layout from './layout.js'

const dispatcher = dispatch(IS_DEV, 'search', 'searchresult')
const baseUrl = 'http://localhost:8000'

let browser = new Browser(dispatcher)

let rest = new Rest(dispatcher, baseUrl)

let layout = new Layout(dispatcher)
layout.setBrowser(browser)
document.body.append(layout.render())

if (module.hot) {
  module.hot.accept(['./browser.js', './browser/search.js', './browser/search.html', './browser/address.js', './browser/address.html'], () => {
    console.log('Updating browser module')
    dispatcher.on('.browser', null)
    browser = new Browser(dispatcher)
    layout.setBrowser(browser)
    browser.render()
    dispatcher.replay('browser')
  })
  module.hot.accept('./rest.js', () => {
    console.log('Updating rest module')
    rest = new Rest(dispatcher, baseUrl)
    dispatcher.replay('rest')
  })
}
