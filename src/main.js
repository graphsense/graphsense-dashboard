import { Elm } from "./Main.elm";

const getNavigatorLanguage = () => {
  if (navigator.languages && navigator.languages.length) {
    return navigator.languages[0]
  } else {
    return navigator.userLanguage || navigator.language || navigator.browserLanguage || 'en'
  }
}

const locale = getNavigatorLanguage().split('-')[0]

const docElem = document.documentElement
const body = document.getElementsByTagName('body')[0]
const width = window.innerWidth || docElem.clientWidth || body.clientWidth
const height = window.innerHeight || docElem.clientHeight || body.clientHeight

const now = +(new Date())

const app = Elm.Main.init({flags: {locale, width, height, now}});
