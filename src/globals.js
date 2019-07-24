const browserHeight = 220
const browserPadding = 1
const clusterWidth = 190
const expandHandleWidth = 15
const maxAddableNodes = 100
const maxSearchDepth = 7
const maxSearchBreadth = 100

const moreThan1TagCategory = 'More than 1 tag'

const categories = [
  'Organization',
  'Miner',
  'Exchange',
  'Walletprovider',
  'Marketplace',
  'Mixingservice',
  'Old/historic',
  'Gambling',
  'Services/others',
  'Ransomware',
  'Sextortion',
  moreThan1TagCategory
]

const currencies =
  {
    'btc': 'Bitcoin',
    'ltc': 'Litecoin',
    'bch': 'Bitcoin Cash',
    'zec': 'Zcash',
    'xrp': 'Ripple',
    'eth': 'Ethereum'
  }

export {
  browserHeight,
  browserPadding,
  clusterWidth,
  categories,
  expandHandleWidth,
  moreThan1TagCategory,
  maxAddableNodes,
  currencies,
  maxSearchBreadth,
  maxSearchDepth
}
