const browserHeight = 220
const browserPadding = 1
const entityWidth = 190
const expandHandleWidth = 15
const maxAddableNodes = 100
const maxSearchDepth = 7
const maxSearchBreadth = 100

const searchlimit = 100
const prefixLength = 5
const labelPrefixLength = 3

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
  entityWidth,
  categories,
  expandHandleWidth,
  moreThan1TagCategory,
  maxAddableNodes,
  currencies,
  maxSearchBreadth,
  maxSearchDepth,
  searchlimit,
  prefixLength,
  labelPrefixLength

}
