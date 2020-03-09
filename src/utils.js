import numeral from 'numeral'

function firstToUpper (string) {
  return string.charAt(0).toUpperCase() + string.slice(1)
}

function formatBTC (valueValue, currencyCode, { dontAppendCurrency, keyspace }) {
  const value = valueValue / 10000 / 10000
  if (value === 0) {
    return '0 ' + (keyspace || currencyCode).toUpperCase()
  }
  if (Math.abs(value) < 0.0001) {
    return valueValue + (!dontAppendCurrency ? ' s' : '')
  }
  return numeral(value).format('1,000.[0000]') + (!dontAppendCurrency ? ' ' + (keyspace || currencyCode).toUpperCase() : '')
}

function formatFiat (value, currencyCode, { dontAppendCurrency }) {
  return numeral(value).format('1,000.[00]') + (!dontAppendCurrency ? ' ' + currencyCode.toUpperCase() : '')
}

function formatCurrency (value, currencyCode, options) {
  const options_ = { dontAppendCurrency: false, keyspace: 'btc', ...options }
  if (currencyCode === 'value') {
    return formatBTC(value, currencyCode, options_)
  } else {
    return formatFiat(value, currencyCode, options_)
  }
}

export { firstToUpper, formatCurrency }
