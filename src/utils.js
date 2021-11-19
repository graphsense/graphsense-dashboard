import numeral from 'numeral'

export function firstToUpper (string) {
  return string.charAt(0).toUpperCase() + string.slice(1)
}

export function nbsp (string) {
  return string.replace(' ', '&nbsp;')
}

export function coinToSatoshi (value) {
  return value * 1e+8
}

export function satoshiToCoin (value) {
  return value / 1e+8
}

export function coinToWei (value) {
  return value * 1e+18
}

export function weiToCoin (value) {
  return value / 1e+18
}

function smallCurrency (keyspace) {
  if (keyspace === 'ETH') return 'wei'
  return 's'
}

function currencyFormat (keyspace, value) {
  const zeros = Math.max(Math.floor(Math.log10(Math.abs(value))) - 3, 0)
  const max = Math.max((keyspace === 'ETH' ? 18 : 8) - zeros, 2)
  return '1,000.[' + ('0'.repeat(max)) + ']'
}

function formatCoin (valueValue, currencyCode, { dontAppendCurrency, keyspace }) {
  keyspace = keyspace.toUpperCase()
  const value = keyspace === 'ETH' ? weiToCoin(valueValue) : satoshiToCoin(valueValue)
  if (value === 0) {
    return '0' + (!dontAppendCurrency ? ' ' + (keyspace || currencyCode).toUpperCase() : '')
  }
  if (Math.abs(value) < 0.0001) {
    return valueValue + (!dontAppendCurrency ? ' ' + smallCurrency(keyspace) : '')
  }
  return numeral(value).format(currencyFormat(keyspace, valueValue)) + (!dontAppendCurrency ? ' ' + (keyspace || currencyCode).toUpperCase() : '')
}

function formatFiat (value, currencyCode, { dontAppendCurrency }) {
  return numeral(value).format('1,000.[00]') + (!dontAppendCurrency ? ' ' + currencyCode.toUpperCase() : '')
}

export function formatCurrency (value, currencyCode, options) {
  const options_ = { dontAppendCurrency: false, keyspace: '', ...options }
  if (currencyCode === 'value') {
    return formatCoin(value.value, currencyCode, options_)
  } else {
    const fiat = value.fiat_values.filter(({ code, value }) => code.toLowerCase() === currencyCode.toLowerCase())[0]
    if (fiat !== null) {
      return formatFiat(fiat.value, currencyCode, options_)
    }
    return formatCoin(value.value, currencyCode, options_)
  }
}

export const nodesIdentical = (node1, node2) => node1.id == node2.id && node1.keyspace === node2.keyspace // eslint-disable-line eqeqeq

export const versionToInt = (version) => parseInt(version.replace('.', ''))
