import numeral from 'numeral'

export function firstToUpper (string) {
  return string.charAt(0).toUpperCase() + string.slice(1)
}

export function nbsp (string) {
  return string.replace(' ', '&nbsp;')
}

export function coinToSatoshi (value) {
  return value * 10000 * 10000
}

export function satoshiToCoin (value) {
  return value / 10000 / 10000
}

export function weiToCoin (value) {
  return value / 10e+18
}

function smallCurrency (keyspace) {
  if (keyspace === 'ETH') return 'wei'
  return 's'
}

function currencyFormat (keyspace) {
  if (keyspace === 'ETH') return '1,000.[000000000000000000]'
  return '1,000.[00000000]'
}

function formatCoin (valueValue, currencyCode, { dontAppendCurrency, keyspace }) {
  const value = keyspace === 'ETH' ? weiToCoin(valueValue) : satoshiToCoin(valueValue)
  if (value === 0) {
    return '0 ' + (keyspace || currencyCode).toUpperCase()
  }
  if (Math.abs(value) < 0.0001) {
    return valueValue + (!dontAppendCurrency ? ' ' + smallCurrency(keyspace) : '')
  }
  return numeral(value).format(currencyFormat(keyspace)) + (!dontAppendCurrency ? ' ' + (keyspace || currencyCode).toUpperCase() : '')
}

function formatFiat (value, currencyCode, { dontAppendCurrency }) {
  return numeral(value).format('1,000.[00]') + (!dontAppendCurrency ? ' ' + currencyCode.toUpperCase() : '')
}

export function formatCurrency (value, currencyCode, options) {
  const options_ = { dontAppendCurrency: false, keyspace: '', ...options }
  if (currencyCode === 'value') {
    return formatCoin(value, currencyCode, options_)
  } else {
    return formatFiat(value, currencyCode, options_)
  }
}

export const nodesIdentical = (node1, node2) => node1.id == node2.id && node1.keyspace === node2.keyspace // eslint-disable-line eqeqeq

export const versionToInt = (version) => parseInt(version.replace('.', ''))
