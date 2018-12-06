function firstToUpper (string) {
  return string.charAt(0).toUpperCase() + string.slice(1)
}

function _formatCurrency (n, c, d, t) {
  c = isNaN(c = Math.abs(c)) ? 2 : c
  d = d === undefined ? '.' : d
  t = t === undefined ? ',' : t
  let s = n < 0 ? '-' : ''
  let i = String(parseInt(n = Math.abs(Number(n) || 0).toFixed(c)))
  let j = i.length
  j = j > 3 ? j % 3 : 0
  return s + (j ? i.substr(0, j) + t : '') + i.substr(j).replace(/(\d{3})(?=\d)/g, '$1' + t) + (c ? d + Math.abs(n - i).toFixed(c).slice(2) : '')
}

function formatBTC (satoshiValue, currencyCode, {dontAppendCurrency, keyspace}) {
  let value = satoshiValue / 10000 / 10000
  if (value === 0) {
    return '0 ' + (keyspace || currencyCode).toUpperCase()
  }
  if (value < 0.0001) {
    return satoshiValue + (!dontAppendCurrency ? ' s' : '')
  }
  return _formatCurrency(value, 4) + (!dontAppendCurrency ? ' ' + (keyspace || currencyCode).toUpperCase() : '')
}

function formatFiat (value, currencyCode, {dontAppendCurrency}) {
  return _formatCurrency(value, 2) + (!dontAppendCurrency ? ' ' + currencyCode.toUpperCase() : '')
}

function formatCurrency (value, currencyCode, options) {
  let options_ = {dontAppendCurrency: false, keyspace: 'btc', ...options}
  if (currencyCode === 'satoshi') {
    return formatBTC(value, currencyCode, options_)
  } else {
    return formatFiat(value, currencyCode, options_)
  }
}

export {firstToUpper, formatCurrency}
