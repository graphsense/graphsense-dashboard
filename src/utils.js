function firstToUpper (string) {
  return string.charAt(0).toUpperCase() + string.slice(1)
}

function _formatCurrency (n, c, d, t) {
  console.log('formatCurrency', n, c)
  c = isNaN(c = Math.abs(c)) ? 2 : c
  d = d === undefined ? '.' : d
  t = t === undefined ? ',' : t
  let s = n < 0 ? '-' : ''
  let i = String(parseInt(n = Math.abs(Number(n) || 0).toFixed(c)))
  let j = i.length
  j = j > 3 ? j % 3 : 0
  return s + (j ? i.substr(0, j) + t : '') + i.substr(j).replace(/(\d{3})(?=\d)/g, '$1' + t) + (c ? d + Math.abs(n - i).toFixed(c).slice(2) : '')
}

function formatBTC (satoshiValue, currencyCode) {
  let value = satoshiValue / 10000 / 10000
  if (value > 0 && value < 0.0001) {
    return satoshiValue + ' s'
  }
  return _formatCurrency(value, 4) + ' ' + currencyCode.toUpperCase()
}

function formatFiat (value, currencyCode) {
  return _formatCurrency(value, 2) + ' ' + currencyCode.toUpperCase()
}

function formatCurrency (value, currencyCode) {
  if (currencyCode === 'btc') {
    return formatBTC(value, currencyCode)
  } else {
    return formatFiat(value, currencyCode)
  }
}

export {firstToUpper, formatCurrency}
