var DEFAULT_CODE = 'btc';

var CURRENCY_CODES = ['btc', 'eur', 'usd'];

var CurrencySelector = function(htmlElement){

    var self = this,

        selector = jQuery('#currency-switcher'),

        activeCurrencyCode = null,

        getActiveCurrencyCode = function() {
            if(activeCurrencyCode === null) {
                if(typeof(Storage) != "undefined") {
                    activeCurrencyCode = localStorage.getItem("activeCurrencyCode");
                }
                if(activeCurrencyCode === null) {
                    activeCurrencyCode = DEFAULT_CODE;
                }
            }
            return activeCurrencyCode;
        },

        init = function() {
            CURRENCY_CODES.forEach(function(code) {
                selector.append($('<option>', {
                    value: code,
                    text: code.toUpperCase()
                }));
            });
            selector.val(getActiveCurrencyCode());
        },

        setActiveCurrencyCode = function(currencyCode) {
            if(typeof(Storage) != "undefined") {
                localStorage.setItem("activeCurrencyCode", currencyCode);
            }
            activeCurrencyCode = currencyCode;
        },

        onChanged = function() {
            var selectedCurrency = selector.val();
            setActiveCurrencyCode(selectedCurrency);
            events.publish('/currency/switch', selectedCurrency);
        };

    init();

    selector.on('change', onChanged);

    this.getActiveCurrencyCode = getActiveCurrencyCode;

};


var CurrencyUtils = (function(){

    var formatCurrency = function(n, c, d, t){
        var c = isNaN(c = Math.abs(c)) ? 2 : c,
            d = d == undefined ? "." : d,
            t = t == undefined ? "," : t,
            s = n < 0 ? "-" : "",
            i = String(parseInt(n = Math.abs(Number(n) || 0).toFixed(c))),
            j = (j = i.length) > 3 ? j % 3 : 0;
        return s + (j ? i.substr(0, j) + t : "") + i.substr(j).replace(/(\d{3})(?=\d)/g, "$1" + t) + (c ? d + Math.abs(n - i).toFixed(c).slice(2) : "");
    };

    var formatBTC = function(satoshiValue, currencyCode) {
        value = satoshiValue / 10000 / 10000;
        if(value > 0 && value < 0.0001) {
            return satoshiValue + ' s'
        }
        return formatCurrency(value, 4) + ' ' + currencyCode.toUpperCase();
    };

    var formatFiat = function(value, currencyCode) {
        return formatCurrency(value, 2) + ' ' + currencyCode.toUpperCase();
    };

    return {

        formatCurrency: function(value, currencyCode){
            if(currencyCode == 'btc') {
                return formatBTC(value, currencyCode);
            } else {
                return formatFiat(value, currencyCode);
            }
        }
    };

})();
