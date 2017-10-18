var SummaryBox = function(htmlElement, app) {

    var self = this,

        currencyElements = jQuery(htmlElement + ' .currency'),

        getCurrencyElements = function(currency) {
            return jQuery(htmlElement + ' .currency' + '.' + currency);
        },

        renderCurrencyValues = function() {
            currencyElements.each(function() {
                var value = $(this).text();
                var currency = $(this).attr('class').replace("currency ", "");
                formattedCurrency = CurrencyUtils.formatCurrency(value, currency);
                $(this).text(formattedCurrency);
            });
        },

        showCurrency = function(activeCurrency) {
            currencyElements.hide();
            getCurrencyElements(activeCurrency).show();
        };

    renderCurrencyValues();

    showCurrency(app.getActiveCurrency());

    events.subscribe('/currency/switch', function(activeCurrency) {
        showCurrency(activeCurrency);
    });
}
