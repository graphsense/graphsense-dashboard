var GraphSense = {};

var App = function() {

    var self = this,

        currencySelector = new CurrencySelector('#currency-switcher'),

        getActiveCurrency = function() {
            return currencySelector.getActiveCurrencyCode();
        };

    this.getActiveCurrency = getActiveCurrency;

};


var getGraphSenseApp = function() {
    if(! GraphSense.App) {
        GraphSense.App = new App();
    }
    return GraphSense.App;
}
