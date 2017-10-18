jQuery(document).ready(function() {

    var app = getGraphSenseApp(),

        addressSummary = new SummaryBox('#summary', app),

        tabs = ['#currency_flows_tab'],

        activeCurrency = app.getActiveCurrency(),

        activate_tab_div = function(tab_id) {
          $(tab_id).addClass('active');
          for (var i=0; i < tabs.length; i++) {
            if(tabs[i] != tab_id) {
              deactivate_tab_div(tabs[i])
            }
          }
        },

        deactivate_tab_div = function(tab_id) {
          $(tab_id).removeClass('active');
        },

        add_tab_switch_handler = function() {
          $('#detail_tabs li').click(function (e) {
            e.preventDefault();
            var tab_id = $(this)[0].id;
            switch(tab_id) {
              // address graph activated
              case('currency_flows'):
                show_currency_flows();
                break;
              // input transaction table activated
              case('tx_address_graph'):
                show_tx_address_graph();
                break;
            }
            $(this).tab('show')
          });
        },

        render_currency_flows = function() {
          var tabElement = '#currency_flows_tab';
          var currencyElements = jQuery(tabElement + ' .currency');
          currencyElements.each(function() {
              var value = $(this).text();
              var currency = $(this).attr('class').replace("currency ", "");
              formattedCurrency = CurrencyUtils.formatCurrency(value, currency);
              $(this).text(formattedCurrency);
          });
        },

        showCurrencyFlows = function() {
          var tabElement = '#currency_flows_tab';
          var currencyElements = jQuery(tabElement + ' .currency');
          currencyElements.hide();
          jQuery(tabElement + ' .currency' + '.' + activeCurrency).show();

          activate_tab_div(tabElement);
        };

    add_tab_switch_handler();
    render_currency_flows();
    showCurrencyFlows();

    events.subscribe('/currency/switch', function(newCurrency) {
       activeCurrency = newCurrency;
       showCurrencyFlows();
    });

});
