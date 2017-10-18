jQuery( document ).ready(function() {

    var app = getGraphSenseApp(),

        blockSummary = new SummaryBox('#summary', app),

        tabs = ['#txs_tab'],

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

        show_transactions_table = function() {
          var request_uri = $SCRIPT_ROOT + '/block/' + height + '/transactions.json';
          $('#txs_table').DataTable( {
            retrieve: true,
            searching: false,
            ajax: {
              url: request_uri,
              "dataSrc": 'txs'
            },
            "columns": [
              {
                "name": "tx_hash",
                "data": "txHash",
                "render": function(data, type, full, meta) {
                  return '<a href="' + $SCRIPT_ROOT + '/tx/' + data + '">' + data + '</a>';
                }
              },
              {
                "name": "no_inputs",
                "data": "noInputs"
              },
              {
                "name": "no_outputs",
                "data": "noOutputs"
              },
              {
                "name": "btc",
                "data": "fee.satoshi",
                "visible": (app.getActiveCurrency() == 'btc'),
                "render": function(data, type, full, meta) {
                  return CurrencyUtils.formatCurrency(data, 'btc');
                }
              },
              {
                "name": "eur",
                "data": "fee.eur",
                "visible": (app.getActiveCurrency() == 'eur'),
                "render": function(data, type, full, meta) {
                  return CurrencyUtils.formatCurrency(data, 'eur');
                }
              },
              {
                "name": "usd",
                "data": "fee.usd",
                "visible": (app.getActiveCurrency() == 'usd'),
                "render": function(data, type, full, meta) {
                  return CurrencyUtils.formatCurrency(data, 'usd');
                }
              },
              {
                "name": "btc",
                "data": "totalValue.satoshi",
                "visible": (app.getActiveCurrency() == 'btc'),
                "render": function(data, type, full, meta) {
                  return CurrencyUtils.formatCurrency(data, 'btc');
                }
              },
              {
                "name": "eur",
                "data": "totalValue.eur",
                "visible": (app.getActiveCurrency() == 'eur'),
                "render": function(data, type, full, meta) {
                  return CurrencyUtils.formatCurrency(data, 'eur');
                }
              },
              {
                "name": "usd",
                "data": "totalValue.usd",
                "visible": (app.getActiveCurrency() == 'usd'),
                "render": function(data, type, full, meta) {
                  return CurrencyUtils.formatCurrency(data, 'usd');
                }
              }
            ]
          } );
          activate_tab_div('#txs_tab');
        },

        add_tab_switch_handler = function() {
          $('#detail_tabs li').click(function (e) {
            e.preventDefault();
            show_transactions_table();
            $(this).tab('show')
          });
        },

        switchCurrencyColumns = function(activeCurrency) {
            var tables = $.fn.dataTable.tables({visible: false, api: true});
            tables.columns('.currencyColumn').visible(false);
            tables.columns('.currencyColumn.' + activeCurrency).visible(true);
        };

    show_transactions_table();
    add_tab_switch_handler();

    events.subscribe('/currency/switch', function(activeCurrency) {
        switchCurrencyColumns(activeCurrency);
    });

});
