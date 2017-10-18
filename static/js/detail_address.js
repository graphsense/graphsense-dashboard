jQuery( document ).ready(function() {

    var app = getGraphSenseApp(),

        addressSummary = new SummaryBox('#summary', app),

        clusterSummary = new SummaryBox('#cluster-summary', app),

        tabs = ['#ag_tab', '#txs_tab', '#tag_tab'],

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

        show_address_graph = function() {
          if(d3.select("#graph-container").select("svg").empty()) {
            var requestURI = $SCRIPT_ROOT + '/address/' + address_id + '/egonet.json';
            $.getJSON(requestURI)
              .done(function( data ) {
                // create graph control
                var graphControlEl = document.getElementById('graph-control');
                var graphControl = new GraphControl(graphControlEl);
                var graph = new Graph(data.nodes, data.edges, data.focusNode);
                var targetElement = document.getElementById('graph-container');
                var forceLayout = new ForceLayout(app, graphControl, targetElement, graph, requestURI);
              })
              .fail(function( jqxhr, textStatus, error ) {
                var err = textStatus + ", " + error;
                console.log( "Request Failed: " + err );
            });
          }
          activate_tab_div('#ag_tab')
        },

        show_transactions_table = function() {
          var request_uri = $SCRIPT_ROOT + '/address/' + address_id + '/transactions.json';
          var table = $('#txs_table').DataTable( {
            retrieve: true,
            searching: false,
            ajax: {
              url: request_uri,
              "dataSrc": ''
            },
            "columns": [
              {
                "name": "entitylink",
                "data": "txHash",
                "render": function(data, type, full, meta) {
                  return '<a href="' + $SCRIPT_ROOT + '/tx/' + data + '">' + data + '</a>';
                }
              },
              {
                "name": "btc",
                "data": "value",
                "visible": (app.getActiveCurrency() == 'btc'),
                "render": function(data, type, full, meta) {
                  if (data.satoshi >= 0) {
                    color = 'green-text';
                  } else {
                    color = 'red-text'
                  }
                  span = '<span class=' + color + '>' + CurrencyUtils.formatCurrency(data.satoshi, 'btc') + '</span>';
                  return span;
                }
              },
              {
                "name": "eur",
                "data": "value",
                "visible": (app.getActiveCurrency() == 'eur'),
                "render": function(data, type, full, meta) {
                  if (data.eur >= 0) {
                    color = 'green-text';
                  } else {
                    color = 'red-text'
                  }
                  span = '<span class=' + color + '>' + CurrencyUtils.formatCurrency(data.eur, 'eur') + '</span>';
                  return span;
                }
              },
              {
                "name": "usd",
                "data": "value",
                "visible": (app.getActiveCurrency() == 'usd'),
                "render": function(data, type, full, meta) {
                  if (data.usd >= 0) {
                    color = 'green-text';
                  } else {
                    color = 'red-text'
                  }
                  span = '<span class=' + color + '>' + CurrencyUtils.formatCurrency(data.usd, 'usd') + '</span>';

                  // return btc_span + eur_span + usd_span;
                  return span;
                }
              },
              {
                "data": "height",
                "render": function(data, type, full, meta) {
                  return '<a href="' + $SCRIPT_ROOT + '/block/' + data + '">' + data + '</a>';
                }
              },
              {
                "data": "timestamp",
                "render": function(data, type, full, meta) {
                  return DateUtils.toDateTimeString(data);
                }
              }
            ]
          } );

          // datatables buttons
          var tableButtons = new $.fn.dataTable.Buttons( table, {
              buttons: [
                    {
                        extend: 'copy',
                        title: address_id,
                    },
                    {
                        extend: 'print',
                        title: "Address: " + address_id,
                    },
                    {
                        extend: 'csv',
                        title: address_id,
                    },
                ]
          });

          // Append datatables buttons to container if they do not exist
          if ( !$('#trx-table-buttons').children().length ) {
            tableButtons.container().appendTo('#trx-table-buttons');
          }

          // Add icons to generated datatables buttons
          $('.buttons-copy').addClass( "glyphicon glyphicon-copy" );
          $('.buttons-print').addClass( "glyphicon glyphicon-print" );
          $('.buttons-csv').addClass( "glyphicon glyphicon-export" );

          activate_tab_div('#txs_tab');
        },

        show_tags_table = function() {
          var request_uri = $SCRIPT_ROOT + '/address/' + address_id + '/tags.json';
          $('#tag_table').DataTable( {
            retrieve: true,
            paging: true,
            searching: true,
            ajax: {
              url: request_uri,
              "dataSrc": ''
            },
            "columns": [
              { "data": "tag"},
              { "data": "type"},
              { "data": "tagUri"},
              { "data": "description"},
              { "data": "actorCategory"},
              { "data": "source",
                "render": function(data, type, full, meta) {
                  return full.sourceUri == "" ?
                    data : '<a href="' + full.sourceUri + '">' + data + '</a>'
                }
              },
              //{ "data": "sourceUri"},
              { "data": "timestamp",
                "render": function(data, type, full, meta) {
                  return DateUtils.toDateTimeString(data);
                }
              }
            ]
          } );
          activate_tab_div('#tag_tab');
        },

        add_tab_switch_handler = function() {
          $('#detail_tabs li').click(function (e) {
            e.preventDefault();
            var tab_id = $(this)[0].id;
            switch(tab_id) {
              // address graph activated
              case('ag'):
                show_address_graph();
                break;
              // input transaction table activated
              case('txs'):
                show_transactions_table();
                break;
              // output transaction table activated
              case('tags'):
                show_tags_table();
                break;
            }
            $(this).tab('show')
          });
        },

        switchCurrencyColumns = function(activeCurrency) {
            var tables = $.fn.dataTable.tables({visible: false, api: true});
            tables.columns('.currencyColumn').visible(false);
            tables.columns('.currencyColumn.' + activeCurrency).visible(true);
        };

    show_address_graph();
    add_tab_switch_handler();

    events.subscribe('/currency/switch', function(activeCurrency) {
        switchCurrencyColumns(activeCurrency);
    });

    events.subscribe('graphControl/edgeDownloadClicked', function() {
        window.location.href = $SCRIPT_ROOT + '/address/' + address_id + '/egonet/edges.csv';
    });

    events.subscribe('graphControl/nodeDownloadClicked', function() {
        window.location.href = $SCRIPT_ROOT + '/address/' + address_id + '/egonet/nodes.csv';
    });

});
