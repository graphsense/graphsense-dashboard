jQuery( document ).ready(function() {

    var app = getGraphSenseApp(),

        clusterSummary = new SummaryBox('#summary', app),

        tabs = ['#cg_tab', '#addresses_tab', '#tag_tab'],

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

        show_cluster_graph = function() {
          if(d3.select("#graph-container").select("svg").empty()) {
            var requestURI = $SCRIPT_ROOT + '/cluster/' + cluster_id + '/egonet.json';
            $.getJSON(requestURI)
              .done(function( data ) {
                // create graph control
                var graphControlEl = document.getElementById('graph-control');
                var graphControl = new GraphControl(graphControlEl);
                var graph = new Graph(data.nodes, data.edges, data.focusNode);
                var targetElement = document.querySelector('#graph-container');
                var forceLayout = new ForceLayout(app, graphControl, targetElement, graph, requestURI);
              })
              .fail(function( jqxhr, textStatus, error ) {
                var err = textStatus + ", " + error;
                console.log( "Request Failed: " + err );
            });
          }
          activate_tab_div('#cg_tab')
        },

        show_address_table = function() {
          var request_uri = $SCRIPT_ROOT + '/cluster/' + cluster_id + '/addresses.json';
          $('#addresses_table').DataTable( {
            retrieve: true,
            searching: false,
            // buttons: ['excel', 'csv'],
            ajax: {
              url: request_uri,
              "dataSrc": ''
            },
            "columns": [
              {
                "data": "address",
                "render": function(data, type, full, meta) {
                  return '<a href="' + $SCRIPT_ROOT + '/address/' + data + '">' + data + '</a>';
                }
              },
              {
                "data": "firstTx",
                "render": function(data, type, full, meta) {
                  return '<a href="' + $SCRIPT_ROOT + '/tx/' + data.txHash + '">' + DateUtils.toDateTimeString(data.timestamp) + '</a>';
                }
              },
              {
                "data": "lastTx",
                "render": function(data, type, full, meta) {
                  return '<a href="' + $SCRIPT_ROOT + '/tx/' + data.txHash + '">' + DateUtils.toDateTimeString(data.timestamp) + '</a>';
                }
              },
              {
                "data": "totalReceived.satoshi",
                "visible": (app.getActiveCurrency() == 'btc'),
                "render": function(data, type, full, meta) {
                  return CurrencyUtils.formatCurrency(data, 'btc')
                }
              },
              {
                "data": "totalReceived.eur",
                "visible": (app.getActiveCurrency() == 'eur'),
                "render": function(data, type, full, meta) {
                  return CurrencyUtils.formatCurrency(data, 'eur')
                }
              },
              {
                "data": "totalReceived.usd",
                "visible": (app.getActiveCurrency() == 'usd'),
                "render": function(data, type, full, meta) {
                  return CurrencyUtils.formatCurrency(data, 'usd')
                }
              },
              {
                "data": "balance.satoshi",
                "visible": (app.getActiveCurrency() == 'btc'),
                "render": function(data, type, full, meta) {
                  return CurrencyUtils.formatCurrency(data, 'btc')
                }
              },
              {
                "data": "balance.eur",
                "visible": (app.getActiveCurrency() == 'eur'),
                "render": function(data, type, full, meta) {
                  return CurrencyUtils.formatCurrency(data, 'eur')
                }
              },
              {
                "data": "balance.usd",
                "visible": (app.getActiveCurrency() == 'usd'),
                "render": function(data, type, full, meta) {
                  return CurrencyUtils.formatCurrency(data, 'usd')
                }
              }
            ]
          } );
          activate_tab_div('#addresses_tab');
        },


        show_tags_table = function() {
          var request_uri = $SCRIPT_ROOT + '/cluster/' + cluster_id + '/tags.json';
          $('#tag_table').DataTable( {
            retrieve: true,
            paging: true,
            searching: true,
            ajax: {
              url: request_uri,
              "dataSrc": ''
            },
            "columns": [
              {
                "data": "address",
                "render": function(data, type, full, meta) {
                  return '<a href="' + $SCRIPT_ROOT + '/address/' + data + '">' + data + '</a>';
                }
              },
              { "data": "tag"},
              { "data": "tagUri"},
              { "data": "description"},
              { "data": "actorCategory"},
              { "data": "source",
                "render": function(data, type, full, meta) {
                  return full.sourceUri == "" ?
                    data : '<a href="' + full.sourceUri + '">' + data + '</a>'
                }
              },
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
              case('cg'):
                show_cluster_graph();
                break;
              // input transaction table activated
              case('addresses'):
                show_address_table();
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

    show_cluster_graph();
    add_tab_switch_handler();

    events.subscribe('/currency/switch', function(activeCurrency) {
        switchCurrencyColumns(activeCurrency);
    });

    events.subscribe('graphControl/edgeDownloadClicked', function() {
        window.location.href = $SCRIPT_ROOT + '/cluster/' + cluster_id + '/egonet/edges.csv';
    });

    events.subscribe('graphControl/nodeDownloadClicked', function() {
        window.location.href = $SCRIPT_ROOT + '/cluster/' + cluster_id + '/egonet/nodes.csv';
    });

});
