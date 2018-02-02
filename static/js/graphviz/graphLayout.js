var ForceLayout = function(app, graphControl, targetElement, graph, requestURI) {

    var self = this,

        width = targetElement.offsetWidth,

        height = width / 2,

        nodeDistance = 200,

        focusNodePosX = height / 2,

        focusNodePosY = width / 2,

        minLinkStrokeWidth = 2,

        maxLinkStrokeWidth = 5,

        minNodeRadius = 20,

        maxNodeRadius = 35,

        expansionMarkerRadius = 5,

        activeCurrency = app.getActiveCurrency(),

        maxNodeNeighbors = graphControl.getMaxNeighborValue(),

        edgeValueType = graphControl.getEdgeValueType(),

        nodeValueType = graphControl.getNodeValueType(),

        simulation = d3.forceSimulation()
                       .force("link", d3.forceLink()
                                        .id(function(d) { return d.id; })
                                        .distance(nodeDistance).strength(1.5))
                       .force("charge", d3.forceManyBody().strength(function(d) {return -4 * d.radius}))
                       .force("center", d3.forceCenter(width / 2, height / 2)),

        svg = d3.select(targetElement).append('svg')
            .attr("width", width)
            .attr("height", height),

        linkGroup = svg.append("g")
                       .attr("class", "links"),

        nodeGroup = svg.append("g")
                       .attr("class", "nodes"),

        linkLabelGroup = svg.append("g")
                            .attr("class", "linkLabels"),

        getCurrencyValue = function(dataNode, currency) {
            switch(currency) {
                case('eur'):
                    return(dataNode['eur']);
                break;

                case('usd'):
                    return(dataNode['usd']);
                break;

                case('btc'):
                    return(dataNode['satoshi']);
                break;
            }
        },

        getNodeValue = function(node, currency) {
            if(nodeValueType === "received") {
                return node.received;
            } else if(nodeValueType === "balance") {
                return node.balance;
            } else {
                throw "Cannot retrieve node value; unknown property " + nodeValueType;
            }
        },

        getNodeRadius = function(node) {
            var nodeRadiusScale = d3.scaleLog().clamp(true)
                .domain([d3.min(graph.nodes, function(n) {return getNodeValue(n, 'btc') + 1;}),
                         d3.max(graph.nodes, function(n) {return getNodeValue(n, 'btc') + 1;})])
                .range([minNodeRadius, maxNodeRadius]).base(2);

            return nodeRadiusScale(getNodeValue(node, 'btc'));

        },

        getLinkValue = function(link) {
            if(edgeValueType === "noTransactions") {
                return link.transactions;
            } else if(edgeValueType === "estimatedValue") {
                return getCurrencyValue(link.estimatedValue, activeCurrency);
            } else {
                throw "Cannot retrieve link value; unknown property " + edgeValueType;
            }
        },

        getLinkStrokeWidth = function(link) {

            var linkStrokeWidthScale = d3.scaleLog().clamp(true)
                .domain([d3.min(graph.links, function(l) {return getLinkValue(l);}),
                         d3.max(graph.links, function(l) {return getLinkValue(l);})])
                .range([minLinkStrokeWidth, maxLinkStrokeWidth]).base(2);

            return linkStrokeWidthScale(link);

        },

        init = function() {

            svg.append("defs").append("marker")
                .attr("id", "arrow")
                .attr("viewBox", "0 0 20 20")
                .attr("refX", 10)
                .attr("refY", 3)
                .attr("markerWidth", 10)
                .attr("markerHeight", 10)
                .attr("orient", "auto")
                .append("path")
                    .attr("class", "arrow")
                    .attr("d", "M0,0 L0,6 L9,3 z");
        },

        update = function() {

            // Set node radius
            graph.nodes.forEach(function(node) {
                node.radius = getNodeRadius(node);
            });

            // Redefine and restart simulation
            simulation.nodes(graph.nodes)
                      .on("tick", ticked);

            simulation.force("link")
                      .links(graph.links);

            // Update nodes
            var node = nodeGroup.selectAll("g")
                .remove()
                .exit()
                .data(simulation.nodes(), function(d) { return d.id; });


            // Enter any new nodes
            var nodeEnter = node.enter().append("g")
                .on("mouseover", nodeMouseover)
                .on("mouseout", nodeMouseout)
                .call(d3.drag()
                    .on("start", dragstarted)
                    .on("drag", dragged)
                    .on("end", dragended));


            nodeEnter.append("circle")
                .attr("id", function(d) {return d.id;})
                .attr("class", function(d) {
                    var classString = d.nodeType;
                    if(d.category == "Explicit") {
                        classString += " tagged";
                    } else if(d.category == "Implicit") {
                        classString += " implicitlyTagged";
                    } else {
                        classString += " anonymous";
                    }
                    if(d.id == graph.focusNodeId) {
                        classString = classString + " focusnode";
                    }
                    return classString;
                })
                .attr("r", function(d) { return d.radius; });

            nodeEnter.append("circle")
                .on("click", function(n) {markerClicked(n, true)})
                .attr("id", function(d) {return d.id + "_left-marker"})
                .attr("class", "marker")
                .attr("r", expansionMarkerRadius)
                .attr("visibility", "hidden")
                .attr("cx", function(d) {return - d.radius;});

            nodeEnter.append("circle")
                .on("click", function(n) {markerClicked(n, false)})
                .attr("id", function(d) {return d.id + "_right-marker"})
                .attr("class", "marker")
                .attr("r", expansionMarkerRadius)
                .attr("visibility", "hidden")
                .attr("cx", function(d) {return d.radius;});

            nodeEnter.append("title")
                .text(function(d) { return d.id;});

            nodeEnter.append("a")
                .attr("xlink:href", function(d) {
                    return $SCRIPT_ROOT + '/' + d.nodeType + '/' + d.id;
                })
                .append("text")
                .style("pointer-events", "all")
                .attr("text-anchor", "middle")
                .attr("y", 5)
                .text(function(d) {
                    var label = d.id;
                    if(d.radius >= 15) {
                        return label.toString().substring(0,3) + "...";
                    } else {
                        return "";
                    }

                });

            node = nodeEnter.merge(node);

            // Update links
            var link = linkGroup.selectAll("path")
                .remove()
                .exit()
                .data(simulation.force("link").links(), function(d) { return d.source.id + "-" + d.target.id; });

            // Enter any new links
            var linkEnter = link.enter()
                .append("path")
                .attr("d", function(d) { return 'M '+ d.source.x + ' ' + d.source.y + ' L ' + d.target.x + ' ' + d.target.y; })
                .attr("id", function(d) { return d.source.id + "-" + d.target.id; })
                .attr("marker-end", "url(#arrow)")
                .attr("stroke-width", function(l) {
                    var linkValue = getLinkValue(l);
                    var strokeWidth = getLinkStrokeWidth(linkValue);
                    return strokeWidth;
                });

            link = linkEnter.merge(link);


            // Redraw link labels
            var linkLabel = linkLabelGroup.selectAll("text")
                .remove()
                .exit()
                .data(simulation.force("link").links());

            var linkLabelEnter = linkLabel.enter()
                .append("text")
                .attr("dy", -5)
                .attr("text-anchor", "middle")
                .attr("font-size", 10);

            linkLabelEnter.append("textPath")
                .attr("xlink:href", function(d) { return "#" + d.source.id + "-" + d.target.id; })
                .attr("startOffset", "50%")
                .text(function(l) {
                    var linkValue = getLinkValue(l);
                    if(edgeValueType == 'estimatedValue') {
                        linkValue = CurrencyUtils.formatCurrency(linkValue, activeCurrency);
                    }
                    return linkValue;
                })
                .style("pointer-events", "none");

            simulation.alpha(0.5).restart();


            function ticked() {
                var k = 10 * simulation.alpha();

                node.attr("transform", function(n) {
                    if (n.focusNodeDistance && !n.fx)
                        n.x = n.x + k * n.focusNodeDistance;
                    var b = n.radius + 2;
                    if (n.x < b) n.x = b;
                    if (n.y < b) n.y = b;
                    if (n.x > width - b) n.x = width - b;
                    if (n.y > height - b) n.y = height - b;
                    return "translate(" + n.x + "," + n.y + ")";
                });

                link.attr("d", function(l) {
                    var diffX = l.target.x - l.source.x;
                    var diffY = l.target.y - l.source.y;
                    var len = Math.sqrt(diffX * diffX + diffY * diffY);
                    var targetX = l.target.x - (diffX / len * l.target.radius);
                    var targetY = l.target.y - (diffY / len * l.target.radius);
                    var sourceX = l.source.x + (diffX / len * l.source.radius);
                    var sourceY = l.source.y + (diffY / len * l.source.radius);
                    return 'M ' + sourceX + ' ' + sourceY + ' L ' + targetX + ' ' + targetY;
                });

              };

        },

        dragstarted = function(d) {
            if (!d3.event.active) simulation.alphaTarget(0.5).restart();
            d.fx = d.x;
            d.fy = d.y;
        },

        dragged = function(d) {
            d.fx = d3.event.x;
            d.fy = d3.event.y;
        },

        dragended = function(d) {
            if (!d3.event.active) simulation.alphaTarget(0);
            d.fx = null;
            d.fy = null;
        },

        nodeMouseover = function(node) {
            node.fx = node.x;
            node.fy = node.y;
            var markerSVGElement = document.querySelector("circle[id='" + node.id + "_left-marker']");
            markerSVGElement.setAttribute("visibility", "visible");
            markerSVGElement = document.querySelector("circle[id='" + node.id + "_right-marker']");
            markerSVGElement.setAttribute("visibility", "visible");

        },

        nodeMouseout = function(node) {
            node.fx = null;
            node.fy = null;
            var markerSVGElement = document.querySelector("circle[id='" + node.id + "_left-marker']");
            markerSVGElement.setAttribute("visibility", "hidden");
            markerSVGElement = document.querySelector("circle[id='" + node.id + "_right-marker']");
            markerSVGElement.setAttribute("visibility", "hidden");
        },

        markerClicked = function(node, incoming) {
            simulation.stop();

            var requestURI = $SCRIPT_ROOT + '/' + node.nodeType + '/' + node.id + '/egonet.json';

            if(graph.collapseNode(node.id, incoming)) {
                update();
            } else {
                $.getJSON(requestURI, {direction: (incoming ? "in" : "out"), limit: maxNodeNeighbors})
                  .done(function(data) {
                    graph.mergeGraph(data.nodes, data.edges);
                    update();
                  })
                  .fail(function(jqxhr, textStatus, error ) {
                    var err = textStatus + ", " + error;
                    console.log( "Request Failed: " + err );
                });
            }

        };

    init();
    update();


    // EVENT SUBSCRIPTIONS

    events.subscribe('/currency/switch', function(newCurrency) {
        activeCurrency = newCurrency;
        update();
    });

    events.subscribe('/graphControl/maxNeighborChanged', function(maxNeighbors) {
        maxNodeNeighbors = maxNeighbors;
    });

    events.subscribe('/graphControl/edgeValueChanged', function(newEdgeValueType) {
        edgeValueType = newEdgeValueType;
        update();
    });

    events.subscribe('/graphControl/nodeValueChanged', function(newNodeValueType) {
        nodeValueType = newNodeValueType;
        update();
    });

};
