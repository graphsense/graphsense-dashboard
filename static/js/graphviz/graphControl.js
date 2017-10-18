var GraphControl = function(htmlElement) {

    var self = this,

        graphControl = jQuery(htmlElement),

        neighborSelector = graphControl.find('#neighbor-selector'),

        edgeSelector = graphControl.find('#edge-selector'),

        nodeSelector = graphControl.find('#node-selector'),

        nodeDownloadButton = graphControl.find('#node-download'),

        edgeDownloadButton = graphControl.find('#edge-download'),

        getSelectorValue = function(selector) {
            return selector.find("option:selected").val();
        },

        getMaxNeighborValue = function() {
            return getSelectorValue(neighborSelector);
        },

        getEdgeValueType = function() {
            return getSelectorValue(edgeSelector);
        }

        getNodeValueType = function() {
            return getSelectorValue(nodeSelector);
        };

    this.getMaxNeighborValue = getMaxNeighborValue;

    this.getEdgeValueType = getEdgeValueType;

    this.getNodeValueType = getNodeValueType;

    neighborSelector.on('change', function(){
        var selected = getMaxNeighborValue();
        events.publish('/graphControl/maxNeighborChanged', selected);
    });

    edgeSelector.on('change', function(){
        var selected = getEdgeValueType();
        events.publish('/graphControl/edgeValueChanged', selected);
    });

    nodeSelector.on('change', function(){
        var selected = getNodeValueType();
        events.publish('/graphControl/nodeValueChanged', selected);
    });

    nodeDownloadButton = nodeDownloadButton.on('click', function() {
        events.publish('graphControl/nodeDownloadClicked');
        nodeDownloadButton.blur();
    });

    edgeDownloadButton = edgeDownloadButton.on('click', function() {
        events.publish('graphControl/edgeDownloadClicked');
        edgeDownloadButton.blur();
    });

};
