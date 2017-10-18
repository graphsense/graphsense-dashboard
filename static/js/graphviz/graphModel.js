// http://blog.benoitvallon.com/data-structures-in-javascript/the-graph-data-structure/

var Graph = function(nodes, links, focusNodeId) {

    var self = this,

        // returns a specific node
        _findNode = function(nodeId) {
            for(var i = 0; i < nodes.length; i++){
                if(nodes[i].id == nodeId){
                    return nodes[i];
                }
            }
            return null;
        },

        // returns the array index of a specific node
        _findNodeIndex = function(nodeId) {
            for(var i = 0; i < nodes.length; i++){
                if(nodes[i].id == nodeId){
                    return i;
                }
            }
            return null;
        },

        // returns a node's direct children
        _adjacentNodes = function(nodeId, incoming) {
            var children = [];
            links.forEach(function(link){
                if (incoming) {
                    if(nodeId === link.targetId)
                        children.push(link.sourceId);
                } else {
                    if(nodeId === link.sourceId)
                        children.push(link.targetId);
                }
            });
            //console.log("Children of", nodeId, children);
            return children;
        },

        // traverses the graph DF and returns visited vertices and depth
        _traverseDFS = function(nodeId, depth, visited, incoming, fn) {
            // console.log("Traversing node", nodeId);
            visited[nodeId] = true;

            var nodes = _adjacentNodes(nodeId, incoming);
            nodes.forEach(function(node, i) {
                 if(!visited[node]) {
                     _traverseDFS(node, depth + 1, visited, incoming, fn);
                     fn(node, depth);
                 }
            });
        },

        // re-computes focus node distances
        _computeFocusNodeDistances = function() {
            // console.log("Computing focus node distances");
            // assign depth 0 to foucsNode
            _findNode(focusNodeId).focusNodeDistance = 0;

            // traverseChildren
            _traverseDFS(focusNodeId, 1, [], false, function(nodeId, depth){
                _findNode(nodeId).focusNodeDistance = depth;
            });
            // traverseParents
            _traverseDFS(focusNodeId, 1, [], true, function(nodeId, depth){
                _findNode(nodeId).focusNodeDistance = depth * -1;
            });
        },

        // initalize graph with additional properties
        _initialize = function() {
            if(focusNodeId === null){
                throw "Focus node not specified";
            }

            // check nodes for duplicats
            var nodeIDs = nodes.map(function(d) {return d.id; });
            if(new Set(nodeIDs).size < nodeIDs.length) {
                throw "Graph contains duplicate nods";
            }

            links.forEach(function(link){
                // check if source node is defined
                if(_findNode(link.source) === null) {
                    throw "Node " + link.source + " not specified."
                }
                link.sourceId = link.source;
                // check if target node is defined
                if(_findNode(link.target) === null) {
                    throw "Node " + link.target + " not specified."
                }
                link.targetId = link.target;
            });
            _computeFocusNodeDistances();
        },

        // removes links involving a given node id from graph
        _removeNodeLinks = function(nodeId) {
            for(var i = links.length - 1; i >= 0; i--) {
                if(links[i].sourceId === nodeId || links[i].targetId === nodeId) {
                    links.splice(i, 1);
                }
            }
        },

        // removes links involving a given node id from graph
        _isPartOfCycle = function(linkId) {
            var startNode = links[linkId].sourceId;
            var endNode = links[linkId].targetId;
            var fringe = new Set([startNode]);
            var visited = new Set();
            while(fringe.size != 0 && !visited.has(endNode)) {
                var nextFringe = new Set();
                for(var nodeId of fringe) {
                    visited.add(nodeId);
                    for(var i = 0; i < links.length; i++) {
                        if (i != linkId) {
                            var nextNode = null;
                            if (links[i].sourceId === nodeId)
                                nextNode = links[i].targetId;
                            else if (links[i].targetId === nodeId)
                                nextNode = links[i].sourceId;
                            if (nextNode && !visited.has(nextNode))
                                nextFringe.add(nextNode);
                        }

                    }
                }
                fringe = nextFringe;
            }
            if (visited.has(endNode)) return true;
            else return false;
        },

        // PUBLIC FUNCTIONS

        // add vertex to graph, if it does not exist
        addNode = function(node) {
            if(_findNode(node.id) == null) {
                nodes.push(node);
            }
        },

        // add edge to graph
        addLink = function(link) {
            link.sourceId = link.source;
            link.targetId = link.target;
            links.push(link);
            _computeFocusNodeDistances();
        },

        // removes node with given id from graph
        removeNode = function(nodeId) {
            var index = _findNodeIndex(nodeId);
            if(index != null) {
                nodes.splice(index, 1);
            }
            _removeNodeLinks(nodeId);
        },

        collapseNode = function(nodeId, incoming) {
            var modified = false;
            _adjacentNodes(nodeId, incoming).forEach(function(node) {
                if (_adjacentNodes(node, true).length + _adjacentNodes(node, false).length == 1) {
                    removeNode(node);
                    modified = true;
                }
            });
            for(var i = links.length - 1; i >= 0; i--) {
                if((incoming && links[i].targetId == nodeId) ||
                   (!incoming && links[i].sourceId == nodeId)) {
                    if(_isPartOfCycle(i)) {
                        links.splice(i, 1);
                        modified = true;
                    }
                }
            }
            return modified;
        },

        mergeGraph = function(nodes, links) {
            nodes.forEach(function(node) {
                addNode(node);
            });
            links.forEach(function(link) {
                addLink(link);
            });
        },

        // pretty print graph nodes and links
        printGraph = function() {
            console.log("Nodes:")
            nodes.forEach(function(n){console.log(JSON.stringify(n))});
            console.log("Edges:")
            edges.forEach(function(e){console.log(JSON.stringify(e))});
        };




    // Initialize graph data structure
    _initialize();

    // Public functions
    this.nodes = nodes;
    this.links = links;
    this.focusNodeId = focusNodeId;
    this.printGraph = printGraph;
    this.addNode = addNode;
    this.addLink = addLink;
    this.removeNode = removeNode;
    this.collapseNode = collapseNode;
    this.mergeGraph = mergeGraph;

};
