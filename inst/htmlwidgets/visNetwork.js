// Add shim for Function.prototype.bind() from:
// https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Function/bind#Compatibility
// for fix some RStudio viewer bug (Desktop / windows)
if (!Function.prototype.bind) {
  Function.prototype.bind = function (oThis) {
    if (typeof this !== "function") {
    // closest thing possible to the ECMAScript 5 internal IsCallable function
      throw new TypeError("Function.prototype.bind - what is trying to be bound is not callable");
    }

    var aArgs = Array.prototype.slice.call(arguments, 1),
        fToBind = this,
        fNOP = function () {},
        fBound = function () {
            return fToBind.apply(this instanceof fNOP && oThis
                                               ? this
                                               : oThis,
                                             aArgs.concat(Array.prototype.slice.call(arguments)));
        };

    fNOP.prototype = this.prototype;
    fBound.prototype = new fNOP();

    return fBound;
  };
}

HTMLWidgets.widget({

  name: 'visNetwork',

  type: 'output',

  initialize: function(el, width, height) {

    return {
    }

  },

  renderValue: function(el, x, instance) {
    //legend  
    if(x.groups && x.legend){
      var legend = document.createElement('div');
      legend.id = "legend"
      legend.setAttribute('style', 'width:100%;height:50px;');
      document.getElementById(el.id).appendChild(legend);
      
      var legendnodes = new vis.DataSet();
      
      var mynetwork = document.getElementById('legend');
      var lx = - mynetwork.clientWidth / 2 + 50;
      var ly = - mynetwork.clientWidth / 2 + 50;
      var step = 70;
      for (g = 0; g < x.groups.length; g++){
        legendnodes.add({id: g, x : lx+g*step, y : ly, label: x.groups[g], group: x.groups[g], value: 1, mass:0});
      }
      
      var datalegend = {
        nodes: legendnodes, 
        edges: null
      };
      
      var optionslegend = {
        dragNetwork: false,
        dragNodes: false,
        selectable: false
      }
      
      optionslegend.groups = x.options.groups
      instance.legend = new vis.Network(document.getElementById("legend"), datalegend, optionslegend);
      
    }
    
    // network
    var nodes = new vis.DataSet();
    var edges = new vis.DataSet();

    nodes.add(x.nodes);
    edges.add(x.edges);
    
    var data = {
      nodes: nodes,
      edges: edges
    };

    var options = x.options

    var graph = document.createElement('div');
    graph.id = "graph"
    graph.setAttribute('style', 'width:100%;height:100%;');
    document.getElementById(el.id).appendChild(graph);
      
    // Custom data manipualtion http://visjs.org/examples/network/21_data_manipulation.html
    if(x.options.dataManipulation){

      var style = document.createElement('style');
      style.type = 'text/css';
      style.appendChild(document.createTextNode(x.datacss));
      document.getElementsByTagName("head")[0].appendChild(style);

      var div = document.createElement('div');
      div.id = 'network-popUp';

      div.innerHTML = '<span id="operation">node</span> <br>\
        <table style="margin:auto;"><tr>\
        <td>id</td><td><input id="node-id" value="new value"></td>\
        </tr>\
        <tr>\
        <td>label</td><td><input id="node-label" value="new value"> </td>\
        </tr></table>\
        <input type="button" value="save" id="saveButton"></button>\
        <input type="button" value="cancel" id="cancelButton"></button>';

      document.getElementById(el.id).appendChild(div);

      options.onAdd = function(data,callback) {
          var span = document.getElementById('operation');
          var idInput = document.getElementById('node-id');
          var labelInput = document.getElementById('node-label');
          var saveButton = document.getElementById('saveButton');
          var cancelButton = document.getElementById('cancelButton');
          var div = document.getElementById('network-popUp');
          span.innerHTML = 'Add Node';
          idInput.value = data.id;
          labelInput.value = data.label;
          saveButton.onclick = saveData.bind(this,data,callback);
          cancelButton.onclick = clearPopUp.bind();
          div.style.display = 'block';
      }

      options.onEdit = function(data,callback) {
          var span = document.getElementById('operation');
          var idInput = document.getElementById('node-id');
          var labelInput = document.getElementById('node-label');
          var saveButton = document.getElementById('saveButton');
          var cancelButton = document.getElementById('cancelButton');
          var div = document.getElementById('network-popUp');
          span.innerHTML = 'Edit Node';
          idInput.value = data.id;
          labelInput.value = data.label;
          saveButton.onclick = saveData.bind(this,data,callback);
          cancelButton.onclick = clearPopUp.bind();
          div.style.display = 'block';
      }

      options.onConnect = function(data,callback) {
        if (data.from == data.to) {
          var r=confirm('Do you want to connect the node to itself?');
          if (r==true) {
            callback(data);
          }
        }
        else {
          callback(data);
        }
      }
    }

    // create network
    instance.network = new vis.Network(document.getElementById("graph"), data, options);

    // add Events
    for (var key in x.events) {
      instance.network.on(key, x.events[key]);
    }

    // Neighbourhood Highlight http://visjs.org/examples/network/29_neighbourhood_highlight.html
    function onClick(selectedItems) {
      var nodeId;
      var degrees = 2;
      // we get all data from the dataset once to avoid updating multiple times.
      var allNodes = nodes.get({returnType:"Object"});
      if (selectedItems.nodes.length == 0) {
        // restore on unselect
        for (nodeId in allNodes) {
          if (allNodes.hasOwnProperty(nodeId)) {
            allNodes[nodeId].color = undefined;
            if (allNodes[nodeId].oldLabel !== undefined) {
              allNodes[nodeId].label = allNodes[nodeId].oldLabel;
              allNodes[nodeId].oldLabel = undefined;
            }
            allNodes[nodeId]['levelOfSeperation'] = undefined;
            allNodes[nodeId]['inConnectionList'] = undefined;
          }
        }
      }
      else {
        var allEdges = edges.get();

        // we clear the level of seperation in all nodes.
        clearLevelOfSeperation(allNodes);

        // we will now start to collect all the connected nodes we want to highlight.
        var connectedNodes = selectedItems.nodes;

        // we can store them into levels of seperation and we could then later use this to define a color per level
        // any data can be added to a node, this is just stored in the nodeObject.
        storeLevelOfSeperation(connectedNodes,0, allNodes);
        for (var i = 1; i < degrees + 1; i++) {
          appendConnectedNodes(connectedNodes, allEdges);
          storeLevelOfSeperation(connectedNodes, i, allNodes);
        }
        for (nodeId in allNodes) {
          if (allNodes.hasOwnProperty(nodeId)) {
            if (allNodes[nodeId]['inConnectionList'] == true) {
              if (allNodes[nodeId]['levelOfSeperation'] !== undefined) {
                if (allNodes[nodeId]['levelOfSeperation'] >= 2) {
                  allNodes[nodeId].color = 'rgba(150,150,150,0.75)';
                }
                else {
                  allNodes[nodeId].color = undefined;
                }
              }
              else {
                allNodes[nodeId].color = undefined;
              }
              if (allNodes[nodeId].oldLabel !== undefined) {
                allNodes[nodeId].label = allNodes[nodeId].oldLabel;
                allNodes[nodeId].oldLabel = undefined;
              }
            }
            else {
              allNodes[nodeId].color = 'rgba(200,200,200,0.5)';
              if (allNodes[nodeId].oldLabel === undefined) {
                allNodes[nodeId].oldLabel = allNodes[nodeId].label;
                allNodes[nodeId].label = "";
              }
            }
          }
        }
      }
      var updateArray = [];
      for (nodeId in allNodes) {
        if (allNodes.hasOwnProperty(nodeId)) {
          updateArray.push(allNodes[nodeId]);
        }
      }
      nodes.update(updateArray);
    }

    function storeLevelOfSeperation(connectedNodes, level, allNodes) {
      for (var i = 0; i < connectedNodes.length; i++) {
        var nodeId = connectedNodes[i];
        if (allNodes[nodeId]['levelOfSeperation'] === undefined) {
          allNodes[nodeId]['levelOfSeperation'] = level;
        }
        allNodes[nodeId]['inConnectionList'] = true;
      }
    }

    function clearLevelOfSeperation(allNodes) {
      for (var nodeId in allNodes) {
        if (allNodes.hasOwnProperty(nodeId)) {
          allNodes[nodeId]['levelOfSeperation'] = undefined;
          allNodes[nodeId]['inConnectionList'] = undefined;
        }
      }
    }

    function appendConnectedNodes(sourceNodes, allEdges) {
      var tempSourceNodes = [];
      // first we make a copy of the nodes so we do not extend the array we loop over.
      for (var i = 0; i < sourceNodes.length; i++) {
        tempSourceNodes.push(sourceNodes[i])
      }

      for (var i = 0; i < tempSourceNodes.length; i++) {
        var nodeId = tempSourceNodes[i];
        if (sourceNodes.indexOf(nodeId) == -1) {
          sourceNodes.push(nodeId);
        }
        addUnique(getConnectedNodes(nodeId, allEdges),sourceNodes);
      }
      tempSourceNodes = null;
    }

    function addUnique(fromArray, toArray) {
      for (var i = 0; i < fromArray.length; i++) {
        if (toArray.indexOf(fromArray[i]) == -1) {
          toArray.push(fromArray[i]);
        }
      }
    }

    function getConnectedNodes(nodeId, allEdges) {
      var edgesArray = allEdges;
      var connectedNodes = [];

      for (var i = 0; i < edgesArray.length; i++) {
        var edge = edgesArray[i];
        if (edge.to == nodeId) {
          connectedNodes.push(edge.from);
        }
        else if (edge.from == nodeId) {
          connectedNodes.push(edge.to)
        }
      }
      return connectedNodes;
    }

    if(x.highlight){
      instance.network.on("click",onClick);
    }


    if(x.options.dataManipulation){
      instance.network.on("resize", function(params) {console.log(params.width,params.height)});
    }

      function clearPopUp() {
        var saveButton = document.getElementById('saveButton');
        var cancelButton = document.getElementById('cancelButton');
        saveButton.onclick = null;
        cancelButton.onclick = null;
        var div = document.getElementById('network-popUp');
        div.style.display = 'none';

      }

      function saveData(data,callback) {
        var idInput = document.getElementById('node-id');
        var labelInput = document.getElementById('node-label');
        var div = document.getElementById('network-popUp');
        data.id = idInput.value;
        data.label = labelInput.value;
        clearPopUp();
        callback(data);

      }
      
      
  },

  resize: function(el, width, height, instance) {
    if(instance.network)
      instance.network.redraw();
    if(instance.legend)
      instance.legend.redraw();
  }

});
