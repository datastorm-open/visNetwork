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
    
    var data;
    var nodes;
    var edges;
    
    // highlight nearest variables
    var allNodes;
    var highlightActive = false;
    var nodesDataset ;
    var edgesDataset ;

    // clear el.id (for shiny...)
    document.getElementById(el.id).innerHTML = "";  
    
    // id nodes selection : add a list on top left
    // actually only with nodes + edges data (not dot and gephi)
    if(x.idselection && x.nodes){  
      var option;
      //Create and append select list
      var selnodes = HTMLWidgets.dataframeToD3(x.nodes);
      var selectList = document.createElement("select");
      
      selectList.id = "nodeSelect"+el.id;
      
      document.getElementById(el.id).appendChild(selectList);
      
      option = document.createElement("option");
      option.value = "";
      option.text = "";
      selectList.appendChild(option);
        
      //Create and append the options
      for (var i = 0; i < selnodes.length; i++) {
        option = document.createElement("option");
        option.value = selnodes[i]['id'];
        if(selnodes[i]['label']){
          option.text = selnodes[i]['label'];
        }else{
          option.text = selnodes[i]['id'];
        }
        selectList.appendChild(option);
      }
      
      if (window.Shiny){
            var changeInput = function(id, data) {
              Shiny.onInputChange(el.id + '_' + id, data);
            };
            changeInput('selected', document.getElementById("nodeSelect"+el.id).value);
      }
          
      selectList.onchange =  function(){
        if(instance.network)
          currentid = document.getElementById("nodeSelect"+el.id).value;
          if(currentid === ""){
            instance.network.selectNodes([]);
          }else{
            instance.network.selectNodes([currentid]);
          }
          if(x.highlight){
            neighbourhoodHighlight(instance.network.getSelection());
          }
          if (window.Shiny){
            var changeInput = function(id, data) {
              Shiny.onInputChange(el.id + '_' + id, data);
            };
            changeInput('selected', document.getElementById("nodeSelect"+el.id).value);
          }
      };
    }
    
    // divide page
    var maindiv  = document.createElement('div');
    maindiv.id = "maindiv"+el.id;
    maindiv.setAttribute('style', 'width:100%;height:100%');
    document.getElementById(el.id).appendChild(maindiv);
    
    var graph = document.createElement('div');
    graph.id = "graph"+el.id;
      
    if(x.groups && x.legend){
      var legendwidth = x.legendWidth*100;
      var legend = document.createElement('div');
      legend.id = "legend"+el.id;
      legend.setAttribute('style', 'float:left; width:'+legendwidth+'%;height:100%');
      document.getElementById("maindiv"+el.id).appendChild(legend);
      
      graph.setAttribute('style', 'float:right; width:'+(100-legendwidth)+'%;height:100%');
      
    }else{
      graph.setAttribute('style', 'float:right; width:100%;height:100%');
    }
    
    document.getElementById("maindiv"+el.id).appendChild(graph);
    
    //create legend if needed  
    if(x.groups && x.legend){
      
      var legendnodes = new vis.DataSet();
      
      var mynetwork = document.getElementById('legend'+el.id);
      var lx = - mynetwork.clientWidth / 2 + 50;
      var ly = - mynetwork.clientWidth / 2 + 50;
      var step = 70;
      for (g = 0; g < x.groups.length; g++){
        legendnodes.add({id: g, x : lx, y : ly+g*step, label: x.groups[g], group: x.groups[g], value: 1, mass:0});
      }
      
      var datalegend = {
        nodes: legendnodes, 
        edges: null
      };
      
      var optionslegend = {
        dragNetwork: false,
        dragNodes: false,
        selectable: false,
        interaction:{
          dragNodes: false,
          dragView: false
        }
      };
      
      optionslegend.groups = x.options.groups;
      instance.legend = new vis.Network(document.getElementById("legend"+el.id), datalegend, optionslegend);
      
    }
    
    if(x.nodes){

      // network
      nodes = new vis.DataSet();
      edges = new vis.DataSet();
      
      nodes.add(HTMLWidgets.dataframeToD3(x.nodes));
      edges.add(HTMLWidgets.dataframeToD3(x.edges));
      
      data = {
        nodes: nodes,
        edges: edges
      };
      
    }else if(x.dot){
      data = {
        dot: x.dot
      };
    }else if(x.gephi){
      data = {
        gephi: x.gephi
      };
    } 
    
    var options = x.options;
    
    // Custom data manipualtion
    if(x.options.manipulation.enabled){
      
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
      
      options.manipulation.addNode = function(data,callback) {
        document.getElementById('operation').innerHTML = "Add Node";
        document.getElementById('node-id').value = data.id;
        document.getElementById('node-label').value = data.label;
        document.getElementById('saveButton').onclick = saveData.bind(this, data, callback);
        document.getElementById('cancelButton').onclick = clearPopUp.bind();
        document.getElementById('network-popUp').style.display = 'block';
      };
      
      options.manipulation.editNode = function(data,callback) {
        document.getElementById('operation').innerHTML = "Edit Node";
        document.getElementById('node-id').value = data.id;
        document.getElementById('node-label').value = data.label;
        document.getElementById('saveButton').onclick = saveData.bind(this, data, callback);
        document.getElementById('cancelButton').onclick = cancelEdit.bind(this,callback);
        document.getElementById('network-popUp').style.display = 'block';
      };
      
      options.manipulation.addEdge = function(data,callback) {
        if (data.from == data.to) {
          var r = confirm("Do you want to connect the node to itself?");
          if (r === true) {
            callback(data);
          }
        }
        else {
          callback(data);
        }
      };
    }
    
    // create network
    instance.network = new vis.Network(document.getElementById("graph"+el.id), data, options);
    
    // add Events
    for (var key in x.events) {
      instance.network.on(key, x.events[key]);
    }
    
    function neighbourhoodHighlight(params) {
      var selectNode;
      var changeInput = function(id, data) {
        Shiny.onInputChange(el.id + '_' + id, data);
      };
      if (params.nodes.length > 0) {
  
        if(x.idselection){
          selectNode = document.getElementById('nodeSelect'+el.id);
          selectNode.value = params.nodes;
          if (window.Shiny){
            changeInput('selected', selectNode.value);
          }
        }

        highlightActive = true;
        var i,j;
        var selectedNode = params.nodes[0];
        var degrees = 2;

        // mark all nodes as hard to read.
        for (var nodeId in allNodes) {
          allNodes[nodeId].color = 'rgba(200,200,200,0.5)';
          if (allNodes[nodeId].hiddenLabel === undefined) {
            allNodes[nodeId].hiddenLabel = allNodes[nodeId].label;
            allNodes[nodeId].label = undefined;
          }
        }
        var connectedNodes = instance.network.getConnectedNodes(selectedNode);
        var allConnectedNodes = [];

        // get the second degree nodes
        for (i = 1; i < degrees; i++) {
          for (j = 0; j < connectedNodes.length; j++) {
            allConnectedNodes = allConnectedNodes.concat(instance.network.getConnectedNodes(connectedNodes[j]));
          }
        }

        // all second degree nodes get a different color and their label back
        for (i = 0; i < allConnectedNodes.length; i++) {
          //allNodes[allConnectedNodes[i]].color = 'rgba(150,150,150,0.75)';
          if (allNodes[allConnectedNodes[i]].hiddenLabel !== undefined) {
            allNodes[allConnectedNodes[i]].label = allNodes[allConnectedNodes[i]].hiddenLabel;
            allNodes[allConnectedNodes[i]].hiddenLabel = undefined;
          }
        }

        // all first degree nodes get their own color and their label back
        for (i = 0; i < connectedNodes.length; i++) {
          allNodes[connectedNodes[i]].color = undefined;
          if (allNodes[connectedNodes[i]].hiddenLabel !== undefined) {
            allNodes[connectedNodes[i]].label = allNodes[connectedNodes[i]].hiddenLabel;
            allNodes[connectedNodes[i]].hiddenLabel = undefined;
          }
        }

        // the main node gets its own color and its label back.
        allNodes[selectedNode].color = undefined;
        if (allNodes[selectedNode].hiddenLabel !== undefined) {
          allNodes[selectedNode].label = allNodes[selectedNode].hiddenLabel;
          allNodes[selectedNode].hiddenLabel = undefined;
        }
      }
      else if (highlightActive === true) {
        if(x.idselection){
          selectNode = document.getElementById('nodeSelect'+el.id);
          selectNode.value = "";
          if (window.Shiny){
            changeInput('selected', "");
          }
        }
        // reset all nodes
        for (var nodeId in allNodes) {
          allNodes[nodeId].color = undefined;
          if (allNodes[nodeId].hiddenLabel !== undefined) {
          allNodes[nodeId].label = allNodes[nodeId].hiddenLabel;
            allNodes[nodeId].hiddenLabel = undefined;
          }
        }
      highlightActive = false
      }

      // transform the object into an array
      var updateArray = [];
      for (nodeId in allNodes) {
        if (allNodes.hasOwnProperty(nodeId)) {
          updateArray.push(allNodes[nodeId]);
        }
      }
      nodesDataset.update(updateArray);
    }
  
    function onClickIDSlection(selectedItems) {
      var selectNode;
      var changeInput = function(id, data) {
        Shiny.onInputChange(el.id + '_' + id, data);
      };
      
      if (selectedItems.nodes.length !== 0) {
        selectNode = document.getElementById('nodeSelect'+el.id);
        selectNode.value = selectedItems.nodes;
        if (window.Shiny){
          changeInput('selected', selectNode.value);
        }
      }else{
        selectNode = document.getElementById('nodeSelect'+el.id);
        selectNode.value = "";
        if (window.Shiny){
          changeInput('selected', "");
        }
      }
    }
    
    // actually only with nodes + edges data (not dot and gephi)
    if(x.highlight && x.nodes){
      nodesDataset = nodes; // these come from WorldCup2014.js
      edgesDataset = edges; // these come from WorldCup2014.js
      allNodes = nodesDataset.get({returnType:"Object"});
      instance.network.on("click",neighbourhoodHighlight);
    }else if(x.idselection && x.nodes){
      instance.network.on("click",onClickIDSlection);
    }
    
    
    if(x.options.dataManipulation){
      instance.network.on("resize", function(params) {console.log(params.width,params.height)});
    }
    
    function clearPopUp() {
      document.getElementById('saveButton').onclick = null;
      document.getElementById('cancelButton').onclick = null;
      document.getElementById('network-popUp').style.display = 'none';
    }
    
    function saveData(data,callback) {
      data.id = document.getElementById('node-id').value;
      data.label = document.getElementById('node-label').value;
      clearPopUp();
      callback(data);
    }
    
    function cancelEdit(callback) {
      clearPopUp();
      callback(null);
    }
    
  },
  
  resize: function(el, width, height, instance) {
    console.info("width");
    console.info(width);
    console.info("height");
    console.info(height);
    if(instance.network)
      instance.network.redraw();
    if(instance.legend)
      instance.legend.redraw();
  }
  
});
