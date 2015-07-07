# INFORMATION

### visNetwork was updated from vis.js 3.12.0 to vis.js 4.4.0. visNetwork 0.0.4 and higher use vis.js 4.4.0, with some changes from ealier version

## main changes

- new options on ````visNodes````, ````visEdges````, ````visGroups````
- in ````visOptions```` dataManipulation rename to manipulation
- lot of options move from ````visOptions```` to new ````visInteraction````, and some have been deleted/renamed
  - dragNetwork, hideNodesOnDrag, hover, selectable, keyboard, ....
  - navigation, rename navigationButtons
- new ````visLayout```` function, on which we can fixed seed, and new options on ````visHierarchicalLayout ````
- new solver forceAtlas2Based, and new options on ````visPhysics````
- delete ````visTooltips```` (just tooltipDelay on ````visInteraction````)
- new events on  ````visEvents ````
- new cool configure options, look at ````visConfigure  ````
- more flexible clustering possibilities for javascript coder, but a simple integration into R is really more difficult. We'll try to propose something. Actually, ````visClustering```` is deprecated

Have a look to multiple R examples, vis.js documentation (````visDocumentation````), and online help page http://dataknowledge.github.io/visNetwork/

# visNetwork
R package, using vis.js library for network visualization

```` 
devtools::install_github("dataknowledge/visNetwork")

require(visNetwork)
?visNetwork

nodes <- data.frame(id = 1:3)
edges <- data.frame(from = c(1,2), to = c(1,3))
visNetwork(nodes, edges)
````


