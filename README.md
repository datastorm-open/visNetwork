[![Rdoc](http://www.rdocumentation.org/badges/version/visNetwork)](http://www.rdocumentation.org/packages/visNetwork)
[![Rdoc](http://www.rdocumentation.org/api/badges/Direct/visNetwork)](http://www.rdocumentation.org/packages/visNetwork)


# visNetwork

### R package, using vis.js library for network visualization. visNetwork is now available on CRAN.

# Online documentation

### http://datastorm-open.github.io/visNetwork

And have a look to multiple R examples, vis.js documentation (````visDocumentation````). 

# News

## ``2.0.2`` dev on github

  * ### New ``visNetworkEditor`` and module ``visNetworkEditorUI`` to use ``visConfigure`` javascript functionnalities in R and shiny
  * ### Add ``zoom`` to ``visLegend``
  * ### to vis.js 4.20.1
  * ### Add ``input$network_initialized`` 
  * ### Add ``background`` to ``visNetwork``
  * ### Fix ``visTreeEditor`` bug using rpart object
  * ### ``visTreeEditor`` : add complexity parameters
  
## ``2.0.0`` available on CRAN

* ### Add support for edges color/label using ``highlightNearest/selectedBy``

* ### New ``collapse`` features (``?visOptions``)

* ### New proxy methods : ``visGetBoundingBox``, ``visGetConnectedEdges``, ``visGetConnectedNodes``, ``visGetEdges``, ``visGetNodes``, ``visGetPositions``, ``visGetScale``, ``visGetSelectedEdges``, ``visGetSelectedNodes``, ``visGetSelection``, ``visGetViewPosition``

* ### init ``visTree``, to visualize rpart object, ``visTreeEditor`` and ``visTreeModuleServer``

![alt text](https://github.com/datastorm-open/visNetwork/blob/master/inst/img/tree_example.png)

# Example

```` 
install.packages("visNetwork")

# devtools::install_github("datastorm-open/visNetwork") for development version

require(visNetwork)
?visNetwork

# minimal example
nodes <- data.frame(id = 1:3)
edges <- data.frame(from = c(1,2), to = c(1,3))
visNetwork(nodes, edges)

# vignette
vignette("Introduction-to-visNetwork")

# full javascript documentation
visDocumentation()

# shiny example
shiny::runApp(system.file("shiny", package = "visNetwork"))
````
