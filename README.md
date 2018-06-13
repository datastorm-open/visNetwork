[![CRAN Status Badge](http://www.r-pkg.org/badges/version/visNetwork)](http://cran.r-project.org/package=visNetwork) 
[![CRAN Downloads Badge](https://cranlogs.r-pkg.org/badges/visNetwork)](http://cran.r-project.org/package=visNetwork)


# visNetwork

### R package, using vis.js library for network visualization. visNetwork is now available on CRAN.

# Online documentation

### http://datastorm-open.github.io/visNetwork

And have a look to multiple R examples, vis.js documentation (````visDocumentation````). 

# News

## ``2.0.4`` available on CRAN

  * ### Update to fontAwesome 4.7.0
  * ### Fix node's id bug on collapse
  * ### Add ``main`` argument to ``selectedBy`` and ``nodesIdSelection``
  * ### Add ``sparkline`` graphics in ``visTree``
  * ### New ``visHclust`` for visualize Hierarchical cluster analysis
  * ### New ``visNetworkEditor`` and module ``visNetworkEditorUI`` to use ``visConfigure`` javascript functionnalities in R and shiny
  * ### Add ``zoom`` to ``visLegend``
  * ### to vis.js 4.20.1
  * ### Add ``input$network_initialized`` 
  * ### Add ``background`` to ``visNetwork``
  * ### Fix ``visTreeEditor`` bug using rpart object
  * ### ``visTreeEditor`` : add complexity parameters
  * ### Fix ``layout`` control in ``visIgraph`` & ``visIgraphLayout``
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
