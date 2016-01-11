# visNetwork

### R package, using vis.js library for network visualization. visNetwork is now available on CRAN.

# News

* ### have a look to ````visIgraphLayout````

Better performance ? Use ````visIgraphLayout```` to compute coordinates using ``igraph`` package, and then render network faster. So you can also use all ``igraph`` layout function, like ``layout_in_circle``, ....

* ### have a look to ````visIgraph````

Little bit experimental function, to plot visNetwork object from igraph object

* ### have a look to ````visNetworkProxy````

Can now update and call method in shiny on a visNetwork object !

* ### have a look to ````visLegend````

# Online documentation

### http://dataknowledge.github.io/visNetwork

And have a look to multiple R examples, vis.js documentation (````visDocumentation````). 

```` 
install.packages("visNetwork")

# devtools::install_github("dataknowledge/visNetwork") for developpement version

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
