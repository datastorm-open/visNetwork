# visNetwork

### R package, using vis.js library for network visualization. visNetwork is now available on CRAN.

# News

* #### new function ````visSave```` to save as html and ````visExport```` to save as png/jpeg (shiny and browser only)

* #### Better performance with ````visIgraphLayout````

* #### have a look to ````visIgraph```` to plot visNetwork object from igraph object (Little bit experimental)

* #### Update network in shiny using  ````visNetworkProxy````

* #### Add custom legend with ````visLegend````

# Online documentation

### http://datastorm-open.github.io/visNetwork

And have a look to multiple R examples, vis.js documentation (````visDocumentation````). 

```` 
install.packages("visNetwork")

# devtools::install_github("datastorm-open/visNetwork") for developpement version

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
