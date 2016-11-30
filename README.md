[![Rdoc](http://www.rdocumentation.org/badges/version/visNetwork)](http://www.rdocumentation.org/packages/visNetwork)

# visNetwork

### R package, using vis.js library for network visualization. visNetwork is now available on CRAN.

# Online documentation

### http://datastorm-open.github.io/visNetwork

And have a look to multiple R examples, vis.js documentation (````visDocumentation````). 

# News

## ``1.0.2`` available on CRAN

* #### Update network in shiny using  ````visNetworkProxy```` (lot of functionalities,  ````?visNetworkProxy````)

* #### Enabled ``highlightNearest`` & ``selectedBy`` with ``icons`` and / or ``image``

* #### Enabled hover for``highlightNearest`` and fix/improve performance with high ``degree``

* ####   multiple groups selection / title to network & legend / ionicons icons possibilities. See ?addIonicons

* #### new function ````visSave```` to save as html and ````visExport```` to save as png/jpeg (shiny and browser only)

* #### Better performance with ````visIgraphLayout````

* #### have a look to ````visIgraph```` to plot visNetwork object from igraph object (Little bit experimental)

* #### Add custom legend with ````visLegend````

# Example

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
