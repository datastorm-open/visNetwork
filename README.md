This fork of vsNetwork works in the same way as the original visNetwork, 
with following modifications: 

- renderVisNetwork returns a JS object accessible as list in R indicating the changes applied to the graph

- for add or change a node:
{cmd:"changeNode", id:<node id>, label:<new label value>, map: "", type: "">}

- for add a edge:
{cmd: "addEdge", id: <edge id>, from: <node id>, to: <node id>}

- for deleting elements
{cmd: "deleteElements", nodes: <node id list>, edges: <edge id list>}


#############################################################################################
below the original readme from main visNetwork:
#############################################################################################
, 
# visNetwork
R package, using vis.js library for network visualization. visNetwork is now available on CRAN.
Have a look to multiple R examples, vis.js documentation (````visDocumentation````), and online help page http://dataknowledge.github.io/visNetwork

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
````
