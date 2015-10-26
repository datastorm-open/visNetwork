This fork of vsNetwork works in the dame way as the original visNetwork, 
with this modifications: 

- renderVisnNetwork returns a JS object accesible as list in R indicating the changes applied to the graph
for add or change a node:
{cmd:"changeNode", id:<node id>, label:<new label value>, map: "", type: "">}

for add a edge:
{cmd: "addEdge", id: <edge id>, from: <node id>, to: <node id>

for deleting elements
{cmd: "deleteElements", nodes: <node id list>, edges: <edge id list>

In the "examples" directory, there is an example showing how to access a graph in neo4j, 
to visualize it, modify it and store back the changes into neo4j. 
Only the changes will be applied to the database, a full load of the complete graph only happens with the "Load" Button.

This version is alpha, you have to fill out every control, and you have to know what you do.

It is based on the RNeo4j package written by Nicole White and the visNetwork from B. Thieurmel

#############################################################################################
below the original readme from visnetwork:
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
