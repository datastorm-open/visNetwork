# INFORMATION

### visNetwork was updated from vis.js 3.12.0 to vis.js 4.2.0. visNetwork 0.0.4 and higher use vis.js 4.2.0, with some changes from ealier version

## main changes


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

A page of examples is under construction, and available at 
http://dataknowledge.github.io/visNetwork/
