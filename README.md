# visNetwork
R package, using vis.js library for network visualization

```` 
devtools::install_github("bthieurmel/visNetwork")

require(visNetwork)
?visNetwork

nodes <- data.frame(id = 1:3)
edges <- data.frame(from = c(1,2), to = c(1,3))
visNetwork(nodes, edges)

````

A page of examples is under construction, and available at 
http://bthieurmel.github.io/visNetwork/
