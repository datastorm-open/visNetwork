# visNetwork
R package, using vis.js library for network visualization

### Warning
Actually, outputs works well with R, shiny, and using RStudio server, but in some case (on Windows, windows 8.1 ... ?), viewer output don't work using RStudio desktop.

```` 
devtools::install_github("bthieurmel/visNetwork")

require(visNetwork)
?visNetwork

nodes <- data.frame(id = 1:3)
edges <- data.frame(from = c(1,2), to = c(1,3))
visNetwork(nodes, edges)

````
