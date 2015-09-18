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
