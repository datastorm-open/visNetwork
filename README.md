[![CRAN Status Badge](https://www.r-pkg.org/badges/version/visNetwork)](https://CRAN.R-project.org/package=visNetwork) 
[![CRAN Downloads Badge](https://cranlogs.r-pkg.org/badges/visNetwork)](https://CRAN.R-project.org/package=visNetwork)


# visNetwork

### R package, using vis.js library for network visualization. visNetwork is now available on CRAN.

# Online documentation

### http://datastorm-open.github.io/visNetwork

And have a look to multiple R examples, vis.js documentation (````visDocumentation````). 

# News

## ``2.1.0`` available on CRAN (in a few days...)

  * ### #361 : fix visHclust with new ggraph version
  * ### switch to vis-network 9.1.0
  * ### visTree : fix using invalid R colnames + crash no one X
  * ### add fontawesome 5.13.0 support
  * ### #377 : custom manipulation columns
  * ### #335 : Enable highlightNearest degree for groups
  * ### visTree : update performance compute boxplot sparkline with large data
  * ### visTree/TreeModule : fix passing constant variable & character tooltipColumns
  
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
