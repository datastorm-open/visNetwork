#' Network visualization layout options
#'
#' Network visualization layout options. For full documentation, have a look at \link{visDocumentation}.
#'
#'@param graph : a visNetwork object
#'@param randomSeed : Number. When NOT using the hierarchical layout, the nodes are randomly positioned initially. This means that the settled result is different every time. If you provide a random seed manually, the layout will be the same every time. Ideally you try with an undefined seed, reload until you are happy with the layout and use the getSeed() method to ascertain the seed.
#'@param hierarchical : Boolean. Default to false. >When true, the layout engine positions the nodes in a hierarchical fashion using default settings. For customization you can use \link{visHierarchicalLayout}
#'
#'@examples
#'
#' nodes <- data.frame(id = 1:10)
#' edges <- data.frame(from = round(runif(8)*10), to = round(runif(8)*10))
#'
#' visNetwork(nodes, edges) %>%
#'  visLayout(randomSeed = 123, hierarchical = FALSE) 
#'
#' visNetwork(nodes, edges) %>%
#'  visHierarchicalLayout(direction = "LR")
#'
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLayout} & \link{visHierarchicalLayout} for layout, \link{visPhysics} for physics, \link{visInteraction} for interaction, ...
#'
#'
#'@export

visLayout <- function(graph,
                      randomSeed = TRUE,
                      hierarchical = NULL){
  
  layout <- list()
  
  layout$randomSeed <- randomSeed
  layout$hierarchical <- hierarchical

  if("layout"%in%names(graph$x$options)){
    graph$x$options$layout <- mergeLists(graph$x$options$layout, layout)
  }else{
    graph$x$options$layout <- layout
  }
  
  
  graph
}
