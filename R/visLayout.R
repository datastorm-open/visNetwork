#' Network visualization layout options
#'
#' Network visualization layout options. For full documentation, have a look at  \link[visNetwork]{visDocumentation}.
#'
#' @param graph : a visNetwork object
#' @param randomSeed : Number. When NOT using the hierarchical layout, the nodes are randomly positioned initially. This means that the settled result is different every time. If you provide a random seed manually, the layout will be the same every time. Ideally you try with an undefined seed, reload until you are happy with the layout and use the getSeed() method to ascertain the seed.
#' @param improvedLayout	: Boolean. Default to true. When enabled, the network will use the Kamada Kawai algorithm for initial layout. For networks larger than 100 nodes, clustering will be performed automatically to reduce the amount of nodes. This can greatly improve the stabilization times. If the network is very interconnected (no or few leaf nodes), this may not work and it will revert back to the old method. Performance will be improved in the future.
#' @param clusterThreshold :	Number. Default to	150. Cluster threshold to which improvedLayout applies.
#' @param hierarchical : Boolean. Default to false. When true, the layout engine positions the nodes in a hierarchical fashion using default settings. For customization you can use  \link[visNetwork]{visHierarchicalLayout}
#'
#' @examples
#'
#' nodes <- data.frame(id = 1:10)
#' edges <- data.frame(from = round(runif(8)*10), to = round(runif(8)*10))
#' 
#' # fix seed, so you retrieve same network each time...!
#' visNetwork(nodes, edges) %>%
#'  visLayout(randomSeed = 123) 
#'
#' visNetwork(nodes, edges) %>%
#'  visLayout(randomSeed = 123)   
#'  
#' # hierarchical
#' visNetwork(nodes, edges) %>%
#'  visLayout(hierarchical = TRUE) 
#'  
#' visNetwork(nodes, edges) %>%
#'  visHierarchicalLayout(direction = "LR")
#'
#'@seealso  \link[visNetwork]{visNodes} for nodes options,  \link[visNetwork]{visEdges} for edges options,  \link[visNetwork]{visGroups} for groups options, 
#' \link[visNetwork]{visLegend} for adding legend,  \link[visNetwork]{visOptions} for custom option,  \link[visNetwork]{visLayout} &  \link[visNetwork]{visHierarchicalLayout} for layout, 
#' \link[visNetwork]{visPhysics} for control physics,  \link[visNetwork]{visInteraction} for interaction,  \link[visNetwork]{visNetworkProxy} &  \link[visNetwork]{visFocus} &  \link[visNetwork]{visFit} for animation within shiny,
#' \link[visNetwork]{visDocumentation},  \link[visNetwork]{visEvents},  \link[visNetwork]{visConfigure} ...
#'
#'@export
#'@references See online documentation \url{https://datastorm-open.github.io/visNetwork/}
visLayout <- function(graph,
                      randomSeed = NULL,
                      improvedLayout = NULL,
                      clusterThreshold = NULL,
                      hierarchical = NULL){
  
  if(!any(class(graph) %in% c("visNetwork", "visNetwork_Proxy"))){
    stop("graph must be a visNetwork or a visNetworkProxy object")
  }
  
  layout <- list()
  
  layout$randomSeed <- randomSeed
  layout$improvedLayout <- improvedLayout
  layout$clusterThreshold <- clusterThreshold
  layout$hierarchical <- hierarchical

  if(any(class(graph) %in% "visNetwork_Proxy")){
    options <- list(layout = layout)
    data <- list(id = graph$id, options = options)
    graph$session$sendCustomMessage("visShinyOptions",data)
  }else{
    if("layout"%in%names(graph$x$options)){
      graph$x$options$layout <- mergeLists(graph$x$options$layout, layout)
    }else{
      graph$x$options$layout <- layout
    }
  }
  graph
}
