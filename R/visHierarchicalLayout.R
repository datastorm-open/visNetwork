#' Network visualization Hierarchical layout options
#'
#' Network visualization Hierarchical layout options. See \url{http://visjs.org/docs/network.html#Hierarchical_layout}.
#'
#'@param enabled : Boolean. Default to TRUE when calling this function.	Enable or disable the hierarchical layout.
#'@param levelSeparation : Number. Default to 150.  This defines the space between levels (in the Y-direction, considering UP-DOWN direction).
#'@param nodeSpacing : Number. Default to 100.  This defines the space between nodes in the same level (in the X-direction, considering UP-DOWN direction). This is only relevant during the initial placing of nodes.
#'@param direction : String. Default to 'UD'.  This defines the direction the network is drawn in. The supported directions are: Up-Down (UD), Down-Up (DU), Left-Right (LR) and Right-Left (RL). These need to be supplied by the acronyms in parentheses.
#'@param layout : String. Default to 'hubsize'.  This defines the way the nodes are distributed. Available options are "hubsize" and "direction". The default value is hubsize, meaning the node with the most edges connected to it (largest hub) is on top. Alternatively, direction arranges the nodes based on the direction of the edges.
#'
#'@examples
#'
#' nodes <- data.frame(id = 1:10)
#' edges <- data.frame(from = round(runif(8)*10), to = round(runif(8)*10))
#'
#' visNetwork(nodes, edges) %>%
#'  visHierarchicalLayout()
#'
#' visNetwork(nodes, edges) %>%
#'  visHierarchicalLayout(direction = "LR")
#'
#'@export

visHierarchicalLayout <- function(graph,
                                  enabled = TRUE,
                                  levelSeparation = NULL,
                                  nodeSpacing = NULL,
                                  direction = NULL,
                                  layout = NULL){

  hierarchicalLayout <- list()

  hierarchicalLayout$enabled <- enabled
  hierarchicalLayout$levelSeparation <- levelSeparation
  hierarchicalLayout$nodeSpacing <- nodeSpacing
  hierarchicalLayout$direction <- direction
  hierarchicalLayout$layout <- layout

  graph$x$options$hierarchicalLayout <- hierarchicalLayout

  graph
}
