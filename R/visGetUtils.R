#' Function to get nodes data, with shiny only.
#'
#' Function to get nodes data, with shiny only. 
#' 
#' @param graph : a \code{\link{visNetworkProxy}}  object
#' @param input : name of shiny input created. Default to paste0(graph$id, "_nodes")
#' @param addCoordinates : Boolean. Add coordinates to nodes data ? Default to TRUE.
#' 
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#' 
#' @examples
#'\dontrun{
#'
#'# have a look to : 
#'shiny::runApp(system.file("shiny", package = "visNetwork"))
#'
#'}
#'
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visGetNodes <- function(graph, input = paste0(graph$id, "_nodes"), addCoordinates = T){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visGetNodes with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  data <- list(id = graph$id, input = input, addCoordinates = addCoordinates)
  
  graph$session$sendCustomMessage("visShinyGetNodes", data)
  
  graph
}


#' Function to get edges data, with shiny only.
#'
#' Function to get edges data, with shiny only
#' 
#' @param graph : a \code{\link{visNetworkProxy}}  object
#' @param input : name of shiny input created. Default to paste0(graph$id, "_edges")
#' 
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#' 
#' @examples
#'\dontrun{
#'
#'# have a look to : 
#'shiny::runApp(system.file("shiny", package = "visNetwork"))
#'
#'}
#'
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visGetEdges <- function(graph, input = paste0(graph$id, "_edges")){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visGetEdges with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  data <- list(id = graph$id, input = input)
  
  graph$session$sendCustomMessage("visShinyGetEdges", data)
  
  graph
}

#' Network visualization getPositions method
#'
#' For use getPositions() method in a shiny app. For full documentation, have a look at \link{visDocumentation}.
#' 
#' @param graph : a \code{\link{visNetworkProxy}}  object
#' @param nodes : NULL for all nodes (Default), or a vector of nodes id
#' @param input : name of shiny input created. Default to paste0(graph$id, "_positions")
#' 
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#' 
#' @examples
#'\dontrun{
#'
#'# have a look to : 
#'shiny::runApp(system.file("shiny", package = "visNetwork"))
#'
#'}
#'
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visGetPositions <- function(graph, nodes = NULL, input = paste0(graph$id, "_positions")){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visGetPositions with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  data <- list(id = graph$id, input = input)
  data$nodes <- nodes
  
  graph$session$sendCustomMessage("visShinyGetPositions", data)
  
  graph
}


#' Function to get selected nodes, with shiny only.
#'
#' Function to get selected nodes, with shiny only. Returns a vector of selected node ids.
#' 
#' @param graph : a \code{\link{visNetworkProxy}}  object
#' @param input : name of shiny input created. Default to paste0(graph$id, "_selectedNodes")
#' 
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#' 
#' @examples
#'\dontrun{
#'
#'# have a look to : 
#'shiny::runApp(system.file("shiny", package = "visNetwork"))
#'
#'}
#'
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visGetSelectedNodes <- function(graph, input = paste0(graph$id, "_selectedNodes")){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visGetSelectedNodes with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  data <- list(id = graph$id, input = input)
  
  graph$session$sendCustomMessage("visShinyGetSelectedNodes", data)
  
  graph
}


#' Function to get selected edges, with shiny only.
#'
#' Function to get selected edges, with shiny only. Returns a vector of selected edge ids.
#' 
#' @param graph : a \code{\link{visNetworkProxy}}  object
#' @param input : name of shiny input created. Default to paste0(graph$id, "_selectedEdges")
#' 
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#' 
#' @examples
#'\dontrun{
#'
#'# have a look to : 
#'shiny::runApp(system.file("shiny", package = "visNetwork"))
#'
#'}
#'
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visGetSelectedEdges <- function(graph, input = paste0(graph$id, "_selectedEdges")){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visGetSelectedEdges with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  data <- list(id = graph$id, input = input)
  
  graph$session$sendCustomMessage("visShinyGetSelectedEdges", data)
  
  graph
}

#' Function to get selected edges & nodes, with shiny only.
#'
#' Function to get selected edges & nodes, with shiny only
#' 
#' @param graph : a \code{\link{visNetworkProxy}}  object
#' @param input : name of shiny input created. Default to paste0(graph$id, "_selection")
#' 
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#' 
#' @examples
#'\dontrun{
#'
#'# have a look to : 
#'shiny::runApp(system.file("shiny", package = "visNetwork"))
#'
#'}
#'
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visGetSelection <- function(graph, input = paste0(graph$id, "_selection")){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visGetSelection with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  data <- list(id = graph$id, input = input)
  
  graph$session$sendCustomMessage("visShinyGetSelection", data)
  
  graph
}

#' Function to get current scale of network, with shiny only.
#'
#' Function to get current scale of network, with shiny only. Returns the current scale of the network. 1.0 is comparible to full, 0 is zoomed out infinitely.
#' 
#' @param graph : a \code{\link{visNetworkProxy}}  object
#' @param input : name of shiny input created. Default to paste0(graph$id, "_scale")
#' 
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#' 
#' @examples
#'\dontrun{
#'
#'# have a look to : 
#'shiny::runApp(system.file("shiny", package = "visNetwork"))
#'
#'}
#'
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visGetScale <- function(graph, input = paste0(graph$id, "_scale")){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visGetScale with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  data <- list(id = graph$id, input = input)
  
  graph$session$sendCustomMessage("visShinyGetScale", data)
  
  graph
}

#' Function to get current view position, with shiny only.
#'
#' Function to get current view position, with shiny only. Returns the current central focus point of the view.
#' 
#' @param graph : a \code{\link{visNetworkProxy}}  object
#' @param input : name of shiny input created. Default to paste0(graph$id, "_viewPosition")
#' 
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#' 
#' @examples
#'\dontrun{
#'
#'# have a look to : 
#'shiny::runApp(system.file("shiny", package = "visNetwork"))
#'
#'}
#'
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visGetViewPosition <- function(graph, input = paste0(graph$id, "_viewPosition")){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visGetViewPosition with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  data <- list(id = graph$id, input = input)
  
  graph$session$sendCustomMessage("visShinyGetViewPosition", data)
  
  graph
}

#' Method getConnectedEdges, with shiny only.
#'
#' Method getConnectedEdges, with shiny only. Returns a vector of edgeIds of the edges connected to this node.
#' 
#'@param graph : a \code{\link{visNetworkProxy}}  object
#'@param id : a node id
#'@param input : name of shiny input created. Default to paste0(graph$id, "_connectedEdges")
#'
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#' 
#' @examples
#'\dontrun{
#'
#'# have a look to : 
#'shiny::runApp(system.file("shiny", package = "visNetwork"))
#'
#'}
#'
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visGetConnectedEdges <- function(graph, id, input = paste0(graph$id, "_connectedEdges")){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visGetConnectedEdges with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  stopifnot(length(id) == 1)
  
  data <- list(id = graph$id, nodeId = id, input = input)

  graph$session$sendCustomMessage("visShinyGetConnectedEdges", data)
  
  graph
}

#' Method getConnectedNodes, with shiny only.
#'
#' Method getConnectedNodes, with shiny only. Returns a vector of nodeIds of the all the nodes that are directly connected to this node. If you supply an edgeId, vis will first match the id to nodes.
#' 
#'@param graph : a \code{\link{visNetworkProxy}}  object
#'@param id : a node or edge id
#'@param input : name of shiny input created. Default to paste0(graph$id, "_connectedNodes")
#'
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#' 
#' @examples
#'\dontrun{
#'
#'# have a look to : 
#'shiny::runApp(system.file("shiny", package = "visNetwork"))
#'
#'}
#'
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visGetConnectedNodes <- function(graph, id, input = paste0(graph$id, "_connectedNodes")){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visGetConnectedNodes with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  stopifnot(length(id) == 1)

  data <- list(id = graph$id, nodeId = id, input = input)
  
  graph$session$sendCustomMessage("visShinyGetConnectedNodes", data)
  
  graph
}

#' Method getBoundingBox, with shiny only.
#'
#' Method getBoundingBox, with shiny only. Returns a bounding box for the node including label in the format. These values are in canvas space.
#' 
#'@param graph : a \code{\link{visNetworkProxy}}  object
#'@param id : a node or edge id
#'@param input : name of shiny input created. Default to paste0(graph$id, "_boundingBox")
#'
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#' 
#' @examples
#'\dontrun{
#'
#'# have a look to : 
#'shiny::runApp(system.file("shiny", package = "visNetwork"))
#'
#'}
#'
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visGetBoundingBox <- function(graph, id, input = paste0(graph$id, "_boundingBox")){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visGetBoundingBox with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  stopifnot(length(id) == 1)
  
  data <- list(id = graph$id, nodeId = id, input = input)
  
  graph$session$sendCustomMessage("visShinyGetBoundingBox", data)
  
  graph
}

#' Method storePositions, with shiny only.
#'
#' Method storePositions, with shiny only. Put the X and Y positions of all nodes into that dataset.
#' 
#'@param graph : a \code{\link{visNetworkProxy}}  object
#'
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#' 
#' @examples
#'\dontrun{
#'
#'# have a look to : 
#'shiny::runApp(system.file("shiny", package = "visNetwork"))
#'
#'}
#'
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visStorePositions <- function(graph){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visStorePositions with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  data <- list(id = graph$id)
  
  graph$session$sendCustomMessage("visShinyStorePositions", data)
  
  graph
}