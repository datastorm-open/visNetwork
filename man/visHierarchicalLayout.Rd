% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visHierarchicalLayout.R
\name{visHierarchicalLayout}
\alias{visHierarchicalLayout}
\title{Network visualization Hierarchical layout options}
\usage{
visHierarchicalLayout(
  graph,
  enabled = TRUE,
  levelSeparation = NULL,
  nodeSpacing = NULL,
  treeSpacing = NULL,
  blockShifting = NULL,
  edgeMinimization = NULL,
  parentCentralization = NULL,
  direction = NULL,
  sortMethod = NULL,
  shakeTowards = NULL
)
}
\arguments{
\item{graph}{: a visNetwork object}

\item{enabled}{: Boolean. Default to TRUE when calling this function.    Enable or disable the hierarchical layout.}

\item{levelSeparation}{: Number. Default to 150.  The distance between the different levels.}

\item{nodeSpacing}{: Number. Default to 100. Minimum distance between nodes on the free axis. This is only for the initial layout. If you enable physics, the node distance there will be the effective node distance.}

\item{treeSpacing}{: Number. Default to 200. Distance between different trees (independent networks). This is only for the initial layout. If you enable physics, the repulsion model will denote the distance between the trees.}

\item{blockShifting}{: Boolean. Default to true. Method for reducing whitespace. Can be used alone or together with edge minimization. Each node will check for whitespace and will shift it's branch along with it for as far as it can, respecting the nodeSpacing on any level. This is mainly for the initial layout. If you enable physics, they layout will be determined by the physics. This will greatly speed up the stabilization time though!}

\item{edgeMinimization}{: Boolean. Default to true. Method for reducing whitespace. Can be used alone or together with block shifting. Enabling block shifting will usually speed up the layout process. Each node will try to move along its free axis to reduce the total length of it's edges. This is mainly for the initial layout. If you enable physics, they layout will be determined by the physics. This will greatly speed up the stabilization time though!}

\item{parentCentralization}{: Boolean. Default to true. When true, the parents nodes will be centered again after the the layout algorithm has been finished.}

\item{direction}{: String. Default to 'UD'. The direction of the hierarchical layout. The available options are: UD, DU, LR, RL. To simplify: up-down, down-up, left-right, right-left.}

\item{sortMethod}{: String. Default to 'hubsize'.  The algorithm used to ascertain the levels of the nodes based on the data. The possible options are: hubsize, directed.}

\item{shakeTowards}{: String. Default to 'roots'. Controls whether in directed layout should all the roots be lined up at the top and their child nodes as close to their roots as possible (roots) or all the leaves lined up at the bottom and their parents as close to their children as possible (leaves, default}
}
\description{
Network visualization Hierarchical layout options. For full documentation, have a look at \link{visDocumentation}.
}
\examples{

nodes <- data.frame(id = 1:10)
edges <- data.frame(from = round(runif(8)*10), to = round(runif(8)*10))

visNetwork(nodes, edges) \%>\%
 visHierarchicalLayout()

visNetwork(nodes, edges) \%>\%
 visHierarchicalLayout(direction = "LR")
 
}
\references{
See online documentation \url{https://datastorm-open.github.io/visNetwork/}
}
\seealso{
\link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
}
