% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visIgraphLayout.R
\name{visIgraphLayout}
\alias{visIgraphLayout}
\title{Use a igraph layout for compute coordinates & fast rendering}
\usage{
visIgraphLayout(
  graph,
  layout = "layout_nicely",
  physics = FALSE,
  smooth = FALSE,
  type = "square",
  randomSeed = NULL,
  layoutMatrix = NULL,
  ...
)
}
\arguments{
\item{graph}{: a visNetwork object}

\item{layout}{: Character Name of igraph layout function to use. Default to "layout_nicely"}

\item{physics}{: Boolean. Default to FALSE. Enabled physics on nodes ?}

\item{smooth}{: Boolean. Default to FALSE. Use smooth edges ?}

\item{type}{: Character Type of scale from igrah to vis.js. "square" (default) render in a square limit by height. "full" use width and height to scale in a rectangle.}

\item{randomSeed}{: Number. The nodes are randomly positioned initially. This means that the settled result is different every time. If you provide a random seed manually, the layout will be the same every time.}

\item{layoutMatrix}{: in case of layout = 'layout.norm'. the 'layout' argument (A matrix with two or three columns, the layout to normalize)}

\item{...}{: Adding arguments to layout function}
}
\description{
Use a igraph layout for compute coordinates and fast rendering. 
This function affect x and y coordinates to nodes data.frame using a igraph layout, 
and then render network faster with no stabilization. 
We set some options as : visNodes(physics = FALSE) &
visEdges(smooth = FALSE) & visPhysics(stabilization= FALSE), but you can overwrite
them using arguments or by add another call after visIgraphLayout
}
\examples{

\dontrun{
nnodes <- 200
nnedges <- 400

nodes <- data.frame(id = 1:nnodes)
edges <- data.frame(from = sample(1:nnodes, nnedges, replace = T), 
                   to = sample(1:nnodes, nnedges, replace = T))

# with default layout
visNetwork(nodes, edges) \%>\% 
 visIgraphLayout()

# use full space
visNetwork(nodes, edges) \%>\% 
 visIgraphLayout(type = "full")

# in circle ?
visNetwork(nodes, edges) \%>\% 
 visIgraphLayout(layout = "layout_in_circle") \%>\%
 visOptions(highlightNearest = list(enabled = T, hover = T), 
   nodesIdSelection = T)
 
# keep physics with smooth curves ?
visNetwork(nodes, edges) \%>\% 
 visIgraphLayout(physics = TRUE, smooth = TRUE)

# fix randomSeed to keep position
visNetwork(nodes, edges) \%>\% 
 visIgraphLayout(randomSeed = 123)
 
visNetwork(nodes, edges) \%>\% 
 visIgraphLayout(randomSeed = 123)

# layout_with_sugiyama
nodes <- data.frame(id = 1:5)
edges <- data.frame(from = c(1, 2, 2, 4), to = c(2, 3, 4, 5))

visNetwork(nodes, edges) \%>\%
 visIgraphLayout(layout = "layout_with_sugiyama", layers = c(1, 2, 3, 3, 4))

visNetwork(nodes, edges) \%>\%
 visIgraphLayout(layout = "layout_with_sugiyama")
 
}

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
