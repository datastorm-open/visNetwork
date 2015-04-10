#' Network visualization Physics options
#'
#' Network visualization Physics options. See \url{http://visjs.org/docs/network.html#Physics}.
#'
#'@param barnesHut, named list of options
#'\itemize{
#'  \item{"enabled"}{ : Boolean. Default to true. This switches the Barnes-Hut simulation on or off. If it is turned off, the old repulsion model is used. Barnes-Hut is generally faster and yields better results.}
#'  \item{"gravitationalConstant"}{ : Number. Default to -2000. This is the gravitational constand used to calculate the gravity forces. More information is available here.}
#'  \item{"centralGravity"}{ : Number. Default to 0.1. The central gravity is a force that pulls all nodes to the center. This ensures independent groups do not float apart.}
#'  \item{"springLength"}{ : Number. Default to 95. In the previous versions this was a property of the edges, called length. This is the length of the springs when they are at rest. During the simulation they will be streched by the gravitational fields. To greatly reduce the edge length, the gravitationalConstant has to be reduced as well.}
#'  \item{"springConstant"}{ : Number. Default to 0.04. This is the spring constant used to calculate the spring forces based on Hooke′s Law. More information is available here.}
#'  \item{"damping""}{ : Number. Default to 0.09. This is the damping constant. It is used to dissipate energy from the system to have it settle in an equilibrium. More information is available here.}
#'}
#'@param repulsion, named list of options
#'\itemize{
#'  \item{"centralGravity"}{ : Number. Default to 0.1. The central gravity is a force that pulls all nodes to the center. This ensures independent groups do not float apart.}
#'  \item{"nodeDistance"}{ : Number. Default to 100. This parameter is used to define the distance of influence of the repulsion field of the nodes. Below half this distance, the repulsion is maximal and beyond twice this distance the repulsion is zero.}
#'  \item{"springLength"}{ : Number. Default to 50. In the previous versions this was a property of the edges, called length. This is the length of the springs when they are at rest. During the simulation they will be streched by the gravitational fields. To greatly reduce the edge length, the gravitationalConstant has to be reduced as well.}
#'  \item{"springConstant"}{ : Number. Default to 0.05. This is the spring constant used to calculate the spring forces based on Hooke′s Law. More information is available here.}
#'  \item{"damping"}{ : Number. Default to 0.09. This is the damping constant. It is used to dissipate energy from the system to have it settle in an equilibrium. More information is available here.}
#'}
#'@param hierarchicalRepulsion, named list of options
#'\itemize{
#'  \item{"centralGravity"}{ : Number. Default to 0.5. The central gravity is a force that pulls all nodes to the center. This ensures independent groups do not float apart.}
#'  \item{"nodeDistance"}{ : Number. Default to 60. This parameter is used to define the distance of influence of the repulsion field of the nodes. Below half this distance, the repulsion is maximal and beyond twice this distance the repulsion is zero.}
#'  \item{"springLength"}{ : Number. Default to 100. In the previous versions this was a property of the edges, called length. This is the length of the springs when they are at rest. During the simulation they will be streched by the gravitational fields. To greatly reduce the edge length, the gravitationalConstant has to be reduced as well.}
#'  \item{"springConstant"}{ : Number. Default to 0.01. This is the spring constant used to calculate the spring forces based on Hooke′s Law. More information is available here.}
#'  \item{"damping"}{ : Number. Default to 0.09. This is the damping constant. It is used to dissipate energy from the system to have it settle in an equilibrium. More information is available here.}
#' }
#'
#' @examples
#'
#' nodes <- data.frame(id = 1:10)
#' edges <- data.frame(from = round(runif(8)*10), to = round(runif(8)*10))
#'
#'visNetwork(nodes, edges) %>%
#'  visPhysics(repulsion = list(nodeDistance = 2))
#'
#'visNetwork(nodes, edges) %>%
#'  visPhysics(barnesHut = list(enabled = TRUE))
#'
#'visNetwork(nodes, edges) %>%
#'  visPhysics(hierarchicalRepulsion = list(nodeDistance = 129))
#'
#'
#'@export

visPhysics <- function(graph,
                       barnesHut = NULL,
                       repulsion = NULL,
                       hierarchicalRepulsion = NULL){

  if(length(which(!is.null(c(barnesHut, repulsion, hierarchicalRepulsion)))) != 1){
    stop("You can just use one of barnesHut, repulsion, hierarchicalRepulsion physics")
  }

  physics <- list()

  if(!is.null(barnesHut)){
    physics$barnesHut <- barnesHut
  }
  if(!is.null(repulsion)){
    physics$barnesHut <- list(enabled = FALSE)
    physics$repulsion <- repulsion
  }
  if(!is.null(hierarchicalRepulsion)){
    physics$hierarchicalRepulsion <- hierarchicalRepulsion
  }

  graph$x$options$physics <- physics

  graph
}
