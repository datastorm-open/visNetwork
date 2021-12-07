#' Network visualization Physics options
#'
#' Network visualization Physics options. For full documentation, have a look at \link{visDocumentation}.
#' 
#'@param graph : a visNetwork object
#'@param solver : String. Default to 'barnesHut'.	You can select your own solver. Possible options: 'barnesHut', 'repulsion', 'hierarchicalRepulsion', 'forceAtlas2Based'. When setting the hierarchical layout, the hierarchical repulsion solver is automaticaly selected, regardless of what you fill in here.
#'@param maxVelocity : Number. Default to 50. The physics module limits the maximum velocity of the nodes to increase the time to stabilization. This is the maximum value.
#'@param minVelocity : Number. Default to 0.1. Once the minimum velocity is reached for all nodes, we assume the network has been stabilized and the simulation stops.
#'@param timestep : Number. Default to 0.5. The physics simulation is discrete. This means we take a step in time, calculate the forces, move the nodes and take another step. If you increase this number the steps will be too large and the network can get unstable. If you see a lot of jittery movement in the network, you may want to reduce this value a little.
#'
#'@param barnesHut, named list of options
#'\itemize{
#'  \item{"theta"}{ : Number. Default to 0.5. This parameter determines the boundary between consolidated long range forces and individual short range forces. To oversimplify higher values are faster but generate more errors, lower values are slower but with less errors.}
#'  \item{"gravitationalConstant"}{ : Number. Default to -2000. Gravity attracts. We like repulsion. So the value is negative. If you want the repulsion to be stronger, decrease the value (so -10000, -50000).}
#'  \item{"centralGravity"}{ : Number. Default to 0.3. There is a central gravity attractor to pull the entire network back to the center.}
#'  \item{"springLength"}{ : Number. Default to 95. The edges are modelled as springs. This springLength here is the the rest length of the spring.}
#'  \item{"springConstant"}{ : Number. Default to 0.04. This is how 'sturdy' the springs are. Higher values mean stronger springs.}
#'  \item{"damping"}{ : Number. Default to 0.09. Accepted range: [0 .. 1]. The damping factor is how much of the velocity from the previous physics simulation iteration carries over to the next iteration.}
#'  \item{"avoidOverlap"}{ : Number. Default to 0. Accepted range: [0 .. 1]. When larger than 0, the size of the node is taken into account. The distance will be calculated from the radius of the encompassing circle of the node for both the gravity model. Value 1 is maximum overlap avoidance.}
#'}
#'
#'@param forceAtlas2Based, named list of options
#'\itemize{
#'  \item{"theta"}{ : Number. Default to 0.5. This parameter determines the boundary between consolidated long range forces and individual short range forces. To oversimplify higher values are faster but generate more errors, lower values are slower but with less errors.}
#'  \item{"gravitationalConstant"}{ : Number. Default to -50. Gravity attracts. We like repulsion. So the value is negative. If you want the repulsion to be stronger, decrease the value (so -10000, -50000).}
#'  \item{"centralGravity"}{ : Number. Default to 0.01. There is a central gravity attractor to pull the entire network back to the center.}
#'  \item{"springLength"}{ : Number. Default to 100. The edges are modelled as springs. This springLength here is the the rest length of the spring.}
#'  \item{"springConstant"}{ : Number. Default to 0.08. This is how 'sturdy' the springs are. Higher values mean stronger springs.}
#'  \item{"damping"}{ : Number. Default to 0.4. Accepted range: [0 .. 1]. The damping factor is how much of the velocity from the previous physics simulation iteration carries over to the next iteration.}
#'  \item{"avoidOverlap"}{ : Number. Default to 0. Accepted range: [0 .. 1]. When larger than 0, the size of the node is taken into account. The distance will be calculated from the radius of the encompassing circle of the node for both the gravity model. Value 1 is maximum overlap avoidance.}
#'}
#'
#'@param repulsion, named list of options
#'\itemize{
#'  \item{"nodeDistance"}{ : Number. Default to 100. This is the range of influence for the repulsion.}
#'  \item{"centralGravity"}{ : Number. Default to 0.2. There is a central gravity attractor to pull the entire network back to the center.}
#'  \item{"springLength"}{ : Number. Default to 200. The edges are modelled as springs. This springLength here is the the rest length of the spring.}
#'  \item{"springConstant"}{ : Number. Default to 0.05. This is how 'sturdy' the springs are. Higher values mean stronger springs.}
#'  \item{"damping"}{ : Number. Default to 0.09. Accepted range: [0 .. 1]. The damping factor is how much of the velocity from the previous physics simulation iteration carries over to the next iteration.}
#'}
#'
#'@param hierarchicalRepulsion, named list of options
#'\itemize{
#'  \item{"nodeDistance"}{ : Number. Default to 120. This is the range of influence for the repulsion.}
#'  \item{"centralGravity"}{ : Number. Default to 0.0. There is a central gravity attractor to pull the entire network back to the center.}
#'  \item{"springLength"}{ : Number. Default to 100. The edges are modelled as springs. This springLength here is the the rest length of the spring.}
#'  \item{"springConstant"}{ : Number. Default to 0.01. This is how 'sturdy' the springs are. Higher values mean stronger springs.}
#'  \item{"damping"}{ : Number. Default to 0.09. Accepted range: [0 .. 1]. The damping factor is how much of the velocity from the previous physics simulation iteration carries over to the next iteration.}
#'  \item{"avoidOverlap"}{ : Number. Default to 0. Accepted range: [0 .. 1]. When larger than 0, the size of the node is taken into account. The distance will be calculated from the radius of the encompassing circle of the node for both the gravity model. Value 1 is maximum overlap avoidance.}
#'}
#'
#'@param stabilization, Just a boolean, or a named list of options
#'\itemize{
#'  \item{"enabled"}{ : Boolean. Default to true. Toggle the stabilization. This is an optional property. If undefined, it is automatically set to true when any of the properties of this object are defined.}
#'  \item{"iterations"}{ : Number. Default to 1000. The physics module tries to stabilize the network on load up til a maximum number of iterations defined here. If the network stabilized with less, you are finished before the maximum number.}
#'  \item{"updateInterval"}{ : Number. Default to 50. When stabilizing, the DOM can freeze. You can chop the stabilization up into pieces to show a loading bar for instance. The interval determines after how many iterations the stabilizationProgress event is triggered.}
#'  \item{"onlyDynamicEdges"}{ : Boolean. Default to false. If you have predefined the position of all nodes and only want to stabilize the dynamic smooth edges, set this to true. It freezes all nodes except the invisible dynamic smooth curve support nodes. If you want the visible nodes to move and stabilize, do not use this.}
#'  \item{"fit"}{ : Boolean. Default to true. Toggle whether or not you want the view to zoom to fit all nodes when the stabilization is finished.}
#'}
#'
#'@param adaptiveTimestep :	Boolean. Default to true. If this is enabled, the timestep will intelligently be adapted (only during the stabilization stage if stabilization is enabled!) to greatly decrease stabilization times. The timestep configured above is taken as the minimum timestep. This can be further improved by using the improvedLayout algorithm.
#'
#'@param wind, Named list. A force that pushes all non-fixed nodes in the given direction. Requires all nodes are connected to nodes which are fixed, otherwise non-attached nodes will keep moving indefinitely.
#'\itemize{
#'  \item{"x"}{ : Number. Default to 0. The amount of force to be applied pushing non-fixed nodes to the right (positive value) or to the left (negative value).}
#'  \item{"y"}{ : Number. Default to 0. The amount of force to be applied pushing non-fixed nodes downwards (positive value) or upwards (negative value).}
#'}
#'
#'@param enabled :	Boolean. Default to true. Toggle the physics system on or off. This property is optional. If you define any of the options below and enabled is undefined, this will be set to true.
#'
#'@seealso \link{visNodes} for nodes options, \link{visEdges} for edges options, \link{visGroups} for groups options, 
#'\link{visLegend} for adding legend, \link{visOptions} for custom option, \link{visLayout} & \link{visHierarchicalLayout} for layout, 
#'\link{visPhysics} for control physics, \link{visInteraction} for interaction, \link{visNetworkProxy} & \link{visFocus} & \link{visFit} for animation within shiny,
#'\link{visDocumentation}, \link{visEvents}, \link{visConfigure} ...
#' 
#' @examples
#'
#'nodes <- data.frame(id = 1:10)
#'edges <- data.frame(from = round(runif(8)*10), to = round(runif(8)*10))
#'
#'visNetwork(nodes, edges) %>%
#'  visPhysics(solver = "repulsion")
#'
#'visNetwork(nodes, edges) %>%
#'  visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -10))
#'  
#'visNetwork(nodes, edges) %>%
#'  visPhysics(stabilization = FALSE)
#'
#'
#'@export
#'@references See online documentation \url{http://datastorm-open.github.io/visNetwork/}
visPhysics <- function(graph,
                       solver = NULL,
                       maxVelocity = NULL,
                       minVelocity = NULL,
                       timestep = NULL,
                       barnesHut = NULL,
                       forceAtlas2Based = NULL,
                       repulsion = NULL,
                       hierarchicalRepulsion = NULL, 
                       stabilization = NULL, 
                       adaptiveTimestep = NULL, 
                       wind = NULL,
                       enabled = NULL){

  if(!any(class(graph) %in% c("visNetwork", "visNetwork_Proxy"))){
    stop("graph must be a visNetwork or a visNetworkProxy object")
  }
  
  if(!is.null(solver)){
    if(!solver%in%c("barnesHut", "repulsion", "hierarchicalRepulsion", "forceAtlas2Based")){
      stop('Invalid "solver". Must be one of "barnesHut", "repulsion", "hierarchicalRepulsion", "forceAtlas2Based"')
    }
  }
  
  if(length(which(!is.null(c(barnesHut, repulsion, hierarchicalRepulsion, forceAtlas2Based)))) > 1){
    stop("You can just use one of barnesHut, repulsion, hierarchicalRepulsion physics")
  }

  physics <- list()

  physics$solver <- solver
  physics$maxVelocity <- maxVelocity
  physics$minVelocity <- minVelocity
  physics$timestep <- timestep
  physics$stabilization <- stabilization
  physics$adaptiveTimestep <-  adaptiveTimestep
  physics$enabled <- enabled
  physics$wind <- wind
  
  if(!is.null(barnesHut)){
    physics$barnesHut <- barnesHut
  }
  
  if(!is.null(repulsion)){
    physics$repulsion <- repulsion
  }
  
  if(!is.null(hierarchicalRepulsion)){
    physics$hierarchicalRepulsion <- hierarchicalRepulsion
  }

  if(!is.null(forceAtlas2Based)){
    physics$forceAtlas2Based <- forceAtlas2Based
  }

  if(any(class(graph) %in% "visNetwork_Proxy")){
    options <- list(physics = physics)
    data <- list(id = graph$id, options = options)
    graph$session$sendCustomMessage("visShinyOptions",data)
  }else{
    graph$x$options$physics <- physics
  }
  graph
}
