#' Function to nearest nodes of a target node, with shiny only.
#'
#' Function to nearest nodes of a target node, with shiny only.
#' 
#' @param graph : a \code{\link{visNetworkProxy}}  object
#' @param target : name of shiny input returning target node id
#' @param maxpoints : Number of nearest nodes. Default to 5
#' @param addDist : If TRUE, add a column named dist_ that contains the distance from the coordinate to the point, in pixels.
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
visNearestNodes <- function(graph, target, maxpoints = 5, addDist = T){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visGetNodes with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  force(target)
  
  visNetworkProxy(graph$id) %>%
    visGetNodes(input = graph$session$ns("tmp_nearest_nodes"))
  
  current_nodes <- graph$session$input$tmp_nearest_nodes
  if(is.list(current_nodes)){
    current_nodes <- rbind.fill.network(lapply(current_nodes, function(x){data.frame(t(unlist(x)))}))
  } else{
    current_nodes <- NULL
  }
  
  res <- NULL
  if(!is.null(current_nodes)){
    target_row <- which(current_nodes$id %in% target)
    if(length(target_row) > 0){
      current_nodes[, "x"] <- as.numeric(as.character(current_nodes[, "x"]))
      current_nodes[, "y"] <- as.numeric(as.character(current_nodes[, "y"]))
      current_nodes$dist_ <- sqrt((current_nodes[, "x"] - current_nodes[target_row, "x"])^2 + 
                                    (current_nodes[, "y"] - current_nodes[target_row, "y"])^2)
      res <- current_nodes[-target_row, ]
      res <- res[order(res$dist_), ]
      res <- res[1:min(maxpoints, nrow(res)), ]
      if(!addDist){
        res$dist_ <- NULL
      }
    }
    
  }
  res
}


allocate_column_network <- function (example, nrows, dfs, var) 
{
  a <- attributes(example)
  type <- typeof(example)
  class <- a$class
  isList <- is.recursive(example)
  a$names <- NULL
  a$class <- NULL
  if (is.data.frame(example)) {
    stop("Data frame column '", var, "' not supported by rbind.fill")
  }
  if (is.array(example)) {
    if (length(dim(example)) > 1) {
      if ("dimnames" %in% names(a)) {
        a$dimnames[1] <- list(NULL)
        if (!is.null(names(a$dimnames))) 
          names(a$dimnames)[1] <- ""
      }
      df_has <- vapply(dfs, function(df) var %in% names(df), 
                       FALSE)
      dims <- unique(lapply(dfs[df_has], function(df) dim(df[[var]])[-1]))
      if (length(dims) > 1) 
        stop("Array variable ", var, " has inconsistent dims")
      a$dim <- c(nrows, dim(example)[-1])
      length <- prod(a$dim)
    }
    else {
      a$dim <- NULL
      a$dimnames <- NULL
      length <- nrows
    }
  }
  else {
    length <- nrows
  }
  if (is.factor(example)) {
    df_has <- vapply(dfs, function(df) var %in% names(df), 
                     FALSE)
    isfactor <- vapply(dfs[df_has], function(df) is.factor(df[[var]]), 
                       FALSE)
    if (all(isfactor)) {
      levels <- unique(unlist(lapply(dfs[df_has], function(df) levels(df[[var]]))))
      a$levels <- levels
      handler <- "factor"
    }
    else {
      type <- "character"
      handler <- "character"
      class <- NULL
      a$levels <- NULL
    }
  }
  else if (inherits(example, "POSIXt")) {
    tzone <- attr(example, "tzone")
    class <- c("POSIXct", "POSIXt")
    type <- "double"
    handler <- "time"
  }
  else {
    handler <- type
  }
  column <- vector(type, length)
  if (!isList) {
    column[] <- NA
  }
  attributes(column) <- a
  assignment <- make_assignment_call_network(length(a$dim))
  setter <- switch(handler, character = function(rows, what) {
    what <- as.character(what)
    eval(assignment)
  }, factor = function(rows, what) {
    what <- match(what, levels)
    eval(assignment)
  }, time = function(rows, what) {
    what <- as.POSIXct(what, tz = tzone)
    eval(assignment)
  }, function(rows, what) {
    eval(assignment)
  })
  getter <- function() {
    class(column) <<- class
    column
  }
  list(set = setter, get = getter)
}

output_template_network <- function (dfs, nrows) 
{
  vars <- unique(unlist(lapply(dfs, base::names)))
  output <- vector("list", length(vars))
  names(output) <- vars
  seen <- rep(FALSE, length(output))
  names(seen) <- vars
  for (df in dfs) {
    matching <- intersect(names(df), vars[!seen])
    for (var in matching) {
      output[[var]] <- allocate_column_network(df[[var]], nrows, 
                                       dfs, var)
    }
    seen[matching] <- TRUE
    if (all(seen)) 
      break
  }
  list(setters = lapply(output, `[[`, "set"), getters = lapply(output, 
                                                               `[[`, "get"))
}

make_assignment_call_network <- function (ndims) 
{
  assignment <- quote(column[rows] <<- what)
  if (ndims >= 2) {
    assignment[[2]] <- as.call(c(as.list(assignment[[2]]), 
                                 rep(list(quote(expr = )), ndims - 1)))
  }
  assignment
}

make_names_network <- function (x, prefix = "X") 
{
  nm <- names(x)
  if (is.null(nm)) {
    nm <- rep.int("", length(x))
  }
  n <- sum(nm == "", na.rm = TRUE)
  nm[nm == ""] <- paste(prefix, seq_len(n), sep = "")
  nm
}
quickdf_network <- function (list) 
{
  rows <- unique(unlist(lapply(list, NROW)))
  stopifnot(length(rows) == 1)
  names(list) <- make_names_network(list, "X")
  class(list) <- "data.frame"
  attr(list, "row.names") <- c(NA_integer_, -rows)
  list
}

rbind.fill.network<- function (...) 
{
  dfs <- list(...)
  if (length(dfs) == 0) 
    return()
  if (is.list(dfs[[1]]) && !is.data.frame(dfs[[1]])) {
    dfs <- dfs[[1]]
  }
  if (length(dfs) == 0) 
    return()
  if (length(dfs) == 1) 
    return(dfs[[1]])
  is_df <- vapply(dfs, is.data.frame, logical(1))
  if (any(!is_df)) {
    stop("All inputs to rbind.fill must be data.frames", 
         call. = FALSE)
  }
  rows <- unlist(lapply(dfs, .row_names_info, 2L))
  nrows <- sum(rows)
  ot <- output_template_network(dfs, nrows)
  setters <- ot$setters
  getters <- ot$getters
  if (length(setters) == 0) {
    return(as.data.frame(matrix(nrow = nrows, ncol = 0)))
  }
  pos <- matrix(c(cumsum(rows) - rows + 1, rows), ncol = 2)
  for (i in seq_along(rows)) {
    rng <- seq(pos[i, 1], length = pos[i, 2])
    df <- dfs[[i]]
    for (var in names(df)) {
      setters[[var]](rng, df[[var]])
    }
  }
  quickdf_network(lapply(getters, function(x) x()))
}
