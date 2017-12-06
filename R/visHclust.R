#' Visualize Hierarchical cluster analysis.
#' 
#' Visualize Hierarchical cluster analysis \code{hclust}. This function compute distance using \code{dist}, and
#' Hierarchical cluster analysis using \code{hclust} (from stats package or flashClust if installed), and
#' render the tree with visNetwork, adding informations.
#' 
#' @param object \code{hclust | dist | data.frame}.
#' @param data \code{data.frame} data.frame with data.
#' @param main For add a title. See \link{visNetwork}
#' @param submain For add a subtitle. See \link{visNetwork}
#' @param footer For add a footer. See \link{visNetwork}
#' @param distColumns \code{numeric} indice of columns used for compute distance. 
#'  If NULL (default), keep all \code{numeric} and \code{integer} columns. 
#'  If Not NULL, we keep only  \code{numeric} and \code{integer} columns
#' @param distMethod \code{character} the distance measure to be used for dist function. Default to 'euclidean'. See \code{\link[stats]{dist}}.
#' @param hclustMethod \code{character} the agglomeration method to be used for hclust function. Default to 'complete'. See \code{\link[stats]{hclust}}.
#' @param cutree \code{numeric} or \code{integer} desired number of groups. Default to 0
#' @param tooltipColumns \code{numeric} indice of columns used in tooltip. All by default.
#' So, we add a boxplot or a pie focus on sub-population and all population using \code{sparkline} package.
#' @param colorEdges \code{character} color of edges. Default to 'black'
#' @param colorGroups \code{character}, color for group in exa ("#00FF00"). Default rainbow.
#' @param minNodeSize \code{numeric}, in case of \code{nodesPopSize}, minimum size of a node. Defaut to 15. Else, nodes size is minNodeSize + maxNodeSize / 2 
#' @param maxNodeSize \code{numeric}, in case of \code{nodesPopSize}, maximum size of a node. Defaut to 30. Else, nodes size is minNodeSize + maxNodeSize / 2 
#' @param nodesPopSize \code{boolean}, nodes sizes depends on population ? Default to FALSE
#' @param highlightNearest \code{boolean} highlight sub-tree on click.
#' @param height \code{character}, default to "600px"
#' @param width \code{character}, default to "100\%"
#' @param ... nothing
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' visHclust(iris, cutree = 3)
#' 
#' visHclust(iris, cutree = 3,
#'   tooltipColumns = c(1, 5),
#'   colorGroups = c("red", "blue", "green"))
#'   
#' # no graphics on tooltip
#' visHclust(iris, cutree = 3, main = "Hclust on iris", 
#'   tooltipColumns = NULL)
#'   
#'   visHclust(iris, cutree = 8)%>% 
#'   visGroups(groupname = "cluster", color ="#00FF00", shape = "square")  %>% 
#'   visGroups(groupname = "individual", color ="#FF0000")
#'   
#'   #On dist
#'   visHclust(dist(iris[,1:4]), cutree = 3, data = iris)
#'   
#'   #On hclust
#'   visHclust(hclust(dist(iris[,1:4])), cutree = 3)
#'   
#'   
#' }
#' 
#' @importFrom grDevices rainbow
#' @importFrom stats dist hclust na.omit
#' 
#' @export
visHclust <- function(object, ...) UseMethod("visHclust")

#' @rdname visHclust
#' @export
#' 
visHclust.default <- function(object, ...) visHclust.data.frame(object, ...)

#' @rdname visHclust
#' @export
visHclust.data.frame <- function(object, main = "", submain = "", footer = "",
                      distColumns = NULL, 
                      distMethod = "euclidean", 
                      hclustMethod = "complete",
                      cutree = 0,
                      tooltipColumns = 1:ncol(object),
                      colorEdges = "black",
                      colorGroups = substr(rainbow(cutree),1, 7),
                      highlightNearest = TRUE, 
                      minNodeSize = 50,
                      maxNodeSize = 200,
                      nodesPopSize = TRUE,
                      height = "600px", width = "100%", ...){
  
  # Controls on inputs
  .ctrlArgsvisHcl(distColumns, cutree, object)

  #Controle packages
  .ctrlPckvisHcl(tooltipColumns)


  #Controle on data
  object <- .ctrlDatavisHcl(object)
  
  #Give name to draw
  drawNames <- .giveDrawNamevisHcl(object, tooltipColumns)
  
  #Manage color gp
  colorGroups <- .manageColorGroupHlc(colorGroups, cutree)
   
  #Give data for dist
  dataForHcl <- .giveDataForHcl(object, distColumns)

  #Hclust calc
  dist <- dist(dataForHcl, method = distMethod)
  
  #Node size controle
  if(nodesPopSize){
    minNodeSize = minNodeSize
    maxNodeSize = maxNodeSize
  }else{
    minNodeSize = (minNodeSize + maxNodeSize) / 2
    maxNodeSize = minNodeSize
  }
  
  visHclust.dist(object = dist, data = object, main = main,
                 submain = submain, footer = footer,
                 cutree = cutree,
                 hclustMethod = hclustMethod,
                 tooltipColumns = tooltipColumns,
                 colorEdges = colorEdges,
                 colorGroups = colorGroups,
                 highlightNearest = highlightNearest, 
                 minNodeSize = minNodeSize,
                 maxNodeSize = maxNodeSize,
                 nodesPopSize = nodesPopSize,
                 height = height, width = width)
  

}

#' @rdname visHclust
#' @export
visHclust.dist <- function(object, data = NULL, main = "", submain = "", footer = "",
                           cutree = 0,
                           hclustMethod = "complete",
                           tooltipColumns = NULL,
                           colorEdges = "black",
                           colorGroups = substr(rainbow(cutree),1, 7),
                           highlightNearest = TRUE, 
                           minNodeSize = 50,
                           maxNodeSize = 200,
                           nodesPopSize = TRUE,
                           height = "600px", width = "100%", ...){
  
  #flashClust
  f_hclust <- .giveFhcl()
  if(!is.null(data)){
    if(attr(object, "Size")!=nrow(data)){
      stop("Your dist matrix and data argument mush have same number of row")
    }
  }
  
  hcl <- f_hclust(d = object, method = hclustMethod)
  
  
  visHclust.hclust(object = hcl, data = data, main = main,
                   submain = submain, footer = footer,
                   cutree = cutree,
                   tooltipColumns = tooltipColumns,
                   colorEdges = colorEdges,
                   colorGroups = colorGroups,
                   highlightNearest = highlightNearest, 
                   minNodeSize = minNodeSize,
                   maxNodeSize = maxNodeSize,
                   nodesPopSize = nodesPopSize,
                   height = height, width = width)
}

#' @rdname visHclust
#' @export
visHclust.hclust <- function(object, data = NULL, main = "", submain = "", footer = "",
                           cutree = 0,
                           tooltipColumns = NULL,
                           colorEdges = "black",
                           colorGroups = substr(rainbow(cutree),1, 7),
                           highlightNearest = TRUE, 
                           minNodeSize = 50,
                           maxNodeSize = 200,
                           nodesPopSize = TRUE,
                           height = "600px", width = "100%", ...){
  
  if(!is.null(data)){
    if(length(object$order)!=nrow(data)){
      stop("object and data mush contain same number of individuals")
    }
    
    if(is.null(tooltipColumns)){
      tooltipColumns <- 1:ncol(data)
    }
  }
  
  if(!is.null(data))
  {
  drawNames <- .giveDrawNamevisHcl(data, tooltipColumns)
  }else{
    drawNames <- NULL
  }
  
  ##Convert data for viz
  res <- .convertHclust(object, data, drawNames,
                        minNodeSize = minNodeSize, maxNodeSize = maxNodeSize)
  ##Make graph
  .makeHlcGraph(res, nodesPopSize, minNodeSize, maxNodeSize,
                colorEdges, cutree, colorGroups,
                height, width, main,
                submain, footer, highlightNearest)
  
}


