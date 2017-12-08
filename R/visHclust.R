#' Visualize Hierarchical cluster analysis.
#' 
#' Visualize Hierarchical cluster analysis \code{hclust}. This function compute distance using \code{dist}, and
#' Hierarchical cluster analysis using \code{hclust} (from stats package or flashClust if installed), and
#' render the tree with visNetwork, adding informations. Can also be called on a \code{hclust} or \code{dist} object.
#' 
#' @param object \code{hclust | dist | data.frame}.
#' @param data \code{data.frame} data.frame with data. Only for \code{hclust} or \code{dist} object.
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
#' @param export \code{boolean}, add button for export. Default to TRUE
#' @param ... Nothing
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' #--------------
#' # data.frame
#' #--------------
#' 
#' # default call on data.frame
#' visHclust(iris, cutree = 3, colorEdges = "red")
#' 
#' # update some parameters
#' visHclust(iris, cutree = 3,
#'   tooltipColumns = c(1, 5),
#'   colorGroups = c("red", "blue", "green"))
#'   
#' # no graphics on tooltip
#' visHclust(iris, cutree = 3,
#'   tooltipColumns = NULL,
#'   main = "Hclust on iris")
#'   
#' # update group / individual nodes
#' visHclust(iris, cutree = 8) %>% 
#'  visGroups(groupname = "group", color ="#00FF00", shape = "square")  %>% 
#'  visGroups(groupname = "individual", color ="#FF0000")
#'
#' #--------------
#' # dist
#' #--------------
#' 
#' # without adding data & info in tooltip
#' visHclust(dist(iris[,1:4]), cutree = 3)
#'   
#' # adding data & info in tooltip
#' visHclust(dist(iris[,1:4]), cutree = 3, 
#'     data = iris)
#' 
#' #--------------
#' # hclust
#' #--------------
#' 
#' # without adding data & info in tooltip
#' visHclust(hclust(dist(iris[,1:4])), cutree = 3)
#'   
#' # adding data & info in tooltip
#' visHclust(hclust(dist(iris[,1:4])), cutree = 3, 
#'     data = iris) 
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
                                 height = "600px", width = "100%", export = TRUE, ...){
  
  # Control on inputs
  .ctrlArgsvisHcl(distColumns, cutree, object)
  
  # Control packages
  .ctrlPckvisHcl(tooltipColumns)
  
  # Control data
  object <- .ctrlDatavisHcl(object)
  
  # Give name to draw
  drawNames <- .giveDrawNamevisHcl(object, tooltipColumns)
  
  # Manage color grp
  colorGroups <- .manageColorGroupHlc(colorGroups, cutree)
  
  # Give data for dist
  dataForHcl <- .giveDataForHcl(object, distColumns)
  
  # Hclust calc
  dist <- dist(dataForHcl, method = distMethod)
  
  # Node size control
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
                 height = height, width = width, export = export)
}

#' @rdname visHclust
#' @export
visHclust.dist <- function(object, data = NULL, main = "", submain = "", footer = "",
                           cutree = 0,
                           hclustMethod = "complete",
                           tooltipColumns = if(!is.null(data)){
                             1:ncol(data)
                           } else {NULL},
                           colorEdges = "black",
                           colorGroups = substr(rainbow(cutree),1, 7),
                           highlightNearest = TRUE, 
                           minNodeSize = 50,
                           maxNodeSize = 200,
                           nodesPopSize = TRUE,
                           height = "600px", width = "100%", export = TRUE, ...){
  
  # flashClust
  f_hclust <- .giveFhcl()
  if(!is.null(data)){
    if(attr(object, "Size")!= nrow(data)){
      stop("'dist' matrix and 'data' must have same number of rows")
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
                   height = height, width = width, export = export)
}

#' @rdname visHclust
#' @export
visHclust.hclust <- function(object, data = NULL, main = "", submain = "", footer = "",
                             cutree = 0,
                             tooltipColumns = if(!is.null(data)){
                               1:ncol(data)
                             } else {NULL},
                             colorEdges = "black",
                             colorGroups = substr(rainbow(cutree),1, 7),
                             highlightNearest = TRUE, 
                             minNodeSize = 50,
                             maxNodeSize = 200,
                             nodesPopSize = TRUE,
                             height = "600px", width = "100%", export = TRUE, ...){
  
  if(!is.null(data)){
    if(length(object$order)!=nrow(data)){
      stop("object and data mush contain same number of individuals")
    }
  }
  
  if(!is.null(data)){
    drawNames <- .giveDrawNamevisHcl(data, tooltipColumns)
  }else{
    drawNames <- NULL
  }
  
  # Convert data for viz
  res <- .convertHclust(object, data, drawNames,
                        minNodeSize = minNodeSize, maxNodeSize = maxNodeSize)
  # Make graph
  .makeHlcGraph(res, nodesPopSize, minNodeSize, maxNodeSize,
                colorEdges, cutree, colorGroups,
                height, width, main,
                submain, footer, highlightNearest, export)
  
}





#' Transform data from hclust to nodes and edges
#'
#' @noRd
.convertHclust <- function(hcl, data, drawNames,
                           minNodeSize, maxNodeSize)
{
  ig <- suppressMessages(ggraph::den_to_igraph(hcl))
  neig <- igraph::neighborhood(ig, 150000, mode = "out")
  neig <- sapply(1:length(neig), function(i){
    neig[[i]][!neig[[i]] == i]
  }, simplify = FALSE)
  
  
  dta <- toVisNetworkData(ig, idToLabel = FALSE)
  
  dta <- lapply(dta, data.frame)
  
  dta$nodes$labelComplete <- ""
  dta$nodes$neib <- I(neig)
  if(!is.null(drawNames)){
    
    classDtaIn <- unlist(lapply(data, function(X){class(X)[1]}))
    classDtaIn <- classDtaIn%in%c("numeric", "integer")
    
    dataNum <- data[,classDtaIn, drop = FALSE]
    dataNum <- dataNum[,names(dataNum)%in%drawNames, drop = FALSE]
    
    if(ncol(dataNum) > 0){
      minPop <- apply(dataNum, 2, min)
      maxPop <- apply(dataNum, 2, max)
      meanPop <- colMeans(dataNum)
      popSpkl <- apply(dataNum,2, function(X){
        .addSparkLine(X, type = "box")
      })
      rNum <- 1:nrow(dataNum)
      
      dta$nodes$labelComplete <- sapply(1:nrow(dta$nodes), function(Z){
        if(!dta$nodes[Z,]$leaf){
          nodeDep <- dta$nodes[Z,]$neib[[1]]
          nodeDep <- as.numeric(dta$nodes$label[dta$nodes$id%in%nodeDep])
          nodeDep <- nodeDep[nodeDep%in%rNum]
          .giveLabelsFromDf(dataNum[nodeDep,, drop = FALSE], popSpkl, minPop, maxPop, meanPop)
        }else{""}
      })
    }
    
    dataOthr <- data[,!classDtaIn, drop = FALSE]
    dataOthr <- dataOthr[,names(dataOthr)%in%drawNames, drop = FALSE]
    
    
    if(ncol(dataOthr) > 0){
      popSpkl <- apply(dataOthr,2, function(X){
        Y <- sort(table(X))
        spl <- .addSparkLine(Y , type = "pie", labels = names(Y))
        Y <- data.frame(Y)
        Y$X <- ifelse(nchar(as.character(Y$X) ) > 9, paste0(substr(Y$X, 1, 8), "..."), as.character(Y$X))
        modP <-  Y$X[length(Y$X)]
        paste0(spl, " On pop. (mode: <b>", modP, "</b>)")
      })
      
      namOrder <- lapply(dataOthr, function(X){
        names(sort(table(X)))
      })
      rNum <- 1:nrow(dataOthr)
      dta$nodes$labelComplete <- sapply(1:nrow(dta$nodes), function(Z){
        if(!dta$nodes[Z,]$leaf){
          nodeDep <- dta$nodes[Z,]$neib[[1]]
          nodeDep <- as.numeric(dta$nodes$label[dta$nodes$id%in%nodeDep])
          nodeDep <- nodeDep[nodeDep%in%rNum]
          paste(dta$nodes[Z,]$labelComplete,.giveLabelsFromDfChr(dataOthr[nodeDep,, drop = FALSE], popSpkl, namOrder) )
        }else{""}
      })
    }
  }
  
  dta$nodes$circular <- NULL
  dta$edges$circular <- NULL
  
  dta$nodes$label <- suppressMessages(ggraph::create_layout(hcl, "dendrogram")$label)
  
  names(dta$nodes) <- sub("layout.", "", names(dta$nodes))
  names(dta$nodes)[which(names(dta$nodes) == "leaf")] <- "hidden"
  
  dta$nodes$hidden2 <- FALSE
  dta$nodes$leaf <- dta$nodes$hidden
  tpNum <- max(as.numeric(dta$nodes$id)) + 1
  dta$edges$horizontal  <- FALSE
  outList <- sapply(1:nrow(dta$nodes), function(X){
    row <- dta$nodes[X,]
    if(row$hidden){
      list(row, dta$edges[as.numeric(dta$edges$from) == row$id])
    }else{
      edRow <- dta$edges[dta$edges$from == row$id,]
      
      idTo <- as.numeric(edRow$to)
      XcO <- dta$nodes[dta$nodes$id %in% idTo,]
      
      ret <- do.call("rbind", sapply(1:nrow(edRow), function(Y){
        roW <- edRow[Y,]
        roW$from
        tpNum <- tpNum + X * 100000 + Y
        roWEnd <- data.frame(from = c(roW$from, tpNum), to = c(tpNum, roW$to),
                             label = "", direction = "", horizontal = c(TRUE, FALSE))
        roWEnd
      }, simplify = FALSE))
      
      XcO <- do.call("rbind",list(XcO, 
                                  {
                                    X <- ret$from[!ret$from%in%dta$nodes$id]
                                    data.frame(
                                      id = X,
                                      x = XcO[XcO$id %in% ret[ret$from %in% X,]$to,]$x,
                                      y = dta$nodes[dta$nodes$id %in% ret[ret$to %in% X,]$from,]$y,
                                      hidden = FALSE,
                                      label = 1,
                                      members = dta$nodes[dta$nodes$id %in% ret[ret$from %in% X,]$to,]$members,
                                      ggraph.index =  X,
                                      hidden2 = TRUE,
                                      leaf = TRUE,
                                      neib = I(rep(list(numeric()), length(X))),
                                      labelComplete = ""
                                    )
                                  })
      )
      
      list(XcO, ret)
    }
  }, simplify = FALSE)
  
  dta$nodes <- do.call("rbind",(lapply(outList, function(X){X[[1]]})))
  dta$edges <- do.call("rbind",(lapply(outList, function(X){X[[2]]})))
  dta$edges <- do.call("rbind", (list(data.frame(from = tpNum-1 , to = tpNum, label = "",
                                                 direction = "", horizontal = TRUE), dta$edges)))
  
  dta$nodes <- dta$nodes[!duplicated(dta$nodes$id),]
  dta$nodes$hidden <- !dta$nodes$hidden
  dta$nodes$x <- dta$nodes$x * 200
  dta$nodes$y <- -dta$nodes$y * 2000
  
  dta$nodes$title <- paste("Inertia : <b>", round(-dta$nodes$y/2000, 2), "</b><br>Number of individuals : <b>", dta$nodes$members, "</b>")
  dta$nodes$inertia <-  round(-dta$nodes$y/2000, 2)
  dta$nodes$hidden <- NULL
  names(dta$nodes)[which(names(dta$nodes) == "hidden2")] <- "hidden"
  
  dta$edges$width <- 20
  
  # Add tooltips on edges
  dta$edges$title <- dta$nodes$title[match(dta$edges$to, dta$nodes$id)]
  dta$edges$title[dta$edges$horizontal] <- NA
  dta$edges$label <- dta$nodes$inertia[match(dta$edges$to, dta$nodes$id)]
  dta$edges$label[dta$edges$horizontal] <- NA
  
  dta$edges$from[1] <- dta$nodes[dta$nodes$y == min(dta$nodes$y),]$id[1]
  dta$edges$to[1] <- dta$nodes[dta$nodes$y == min(dta$nodes$y),]$id[2]
  dta$nodes$group <- ifelse(dta$nodes$leaf, "individual", "group")
  titleDetails <- ifelse(!is.null(drawNames), "<br><b>Details : </b>", "")
  dta$nodes$title <- paste(dta$nodes$title, titleDetails, dta$nodes$labelComplete)
  dta$nodes$labelComplete <- NULL
  dta$nodes[dta$nodes$leaf & !dta$nodes$hidden,]$title <- as.character(dta$nodes[dta$nodes$leaf& !dta$nodes$hidden,]$label)
  dta$nodes$scaling.min <- minNodeSize
  dta$nodes$scaling.max <- maxNodeSize
  dta
}

.giveLabelsFromDf <- function(df, popSpkl = NULL, minPop = NULL, maxPop = NULL, meanPop = NULL){
  df <- df[!is.na(df[,1]),, drop = FALSE]
  clM <- colMeans(df)
  if(!is.null(popSpkl)){
    nm <- names(df)
    re <- list()
    for(i in nm){
      re[[i]] <- paste0("<br>", popSpkl[[i]],' : On pop. (mean:<b>', round(meanPop[i],2),"</b>)","<br>",
                        .addSparkLine(df[,i], type = "box",
                                      min = minPop[[i]], max = maxPop[[i]]),
                        " : On grp. (mean:<b>", round(clM[i], 2),"</b>)")
    }
  }
  re <- unlist(re)
  paste(paste("<br> <b>",names(clM), ": </b>", re, collapse = ""))
  
}


.giveLabelsFromDfChr <- function(df, popSpkl, namOrder){
  nm <- names(df)
  re <- list()
  for(i in nm){
    tbl <- table(df[,i])
    tbl <- tbl[na.omit(match(namOrder[[i]], names(tbl)))]
    tbl <- data.frame(tbl)
    newMod <- namOrder[[i]][!namOrder[[i]]%in%tbl$Var1]
    if(length(newMod) > 0){
      tbl <- rbind(tbl, data.frame(Var1 = newMod, Freq = 0))
    }
    namOrder
    tbl$Var1 <- ifelse(nchar(as.character(tbl$Var1) ) > 9, paste0(substr(tbl$Var1, 1, 8), "..."), as.character(tbl$Var1))
    re[[i]] <- paste0(.addSparkLine(tbl$Freq, type = "pie", labels = tbl$Var1))
  }
  re <- unlist(re)
  paste(paste("<br> <b>",names(re), ": </b><br>",
              popSpkl, "<br>",
              re, "On grp. (mode:<b>", tbl[which.max(tbl$Freq),]$Var1,"</b>)", collapse = ""))
}




.addSparkLine <- function(vect, min = NULL, max = NULL, type = "line", labels = NULL){
  if(is.null(min))min <- min(vect)
  if(is.null(max))max <- max(vect)
  drun <- sample(LETTERS, 15, replace = TRUE)
  drun <- paste0(drun, collapse = "")
  if(!is.null(labels)){
    tltp <- paste0((1:length(labels))-1, ": '", labels, "'", collapse = ",")
    tltp <- paste0("
                   tooltipFormat: \'{{offset:offset}} ({{percent.1}}%)\',   tooltipValueLookups: {
                   \'offset\': { ", tltp, "}}")
  }else{
    tltp <- NULL
  }
  paste0('<script type="text/javascript">
         $(function() {
         $(".inlinesparkline', drun,'").sparkline([',paste0(vect, collapse = ",") ,'], {
         type: "',type , '", chartRangeMin: ', min,', chartRangeMax: ', max,'
         , ', tltp, '
         }); 
         });
         </script>
         <span class="inlinesparkline', drun,'"></span>')
}


.ctrlPckvisHcl <- function(tooltipColumns)
{
  if(!is.null(tooltipColumns)){
    if(!requireNamespace("sparkline", quietly = TRUE)){
      stop("'sparkline' package is needed for this function")
    }
  }
  
  if(!requireNamespace("ggraph", quietly = TRUE)){
    stop("'ggraph' package is needed for this function")
  }
  
  if(!requireNamespace("igraph", quietly = TRUE)){
    stop("'igraph' package is needed for this function")
  }
}


.ctrlArgsvisHcl <- function(distColumns, cutree, data){
  # distColumns
  if(!is.null(distColumns))
  {
    if(!all(distColumns) %in% 1:ncol(data)){
      stop("all elements of distColumns should be in 1:ncol(data)")
    }
  }
  
  # cutree control
  if(!(is.numeric(cutree) | is.integer(cutree))){
    stop("cutree should be 'numeric' or 'integer'")
  }else{
    if(!cutree %in% 0:nrow(data)){
      stop("cutree should be in 0:ncol(data)")
    }
  }
}

.giveFhcl <- function(){
  if(!requireNamespace("flashClust", quietly = TRUE)){
    stats::hclust
  } else {
    flashClust::hclust
  }
}


.makeHlcGraph <- function(res, nodesPopSize, minNodeSize, maxNodeSize,
                          colorEdges, cutree, colorGroups,  height, width, main,
                          submain, footer, highlightNearest, export)
{
  
  res$edges$color <- colorEdges
  if(!is.null(cutree))
  {
    if(cutree > 1)
    {
      color <- colorGroups
      levelCut <- unique(sort(res$nodes$y))[(cutree) - 1] + diff(unique(sort(res$nodes$y))[(cutree)+(-1:0)])/2
      Mid <- as.numeric(max(res$nodes$id))
      res$nodes <- rbind(res$nodes, data.frame(id = c(Mid+150000, Mid+150001),
                                               x = c(min(res$nodes$x) - 500, max(res$nodes$x) + 500),
                                               y = rep(levelCut, 2),
                                               label = NA,
                                               members = NA,
                                               ggraph.index = NA,
                                               hidden = TRUE,
                                               leaf = FALSE,
                                               title = NA,
                                               neib = I(rep(list(numeric()), 2)),
                                               inertia = NA,
                                               group = "cut",
                                               scaling.min = minNodeSize,
                                               scaling.max = maxNodeSize))
      res$edges <- rbind(res$edges, data.frame(
        from = Mid+150000, 
        to = Mid+150001,
        label = NA,
        direction = "",
        horizontal = TRUE,
        width = 1,
        title= NA,
        color = "red"
      ))
      
      fromY <- merge(res$edges, res$nodes, by.x = "from", by.y = "id")[,c("from", "y")]
      toY <- merge(res$edges, res$nodes, by.x = "from", by.y = "id")[,c("to", "y")]
      names(toY)[which(names(toY) == "y")] <- "yt"
      endY <- merge(fromY, toY, by.x = "from", by.y = "to")
      nodesMainClass <- unique(endY[endY$y > levelCut & endY$yt < levelCut,]$from)
      
      nod <- nodesMainClass[1]
      nod
      
      ndL <- sapply(nodesMainClass, function(nod)
      {
        c(nod, unlist(res$nodes[res$nodes$id == nod,]$neib))
      }, simplify = FALSE)
      
      
      for(i in 1:length(ndL)){
        res$edges[res$edges$from %in% ndL[[i]] | res$edges$to %in% ndL[[i]],]$color <- color[i]
      }
    }
  }
  res$nodes$value <- res$nodes$members
  vis <- visNetwork(res$nodes, res$edges, height = height, width = width, main = main,
                    submain = submain, footer = footer) %>%
    visPhysics(enabled = FALSE) %>% 
    visInteraction(dragNodes = FALSE, selectConnectedEdges = FALSE) %>%
    visEdges(smooth = FALSE, font = list(background = "white")) %>%
    visGroups(groupname = "group", 
              color = list(background = "#D8D8D8", border = "black", 
                           highlight = "black", hover = "black"), shape = "square")  %>% 
    visGroups(groupname = "individual", 
              color = list(background = "#D8D8D8", border = "black", 
                           highlight = "black", hover = "black"), shape = "dot") %>%
    visInteraction(hover = TRUE)
  
  if(export){
    vis <- vis %>% visExport()
  }
  
  if(highlightNearest)
  {
    vis <- vis%>%
      visOptions(highlightNearest = 
                   list(enabled = TRUE,
                        degree = list(from = 0, to = 50000),
                        algorithm = "hierarchical"))
  }
  vis <- vis%>%sparkline::spk_add_deps()
  
  vis
}


.ctrlDatavisHcl <- function(data){
  if(is.null(data)){return(data)}
  if(!isTRUE(all.equal("data.frame", class(data)))){
    warning("data is coerced to data.frame")
    as.data.frame(data)
  }else{
    data
  }
}


.giveDrawNamevisHcl <- function(data, tooltipColumns){
  if(!is.null(tooltipColumns)){
    names(data)[tooltipColumns]
  } else  {
    NULL
  }
}


.giveDataForHcl <- function(data, distColumns){
  # columns for compute distance
  clas <-  unlist(lapply(data , function(X){class(X)[1]}))
  clasNum <- which(clas %in% c("numeric", "integer"))
  if(!is.null(distColumns)){
    indNotNum <- setdiff(distColumns, clasNum)
    if(length(indNotNum) > 0){
      warning("In distColumns : columns ", paste(indNotNum, collapse = ", "), " not numeric")
    }
    clasNum <- intersect(distColumns, clasNum)
  }
  if(length(clasNum) > 0){
    dataForHcl <- data[, clasNum, drop = FALSE]
  } else {
    stop("No numeric or integer columns in data. Can't compute distance.")
  }
  return(dataForHcl)
}


.manageColorGroupHlc <- function(colorGroups, cutree)
{
  if(length(colorGroups) != cutree){
    colorGroups <- colorGroups[(0:(cutree-1)%%length(colorGroups))+1]
  }
  colorGroups
}

.hlcToPLot <- function(hcl, data, drawNames, minNodeSize, maxNodeSize,
                       nodesPopSize,  colorEdges, cutree, colorGroups,
                       height, width, main,
                       submain, footer, highlightNearest){
  # Convert data for viz
  res <- .convertHclust(hcl, data, drawNames,
                        minNodeSize = minNodeSize, maxNodeSize = maxNodeSize)
  # Make graph
  .makeHlcGraph(res, nodesPopSize, minNodeSize, maxNodeSize,
                colorEdges, cutree, colorGroups,
                height, width, main,
                submain, footer, highlightNearest)
}

