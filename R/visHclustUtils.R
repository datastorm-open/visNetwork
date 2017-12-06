
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
  if(!is.null(drawNames))
  {
    
    classDtaIn <- unlist(lapply(data, function(X){class(X)[1]}))
    classDtaIn <- classDtaIn%in%c("numeric", "integer")
    
    dataNum <- data[,classDtaIn, drop = FALSE]
    dataNum <- dataNum[,names(dataNum)%in%drawNames, drop = FALSE]
    
    if(ncol(dataNum) > 0 )
    {
      minPop <- apply(dataNum, 2, min)
      maxPop <- apply(dataNum, 2, max)
      meanPop <- colMeans(dataNum)
      popSpkl <- apply(dataNum,2, function(X){
        .addSparkLine(X, type = "box")
      })
      rNum <- 1:nrow(dataNum)
      
      dta$nodes$labelComplete <- sapply(1:nrow(dta$nodes), function(Z){
        if(!dta$nodes[Z,]$leaf)
        {
          nodeDep <- dta$nodes[Z,]$neib[[1]]
          nodeDep <- as.numeric(dta$nodes$label[dta$nodes$id%in%nodeDep])
          nodeDep <- nodeDep[nodeDep%in%rNum]
          .giveLabelsFromDf(dataNum[nodeDep,, drop = FALSE], popSpkl, minPop, maxPop, meanPop)
        }else{""}
      })
    }
    
    dataOthr <- data[,!classDtaIn, drop = FALSE]
    dataOthr <- dataOthr[,names(dataOthr)%in%drawNames, drop = FALSE]
    
    
    if(ncol(dataOthr) > 0 )
    {
      popSpkl <- apply(dataOthr,2, function(X){
        Y <- sort(table(X))
        spl <- .addSparkLine(Y , type = "pie", labels = names(Y))
        Y <- data.frame(Y)
        Y$X <- ifelse(nchar(as.character(Y$X) ) > 9, paste0(substr(Y$X, 1, 8), "..."), as.character(Y$X))
        modP <-  Y$X[length(Y$X)]
        paste0(spl, "On pop. (mode: <b>", modP, "</b>)")
      })
      
      namOrder <- lapply(dataOthr, function(X){
        names(sort(table(X)))
      })
      rNum <- 1:nrow(dataOthr)
      dta$nodes$labelComplete <- sapply(1:nrow(dta$nodes), function(Z){
        if(!dta$nodes[Z,]$leaf)
        {
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
  
  #Add tooltips on edges
  dta$edges$title <- dta$nodes$title[match(dta$edges$to, dta$nodes$id)]
  dta$edges$title[dta$edges$horizontal] <- NA
  dta$edges$label <- dta$nodes$inertia[match(dta$edges$to, dta$nodes$id)]
  dta$edges$label[dta$edges$horizontal] <- NA
  
  dta$edges$from[1] <- dta$nodes[dta$nodes$y == min(dta$nodes$y),]$id[1]
  dta$edges$to[1] <- dta$nodes[dta$nodes$y == min(dta$nodes$y),]$id[2]
  dta$nodes$group <- ifelse(dta$nodes$leaf, "group", "individual")
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
    # popSpkl2 <<- popSpkl
    for(i in nm){
      re[[i]] <- paste0("<br>", popSpkl[[i]],' : On pop. (mean:<b>', round(meanPop[i],2),"</b>)","<br>",
                        .addSparkLine(df[,i], type = "box",
                                      min = minPop[[i]], max = maxPop[[i]]),
                        " : On grp. (mean:<b>", round(clM[i], 2),"</b>)")
    }
  }
  re <- unlist(re)
  dd <- paste(paste("<br> <b>",names(clM), ": </b>",re, collapse = ""))
  dd
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
  dd <- paste(paste("<br> <b>",names(re), ": </b><br>",
                    popSpkl, "<br>",
                    re, "On grp. (mode:<b>", tbl[which.max(tbl$Freq),]$Var1,"</b>)", collapse = ""))
  dd
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
                          submain, footer, highlightNearest)
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
    visEdges(smooth = FALSE, font = list(background = "white"))
  
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
  ##Convert data for viz
  res <- .convertHclust(hcl, data, drawNames,
                        minNodeSize = minNodeSize, maxNodeSize = maxNodeSize)
  ##Make graph
  .makeHlcGraph(res, nodesPopSize, minNodeSize, maxNodeSize,
                colorEdges, cutree, colorGroups,
                height, width, main,
                submain, footer, highlightNearest)
}
