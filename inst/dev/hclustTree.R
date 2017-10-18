library(visNetwork)
library(ggraph)

res2 <- hclust(d = dist(iris[1:5, 1:4]))
visHclust(hclust(d = dist(iris[1:5, 1:4])))%>%visGroups(groupname = "cluster" ,color = "red")
visHclust(hclust(d = dist(iris[1:150, 1:4])), cutree =7)




#' vizNetwork for hclust
#'
#' @param hcl \code{hclust} output from hclust
#' @param main \code{character} main title
#' @param edgeColor \code{character} color for edge
#' @param highlightNearest \code{boolean} highlight branch when you click
#' 
#' @export

## Couleur noeuds taille noeuds 
visHclust <- function(hcl, main = "", edgeColor = "black",
                      highlightNearest = TRUE, cutree = NULL)
{
  color <- substr(rainbow(15),1, 7)
  res <- convertHclust(hcl)
  res$edges$color <- edgeColor
  if(!is.null(cutree))
  {
    if(cutree > 1)
    {
  levelCut <- unique(sort(res$nodes$y))[(cutree) - 1] + diff(unique(sort(res$nodes$y))[(cutree)+(-1:0)])/2
  Mid <- as.numeric(max(res$nodes$id))
  res$nodes <- rbind(res$nodes, data.frame(id = c(Mid+852, Mid+853),
                                           x = c(min(res$nodes$x) - 500, max(res$nodes$x) + 500),
                                           y = rep(levelCut, 2),
                                           label = NA,
                                           members = NA,
                                           ggraph.index = NA,
                                           hidden = TRUE,
                                           leaf = FALSE,
                                           title = NA,
                                           inertia = NA,
                                           group = "cut"
  ))
  res$edges <- rbind(res$edges, data.frame(
    from = Mid+852, 
    to = Mid+853,
    label = NA,
    direction = "",
    horizontal = TRUE,
    width = 1,
    title= NA,
    color = "red"
  ))
  
  res$nodes[match(res$edges$from ,res$nodes$id),]
  
  
  
  res$edges[res$edges$from < levelCut & res$edges$to > levelCut]
  
  
    fromY <- merge(res$edges, res$nodes, by.x = "from", by.y = "id")[,c("from", "y")]
     toY <- merge(res$edges, res$nodes, by.x = "from", by.y = "id")[,c("to", "y")]
    names(toY)[which(names(toY) == "y")] <- "yt"
     endY <- merge(fromY, toY, by.x = "from", by.y = "to")
     nodesMainClass <- unique(endY[endY$y > levelCut & endY$yt < levelCut,]$from)
     
     nod <- nodesMainClass[1]
     nod
     
    ndL <- sapply(nodesMainClass, function(nod)
    {
     FD <- nameChild(nod, res)
     names(FD)
     allNam <- names(unlist(FD))
     nodeDep <- unique(unlist(strsplit(allNam, "[.]")))
    }, simplify = FALSE)
     for(i in 1:length(ndL)){
       res$edges[res$edges$from %in% ndL[[i]] | res$edges$to %in% ndL[[i]],]$color <- color[i]
     }
  }}
  
  vis <- visNetwork(res$nodes, res$edges, main = main) %>%
    visPhysics(enabled = FALSE) %>% 
    visEdges(smooth = FALSE, font = list(background = "white") )
  
  if(highlightNearest)
  {
    vis <- vis%>%
    visOptions(highlightNearest = 
                 list(enabled = TRUE,
                      degree = list(from = 0, to = 5000),
                      algorithm = "hierarchical"))
  }
 
  
  vis
  
}

#' Transform data from hclust to nodes and edges
#'
#' @noRd
convertHclust <- function(hcl)
{
  dta <- toVisNetworkData(den_to_igraph(hcl))
  dta <- lapply(dta, data.frame)
  
  dta$nodes$circular <- NULL
  dta$edges$circular <- NULL
  dta$nodes$label <- create_layout(hcl, "dendrogram")$label
  
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
                                leaf = TRUE
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
  
  
  dta$nodes$title <- paste("Inertia : <b>", round(-dta$nodes$y/2000, 2), "</b><br>Number of individual : <b>", dta$nodes$members, "</b>")
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
  dta$nodes$group <- ifelse(dta$nodes$leaf, "cluster", "individual")
  dta
}



nameChild <- function(child, res){
  sapply(child, function(X){
    child <- .givChild(X, res)
    if(length(child)> 0 ){
      nameChild(child, res)
    }else{
      ""
    }
  }, simplify = FALSE)}

.givChild <- function(N, res){
  res$edges[res$edges$from == N,]$to
}

# res2 <- hclust(d = dist(diamonds[1:1000, 5:9]))
# res <- convertHclust(res2)
# 
# addSparkLine <- function(vect){
#   paste0('<script type="text/javascript">
#          $(function() {
#          $(".inlinesparkline").sparkline(); 
#          });
#          </script>
#          <span class="inlinesparkline">[', paste(vect, collapse = ",") , '], { type: "bar"}</span>')
# }
# 
# 
# node <- data.frame(id = 1, title = addSparkLine(1:10))
# 
# visNetwork(node, edges = data.frame())%>%spk_add_deps()
