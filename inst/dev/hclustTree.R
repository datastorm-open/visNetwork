library(data.table)
library(visNetwork)
library(ggraph)

res2 <- hclust(d = dist(iris[1:5, 1:4]))
visHclust(hclust(d = dist(iris[1:150, 1:4])))%>%visGroups(groupname = "cluster" ,color = "red")

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
                      highlightNearest = TRUE)
{

  res <- convertHclust(hcl)
  res$edges$color <- edgeColor
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

#' Init params for graph
#'
#' @noRd
graphHlcOpts <- function(dta, shapeNodes = "circle", shapeLeaf = "box", colorEdges = "black"){
  dta$nodes[, shape := shapeNodes]
  dta$nodes[leaf == TRUE, shape := shapeLeaf]
  dta$edges[, color := colorEdges]
  dta
}

#' Transform data from hclust to nodes and edges
#'
#' @noRd
convertHclust <- function(hcl)
{
  dta <- toVisNetworkData(den_to_igraph(hcl))
  dta <- lapply(dta, data.table)
  
  dta$nodes[,circular := NULL]
  dta$edges[,circular := NULL]
  dta$nodes[, label :=  create_layout(hcl, "dendrogram")$label]
  
  setnames(dta$nodes, c("layout.x"  ,"layout.y", "leaf"), c("x", "y", "hidden"))
  dta$nodes[, hidden2 := FALSE] 
  dta$nodes[,leaf := hidden]
  tpNum <- max(as.numeric(dta$nodes$id)) + 1
  dta$edges$horizontal  <- FALSE
  outList <- sapply(1:nrow(dta$nodes), function(X){
    row <- dta$nodes[X]
    if(row$hidden){
      list(row, dta$edges[as.numeric(dta$edges$from) == row$id])
    }else{
      edRow <- dta$edges[dta$edges$from == row$id]
      
      idTo <- as.numeric(edRow$to)
      XcO <- dta$nodes[id %in% idTo]
      
      ret <- rbindlist(sapply(1:nrow(edRow), function(Y){
        roW <- edRow[Y]
        roW$from
        tpNum <- tpNum + X * 100000 + Y
        roWEnd <- data.table(from = c(roW$from, tpNum), to = c(tpNum, roW$to),
                             label = "", direction = "", horizontal = c(TRUE, FALSE))
        roWEnd
      }, simplify = FALSE))
      
      XcO <- rbindlist(list(XcO, 
                            {
                              X <- ret$from[!ret$from%in%dta$nodes$id]
                              data.table(
                                id = X,
                                x = XcO[id %in% ret[from %in% X]$to]$x,
                                y = dta$nodes[id %in% ret[to %in% X]$from]$y,
                                hidden = FALSE,
                                label = 1,
                                members = dta$nodes[id %in% ret[from %in% X]$to]$members,
                                ggraph.index =  X,
                                hidden2 = TRUE,
                                leaf = TRUE
                              )
                            }
      ))
      list(XcO, ret)
    }
  }, simplify = FALSE)
  
  dta$nodes <- rbindlist(lapply(outList, function(X){X[[1]]}))
  dta$edges <- rbindlist(lapply(outList, function(X){X[[2]]}))
  dta$edges <- rbindlist(list(data.table(from = tpNum-1 , to = tpNum, label = "",
                                         direction = "", horizontal = TRUE), dta$edges))
  
  dta$nodes <- dta$nodes[!duplicated(id)]
  dta$nodes[,c("hidden", "y", "x") := list(!hidden, -y * 2000 , x * 200)]
  dta$nodes$title <- paste("Inertia : <b>", round(-dta$nodes$y/2000, 2), "</b><br>Number of individual : <b>", dta$nodes$members, "</b>")
  dta$nodes$inertia <-  round(-dta$nodes$y/2000, 2)
  dta$nodes[, hidden := NULL]
  setnames(dta$nodes, "hidden2", "hidden")
  
  dta$edges[,width := 20]
  
  #Add tooltips on edges
  dta$edges$title <- dta$nodes$title[match(dta$edges$to, dta$nodes$id)]
  dta$edges$title[dta$edges$horizontal] <- ""
  dta$edges$label <- dta$nodes$inertia[match(dta$edges$to, dta$nodes$id)]
  dta$edges$label[dta$edges$horizontal] <- NA
  
  dta$edges$from[1] <- dta$nodes[dta$nodes$y == min(dta$nodes$y)]$id[1]
  dta$edges$to[1] <- dta$nodes[dta$nodes$y == min(dta$nodes$y)]$id[2]
  dta$nodes$group <- ifelse(dta$nodes$leaf, "cluster", "individual")
  dta
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
