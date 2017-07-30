#' Visualize Recursive Partitioning and Regression Trees (rpart object)
#' 
#' Visualize Recursive Partitioning and Regression Trees \code{rpart}. Have a look to \code{visTreeEditor} to edity and get back network, or to \code{visTreeModuleServer} to use custom tree module in R
#' 
#' @param object \code{rpart}, rpart object
#' @param main For add a title. See \link{visNetwork}
#' @param submain For add a subtitle. See \link{visNetwork}
#' @param footer For add a footer. See \link{visNetwork}
#' @param direction \code{character}, The direction of the hierarchical layout.
#' The available options are: UD, DU, LR, RL. To simplify:
#' up-down, down-up, left-right, right-left. Default UD. See \link{visHierarchicalLayout} 
#' @param nodesPopSize \code{boolean}, nodes sizes depends on population ? Default to FALSE
#' @param fallenLeaves \code{boolean} leaf nodes at the bottom of the graph ? Default to FALSE
#' @param nodesFontSize \code{numeric}, size of labels of nodes. Default to 16
#' @param edgesFontSize \code{numeric}, size of labels of edges Default to 14
#' @param legendFontSize \code{numeric}, size of labels of nodes in legend. Default to 16
#' @param legendNodesSize \code{numeric}, size of nodes in legend. Default to 22
#' @param edgesFontAlign \code{character}, for edges only. Default tp 'horizontal'. Possible options: 'horizontal' (Default),'top','middle','bottom'. See \link{visEdges}  
#' @param colorVar \code{character} colors to use or \code{data.frame} To set color of variables. 2 columns :
#' \itemize{
#'   \item{"variable"}{ : names of variables}
#'   \item{"color"}{ : colors (in hexa). See examples}
#' }
#' @param colorY if classification tree : \code{character} colors to use or \code{data.frame} 2 columns :
#' \itemize{
#'   \item{"modality"}{ : levels of Y}
#'   \item{"color"}{ : colors (in hexa)}
#' }
#' if regression tree : \code{character}, 2 colors (min and max, in hexa)
#' @param colorEdges \code{character} color of edges, in hexa. Default to #8181F7
#' @param legend \code{boolean}, add legend ? Default TRUE. \link{visLegend}
#' @param legendWidth \code{numeric}, legend width, between 0 and 1. Default 0.1
#' @param legendNcol \code{numeric}, number of columns in legend. Default 1
#' @param legendPosition \code{character}, one of "left" (Default) or "right"
#' @param highlightNearest \code{list}, Highlight nearest nodes. See \link{visOptions}
#' @param collapse \code{list}, collapse or not using double click on a node ? See \link{visOptions}
#' @param updateShape \code{boolean}, in case of collapse, udpate cluster node shape as terminal node ? Default to TRUE
#' @param tooltipDelay \code{numeric}, delay for tooltips in millisecond. Default 500
#' @param rules \code{boolean}, add rules in tooltips ? Default to TRUE
#' @param simplifyRules \code{boolean}, simplify rules writing
#' @param digits \code{numeric}, number of digits. Default to 3
#' @param height \code{character}, default to "600px"
#' @param width \code{character}, default to "100\%"
#' @param minNodeSize \code{numeric}, in case of \code{nodesPopSize}, minimum size of a node. Defaut to 15. Else, nodes size is minNodeSize + maxNodeSize / 2 
#' @param maxNodeSize \code{numeric}, in case of \code{nodesPopSize}, maximum size of a node. Defaut to 30. Else, nodes size is minNodeSize + maxNodeSize / 2 
#' @param shapeVar \code{character}, shape for variables nodes See \link{visNodes}
#' @param shapeY \code{character}, shape for terminal nodes See \link{visNodes}
#' @param export \code{boolean}, add export button. Default to TRUE
#' 
#' @return a visNetwork object 
#' 
#' @seealso \link{visTreeEditor}, \link{visTreeModuleServer}
#' @examples
#' 
#' \dontrun{
#' 
#' library(rpart)
#' 
#' # Basic classification tree
#' res <- rpart(Species~., data=iris)
#' visTree(res, main = "Iris classification Tree")
#' 
#' # Basic regression tree
#' res <- rpart(Petal.Length~., data=iris)
#' visTree(res, edgesFontSize = 14, nodesFontSize = 16)
#' 
#' # Complex tree
#' data("solder")
#' res <- rpart(Opening~., data = solder, control = rpart.control(cp = 0.00005))
#' visTree(res, height = "800px", nodesPopSize = TRUE, minNodeSize = 10, maxNodeSize = 30)
#' 
#' # ----- Options
#' res <- rpart(Opening~., data = solder, control = rpart.control(cp = 0.005))
#' 
#' # fallen leaves + align edges label & size
#' visTree(res, fallenLeaves = TRUE, height = "500px", 
#'  edgesFontAlign = "middle", edgesFontSize = 20)
#' 
#' # disable rules in tooltip, and render tooltip faster
#' # disable hover highlight
#' visTree(res, rules = FALSE, tooltipDelay = 0, 
#'  highlightNearest = list(enabled = TRUE, degree = list(from = 50000, to = 0), 
#'  hover = FALSE, algorithm = "hierarchical"))
#' 
#' # Change color with data.frame
#' colorVar <- data.frame(variable = names(solder), 
#'  color = c("#339933", "#b30000","#4747d1","#88cc00", "#9900ff","#247856"))
#'  
#' colorY <- data.frame(modality = unique(solder$Opening), 
#'  color = c("#AA00AA", "#CDAD15", "#213478"))
#' 
#' visTree(res, colorEdges = "#000099", colorVar = colorVar, colorY = colorY)
#' 
#' # Change color with vector
#' visTree(res, colorEdges = "#000099", 
#'     colorVar = substring(rainbow(6), 1, 7), 
#'     colorY = c("blue", "green", "orange"))
#' 
#' }
#' 
#' @export
#' 
#' @importFrom grDevices hcl
#' @importFrom grDevices colorRamp
#' @importFrom grDevices rgb
#' 
visTree <- function(object,
                    main = "",
                    submain = "",
                    footer = "",
                    direction = "UD",
                    fallenLeaves = FALSE,
                    rules = TRUE,
                    simplifyRules = TRUE,
                    shapeVar = "dot",
                    shapeY = "square",
                    colorVar = NULL,
                    colorY = NULL,
                    colorEdges = "#8181F7",
                    nodesFontSize = 16,
                    edgesFontSize = 14,
                    edgesFontAlign = "horizontal",
                    legend = TRUE,
                    legendNodesSize = 22,
                    legendFontSize = 16,
                    legendWidth = 0.1,
                    legendNcol = 1,
                    legendPosition = "left",
                    nodesPopSize = FALSE,
                    minNodeSize = 15,
                    maxNodeSize = 30,
                    highlightNearest =  list(enabled = TRUE,
                                             degree = list(from = 50000, to = 0), hover = FALSE,
                                             algorithm = "hierarchical"),
                    collapse = list(enabled = TRUE, fit = TRUE, resetHighlight = TRUE, 
                                    clusterOptions = list(fixed = TRUE, physics = FALSE)),
                    updateShape = TRUE,
                    tooltipDelay = 500,
                    digits = 3, 
                    height = "600px",
                    width = "100%",
                    export = TRUE){
  
  # controls
  stopifnot("rpart" %in% class(object))
  stopifnot("character" %in% class(direction))
  stopifnot(direction %in% c("UD", "LR", "RL", "DU"))
  stopifnot(length(direction) == 1)
  stopifnot("logical" %in% class(nodesPopSize))
  stopifnot("numeric" %in% class(minNodeSize) | "integer" %in% class(minNodeSize))
  stopifnot("numeric" %in% class(maxNodeSize) | "integer" %in% class(maxNodeSize))
  stopifnot("logical" %in% class(fallenLeaves))
  stopifnot("logical" %in% class(simplifyRules))
  stopifnot("numeric" %in% class(nodesFontSize) | "integer" %in% class(nodesFontSize))
  stopifnot("numeric" %in% class(edgesFontSize) | "integer" %in% class(edgesFontSize))
  stopifnot("numeric" %in% class(legendFontSize) | "integer" %in% class(legendFontSize))
  stopifnot("character" %in% class(edgesFontAlign))
  
  if(!is.null(colorVar)){
    stopifnot(any(c("data.frame", "character") %in% class(colorVar)))
  }
  
  if(!is.null(colorY)){
    if(object$method == "class"){
      stopifnot(any(c("data.frame", "character") %in% class(colorY)))
    }
    if(object$method == "anova"){
      stopifnot("character"%in%class(colorY))
      stopifnot(length(colorY) <= 2)
    }
  }
  
  if(!is.null(colorEdges)){
    stopifnot("character" %in% class(colorEdges))
  }
  stopifnot("logical" %in% class(legend))
  stopifnot("numeric" %in% class(legendWidth) | "integer" %in% class(legendWidth))
  stopifnot("numeric" %in% class(legendNcol) | "integer" %in% class(legendNcol))
  stopifnot("character" %in% class(legendPosition))
  stopifnot(any(c("logical", "list") %in% class(highlightNearest)))
  stopifnot(any(c("logical", "list") %in% class(collapse)))
  stopifnot("numeric" %in% class(tooltipDelay)| "integer" %in% class(tooltipDelay))
  stopifnot("logical" %in% class(rules))
  stopifnot("numeric" %in% class(digits)| "integer" %in% class(digits))
  stopifnot("character" %in% class(height))
  stopifnot("character" %in% class(width))
  stopifnot("character" %in% class(shapeVar))
  stopifnot("character" %in% class(shapeY))
  
  # ------------------------------
  # get information from rpart object
  rpartNodesNames <- row.names(object$frame)
  
  infoClass <- NULL
  parentsDec <- list(lapply(rpartNodesNames, function(X)(.parent(as.numeric(X)))))
  infoVar <- object$frame$var
  infoRules <- .vis_give_rules(object)
  detailRules <- .rpart_lists(object)
  colLabels <- attributes(object$terms)$term.labels
  colClass <- attributes(object$terms)$dataClasses
  colClass <- colClass[names(colClass)%in%colLabels]
  
  if(length(rpartNodesNames) > 1){
    rpartHier <- sapply(as.numeric(rpartNodesNames[2:length(rpartNodesNames)]), function(X){
      info <- .parent(X)
      list(info[length(info)-1], info[length(info)], length(info))
    })
    
    from <- unlist(rpartHier[1,])
    to <- unlist(rpartHier[2,])
    level <- c(1, unlist(rpartHier[3,]))
    
    # ------------------------------
    # build edge info (label + tootip)
    edgesLabels <- character(length(to))
    edgesTooltip <- character(length(to))
    lapply(1:length(to), function(i){
      cur_rule <- strsplit(infoRules[paste0("Node", to[i])], ",")[[1]]
      sens <- substr(cur_rule, 1, 1)
      ind_rule <- as.numeric(substr(cur_rule, 2, nchar(cur_rule)))
      rule <- detailRules[[sens]][[ind_rule]]
      operator <- attributes(rule)$compare
      # if(names(operator) %in% names(colClass[which(colClass %in% c("factor", "character", "ordered"))])){
      if(operator %in% "="){
        operator <- NULL
        edgesLabels[i] <<- paste(rule, collapse = ", ")
      }else{
        rule <- round(rule, digits)
        edgesLabels[i] <<- paste(operator, paste(rule, collapse = ", "))
      }
      
      edgesTooltip[i] <<- paste0('<div style="text-align:center;"><b>', names(attr(rule, "compare")),  "</b></div>", 
                                 paste0('<div style="text-align:center;">', operator, rule, "</div>", collapse = ""))
      invisible()
    })
    
    edgesLabelsFull <- edgesLabels
    formatLabels <- function(x){
      ifelse(nchar(x)  >10, paste0(substr(x, 1, 7), "..."), x)
    }
    edgesLabels <- sapply(edgesLabels, formatLabels)
    
  } else {
    level <- 1
  }
  
  # ------------------------------
  # nodes
  if(length(rpartNodesNames) > 1){
    nodes_pop <- object$frame$n[match(to, rpartNodesNames)]
  } else {
    nodes_pop <- object$frame$n
  }
  nodes_var <- as.character(infoVar)
  nodes_var_color <- nodes_var[nodes_var != "<leaf>"]
  shape <- ifelse(infoVar != "<leaf>", shapeVar, shapeY)
  SortLabel <-  sort(unique(nodes_var_color))
  colorVar <- .generateVarColor(colorVar, nodes_var_color, SortLabel)
  nodes_color <- as.character(colorVar$color[match(nodes_var, colorVar$variable)])
  
  
  # get stats for nodes (mean / variance / proba)
  statsNodes <- NULL
  # Classification TREE
  if(!is.null(attributes(object)$ylevels)){
    infoClass <- attributes(object)$ylevels
    nlevelsClass <- length(infoClass)
    probaClass <- object$frame[,"yval2"]
    effectif <- data.frame(probaClass[,2:(nlevelsClass+1), drop = F])
    probs <- data.frame(probaClass[,(nlevelsClass+2):(ncol(probaClass)-1), drop = F])
    probsHtml <- probs
    for(i in 1:length(infoClass)){
      probsHtml[,i] <- paste0(infoClass[i], " : <b>",
                              round(probsHtml[,i], digits)*100, "%</b>",
                              " (", effectif[,i], ")")
    }
    statsNodes <- apply(probsHtml, 1, function(x){paste0(x, collapse = "<br>")})
    statsNodes <- paste('<hr>', statsNodes)
  }else{
    # Regression TREE
    varNodes <- round(object$frame$dev/(object$frame$n - 1),digits)
    varNodes[which(varNodes == Inf)] <- NA
    statsNodes <- paste0("Mean : <b>" , round(object$frame$yval,digits),
                         "</b><br>Variance : <b>",varNodes, "</b>")
  }
  
  # ------------------------------
  # Build rules for tooltip
  tooltipRules <- list(NULL)
  if(length(parentsDec[[1]]) > 1){
    for(i in 2:length(parentsDec[[1]])){
      use <- parentsDec[[1]][[i]]
      varDecisions <- nodes_var[match(as.character(use[-length(use)]), rpartNodesNames)]
      decisionsrules <- edgesLabelsFull[match(as.character(use), rpartNodesNames)-1]
      varDecisionBegin <- unique(varDecisions)
      if(simplifyRules){
        filtre <- ifelse(colClass[varDecisions]%in% c("character", "factor", "ordered"), 
                         varDecisions,
                         paste0(varDecisions, substr(decisionsrules, 1 ,1)))
        tabFiltre <- table(filtre) > 1
        if(length(which(tabFiltre))>0){
          filtres <- names(tabFiltre)[which(tabFiltre)]
          filtreOut <- NULL
          for(j in filtres){
            filtreOut <- c(filtreOut, max(which(j== filtre)))
          }
          keeprules <- sort(c(which(!filtre%in%filtres), filtreOut))
          varDecisions <- varDecisions[keeprules]
          decisionsrules <- decisionsrules[keeprules]
        }
        
        filtre <- varDecisions
        varDecisionsOrder <- varDecisions
        tabFiltre <- table(filtre)>1
        if(length(which(tabFiltre))>0){
          filtres <- names(tabFiltre)[which(tabFiltre)]
          for(j in filtres){
            rulesNumSimpl <- decisionsrules[which(varDecisions == j)] 
            down <- which(substr(rulesNumSimpl,1,1) == ">")
            newLib <- paste0("", substr(rulesNumSimpl[down], 4, nchar(rulesNumSimpl[down])),
                             " <= <b>", j, "</b> < ", substr(rulesNumSimpl[-down], 3, 
                                                             nchar(rulesNumSimpl[-down])))
            decisionsrules <- decisionsrules[-which(varDecisions == j)]
            varDecisions <- varDecisions[-which(varDecisions == j)]
            varDecisionsOrder <- varDecisionsOrder[-which(varDecisionsOrder == j)]
            varDecisionsOrder <- c(varDecisionsOrder, j)
            varDecisions <- c(varDecisions, "")
            decisionsrules <- c(decisionsrules, newLib)
          }
        }
        varDecisions <- varDecisions[match(varDecisionBegin, varDecisionsOrder )]
        decisionsrules <- decisionsrules[match(varDecisionBegin, varDecisionsOrder )]
      }
      tooltipRules[[i]] <- paste0(paste("<b>",varDecisions, "</b>", decisionsrules), collapse = "<br>")
    }
  }
  
  # ------------------------------
  # Terminal nodes colors
  ind_terminal <- which(nodes_var == "<leaf>")
  if(!is.null(attributes(object)$ylevels)){
    # Classification tree
    listColorY <- .generateYColor(object, colorY, nodes_var, digits = digits, infoClass = infoClass, probs = probs)
    
    colNodClust <- as.character(listColorY$colorY$color[match(listColorY$vardecidedClust, listColorY$colorY$modality)])
    nodes_color[ind_terminal] <- colNodClust[ind_terminal]
    nodes_var[ind_terminal] <- listColorY$vardecidedClust[ind_terminal]
    
  }else{
    # regression tree
    listColorY <- .generateYColor(object, colorY, nodes_var, digits = digits)
    
    # for legend color
    colorMin <-  grDevices::rgb(listColorY$colRamp(0), maxColorValue=255)
    colorMax <-   grDevices::rgb(listColorY$colRamp(1), maxColorValue=255)
    
    # terminal nodes
    nodes_color[ind_terminal] <- listColorY$colorTerm[ind_terminal]
    classTerminal <- round(object$frame$yval, digits)
    nodes_var[ind_terminal] <- listColorY$vardecidedClust[ind_terminal]
    
    # cluster
    colNodClust <- listColorY$colorTerm
  }
  
  if(rules) {
    finalHtmlRules <-  paste0('<hr class="rPartvisNetwork"><div class="rPartvisNetworkTooltipShowhim" style="color:blue;">
          <U>RULES</U><div class="rPartvisNetworkTooltipShowme" style="color:black;">', tooltipRules,'</div></div>')
  }else{
    finalHtmlRules <- ""
  }
  
  finalNodesTooltip <- paste0(
    '<div style="text-align:center;">', "N% : <b>",
    round(object$frame$n/object$frame$n[1],digits)*100, 
    "%</b> (", object$frame$n,")<br>", "Complexity : <b>",
    round(object$frame$complexity, digits),
    "</b><br>", statsNodes,
    ifelse(!unlist(lapply(tooltipRules, is.null)), finalHtmlRules, ""), '</div>')
  
  # ------------------------------
  # Nodes size on population
  value = object$frame$n
  if(nodesPopSize){
    minNodeSize = minNodeSize
    maxNodeSize = maxNodeSize
  }else{
    minNodeSize = (minNodeSize + maxNodeSize) / 2
    maxNodeSize = minNodeSize
  }
  
  # ------------------------------
  # Legend
  legendX <- lapply(SortLabel[SortLabel != "<leaf>"], function(x){
    col <- as.character(colorVar$color[match(x, colorVar$variable)])
    list(label = x, color = col, shape = shapeVar, size = legendNodesSize, 
         Leaf = 0, font.size = legendFontSize)
  })
  
  legendY <- lapply(infoClass, function(X){
    # if(is.null(colorY)){
    #   col <- colorTerm[which(infoClass== X)]
    # }else{
    col <- as.character(listColorY$colorY$color[match(X, listColorY$colorY$modality)])
    # }
    list(label = X, color = col, shape = shapeY, size = legendNodesSize, 
         Leaf = 1, font.size = legendFontSize)
  })
  
  legendFinal <- do.call(rbind,(lapply(c(legendX, legendY), data.frame)))
  if(!is.null(legendFinal)){
    legendFinal$id <- 10000:(10000 + (nrow(legendFinal))-1)
  }
  
  # ------------------------------
  # Final data for visNetwork
  nodes <- data.frame(id = as.numeric(rpartNodesNames), label =nodes_var,
                      level = level, color = nodes_color, value = value,
                      shape = shape, title = finalNodesTooltip, fixed = TRUE,
                      colorClust = colNodClust, labelClust = listColorY$vardecidedClust, Leaf = 0,
                      font.size = nodesFontSize, scaling.min = minNodeSize, scaling.max = maxNodeSize)
  
  nodes$Leaf[ind_terminal] <- 1
  if(fallenLeaves){
    nodes$level[which(nodes$shape %in% shapeY)] <- max(nodes$level)
  }
  
  if(length(rpartNodesNames) > 1){
    smooth <- list(enabled = TRUE, type = "cubicBezier", roundness = 0.5)
    edges <- data.frame(id = paste0("edge", 1:length(from)),from = from, to = to, label = edgesLabels, 
                        value = nodes_pop, title = edgesTooltip, color = colorEdges,
                        font.size = edgesFontSize, font.align = edgesFontAlign, smooth = smooth)
  } else {
    edges <- NULL
  }
  
  tree <- visNetwork(nodes = nodes, edges = edges, height = height, width = width, main = main,
                     submain = submain, footer = footer) %>% 
    visHierarchicalLayout(direction = direction) %>%
    visOptions(highlightNearest =  highlightNearest, collapse = collapse) %>% 
    visInteraction(tooltipDelay = tooltipDelay,
                   dragNodes = FALSE, selectConnectedEdges = FALSE,
                   tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;
                      white-space: nowrap;
                      font-family: cursive;font-size:12px;font-color:purple;background-color: #E6E6E6;
                      border-radius: 15px;') %>% 
    visEdges(scaling = list(label = list(enabled = FALSE))) %>%
    visEvents(type = "once", stabilized = "function() { 
        this.setOptions({layout:{hierarchical:false}, physics:{solver:'barnesHut', enabled:true, stabilization : false}, nodes : {physics : false, fixed : true}});
    }")
  
  if(!is.null(legendFinal)){
    tree <- visLegend(tree, addNodes = legendFinal, useGroups = FALSE, enabled = legend,
                      width = legendWidth, ncol = legendNcol, position = legendPosition)
  }
  
  # rajout informations class tree
  tree$x$tree <- list(updateShape = updateShape, shapeVar = shapeVar, 
                      shapeY = shapeY, colorVar = colorVar, colorY = listColorY)
  
  
  if(export){
    tree <- tree%>%visExport()
  }
  tree
}

.visUpdateTree <- function(graph, updateShape = NULL, shapeVar = NULL, shapeY = NULL){
  
  if(!any(class(graph) %in% "visNetwork_Proxy")){
    stop("Can't use visFocus with visNetwork object. Only within shiny & using visNetworkProxy")
  }
  
  tree <- list()
  tree$updateShape <- updateShape
  tree$shapeVar <- shapeVar
  tree$shapeY <- shapeY
  
  data <- list(id = graph$id, tree = tree)
  
  graph$session$sendCustomMessage("visShinyUpdateTree", data)
  
  graph
}

#Legend regression tree gradient color, still in dev
# ' <div style= "background: red;
# background: -webkit-linear-gradient(colorMax,',',colorMin,');
#                        background: -o-linear-gradient(colorMax,',',colorMin,');
#                        background: -moz-linear-gradient(colorMax,',',colorMin,');
#                        background: linear-gradient(colorMax,',',colorMin,');">Test gradient color</div>'
# ,



.parent <- function(x) {
  if (x[1] != 1) {
    c(Recall(if (x %% 2 == 0L) x / 2 else (x - 1) / 2), x)
  } else {
    x
  }
}

.vis_give_rules <- function (object)
{
  frame <- object$frame
  ruleNums <- as.numeric(row.names(frame))
  is.leaf <- (frame$var == "<leaf>")
  frame[!is.leaf, "order"] <- seq_along(which(!is.leaf))
  TF <- as.numeric(row.names(frame))[-1]%%2==0
  ret <- ifelse(TF,
                as.numeric(row.names(frame))[-1]/2,
                (as.numeric(row.names(frame))[-1] - 1)/2)
  ordeR <- frame[as.character(ret),"order"]
  ret <- ifelse(TF, paste0("L", ordeR), paste0("R", ordeR))
  
  ret <- c("No", ret)
  rpartNodesNames <- as.numeric(row.names(frame))
  
  out <- ret
  names(out) <- paste0("Node", rpartNodesNames)
  return(out)
}

.rpart_lists <- function (object)
{
  ff <- object$frame
  n <- nrow(ff)
  if (n == 1L)
    return("root")
  is.leaf <- (ff$var == "<leaf>")
  whichrow <- !is.leaf
  vnames <- ff$var[whichrow]
  index <- cumsum(c(1, ff$ncompete + ff$nsurrogate + (!is.leaf)))
  irow <- index[c(whichrow, FALSE)]
  ncat <- object$splits[irow, 2L]
  lsplit <- rsplit <- list()
  if (any(ncat < 2L)) {
    jrow <- irow[ncat < 2L]
    cutpoint <- object$splits[jrow, 4L]
    temp1 <- (ifelse(ncat < 0, "<", ">="))[ncat < 2L]
    temp2 <- (ifelse(ncat < 0, ">=", "<"))[ncat < 2L]
    lsplit[ncat < 2L] <- cutpoint
    rsplit[ncat < 2L] <- cutpoint
  }
  if (any(ncat > 1L)) {
    xlevels <- attr(object, "xlevels")
    jrow <- seq_along(ncat)[ncat > 1L]
    crow <- object$splits[irow[ncat > 1L], 4L]
    cindex <- (match(vnames, names(xlevels)))[ncat > 1L]
    lsplit[jrow] <- lapply(seq_along(jrow), function(i) xlevels[[cindex[i]]][object$csplit[crow[i],
                                                                                           ] == 1L])
    rsplit[jrow] <- lapply(seq_along(jrow), function(i) xlevels[[cindex[i]]][object$csplit[crow[i],
                                                                                           ] == 3L])
  }
  lsplit <- lapply(seq_along(lsplit), function(i) structure(lsplit[[i]],
                                                            compare = ifelse(ncat[i] < 2L, ifelse(ncat[i] < 0, "<",
                                                                                                  ">="), "=")))
  rsplit <- lapply(seq_along(lsplit), function(i) structure(rsplit[[i]],
                                                            compare = ifelse(ncat[i] < 2L, ifelse(ncat[i] < 0, ">=",
                                                                                                  "<"), "=")))
  names(lsplit) <- vnames
  names(rsplit) <- vnames
  results <- list(L = lsplit, R = rsplit)
  return(results)
}

.generateVarColor <- function(colorVar, nodes_var, SortLabel){
  if(is.null(colorVar)){
    colorVar <- data.frame(variable = unique(nodes_var), color = grDevices::hcl(seq(0, 250, length = length(unique(nodes_var))), l = 80))
  }else{
    if("data.frame" %in% class(colorVar)){
      unused_var <- setdiff(colorVar$variable, setdiff(SortLabel, "<leaf>"))
      if(length(unused_var) > 0){
        colorVar <- colorVar[-which(colorVar$variable %in% unused_var), ]
      }
      miss_var <- setdiff(setdiff(SortLabel, "<leaf>"), colorVar$variable)
      if(length(miss_var) > 0){
        tmp_color <- setdiff(grDevices::hcl(seq(0, 250, length = nrow(colorVar) + length(miss_var)), l = 80), colorVar$color)
        miss_color <- data.frame(variable = miss_var, 
                                 color = tmp_color[1:length(unique(miss_var))])
        colorVar <- rbind.data.frame(colorVar, miss_color)
      }
      
    }else if("character" %in% class(colorVar)){
      colorVar <- data.frame(variable = setdiff(SortLabel, "<leaf>"), 
                             color = rep(colorVar, length(SortLabel))[1:length(setdiff(SortLabel, "<leaf>"))])
    }
  }
  colorVar
}

.generateYColor <- function(object, colorY, nodes_var, digits = 3, infoClass = NULL, probs = NULL){
  
  if(!is.null(attributes(object)$ylevels)){
    if(is.null(infoClass)){
      infoClass <- attributes(object)$ylevels
    }
    if(is.null(probs)){
      probaClass <- object$frame[,"yval2"]
      nlevelsClass <- length(infoClass)
      effectif <- data.frame(probaClass[,2:(nlevelsClass+1), drop = F])
      probs <- data.frame(probaClass[,(nlevelsClass+2):(ncol(probaClass)-1), drop = F])
    }
    
    # Classification tree
    vardecidedClust <- infoClass[apply(probs, 1, which.max)]
    if(is.null(colorY)){
      colorY <- data.frame(modality = unique(infoClass),
                           color = grDevices::hcl(seq(250, 360, length = length(unique(infoClass))), l = 60))
    }else{
      if("data.frame" %in% class(colorY)){
        miss_y <- setdiff(infoClass, colorY$modality)
        if(length(miss_y) > 0){
          miss_color <- data.frame(modality = miss_y, 
                                   color = grDevices::hcl(seq(250, 360, length = length(unique(miss_y))), l = 60))
          colorY <- rbind.data.frame(colorY, miss_color)
        }
      }else if("character" %in% class(colorY)){
        colorY <- data.frame(modality = infoClass, 
                             color = rep(colorY, length(infoClass))[1:length(infoClass)])
      }
    }
    list(colorY = colorY, vardecidedClust = vardecidedClust)
  } else {
    # Regression tree
    vardecidedClust <- round(object$frame$yval, digits)
    
    # palette
    if(length(row.names(object$frame)) > 1){
      meanV <- object$frame$yval-min(object$frame$yval)
      meanV <- meanV/max(meanV)
    } else {
      meanV <- 1
    }
    colRamp <- .creatColorRampY(colorY)
    colorTerm <- grDevices::rgb(colRamp(meanV), maxColorValue=255)
    
    if(is.null(colorY)){
      colorY <- c("#E6E0F8", "#8904B1")
    } else if(length(colorY) > 1){
      colorY <- c(colorY[1],colorY[2])
    } else {
      colorY <- c(NA,colorY[1])
    }
    list(colRamp = colRamp, colorTerm = colorTerm, colorY = colorY, vardecidedClust = vardecidedClust)
  }
}

.creatColorRampY <- function(colorY)
{
  if(is.null(colorY))
  {
    colRamp <- grDevices::colorRamp(c("#E6E0F8", "#8904B1"))
  }else{
    if(length(colorY) > 1){
      colRamp <- grDevices::colorRamp(c(colorY[1],colorY[2]))
    } else {
      colRamp <- grDevices::colorRamp(c(NA,colorY[1]))
    }
  }
  colRamp
}


#' Run and edit a visTree, and get back in R
#'
#' Packages : shiny, rpart, colourpicker, shinyWidgets
#' 
#' @param  data  \code{rpart or data.drame}
#' @param  ...  all arguments except \code{object} present in \link{visTree}
#' 
#' @examples
#' \dontrun{
#' 
#' net <- visTreeEditor(data = iris)
#' net <- visTreeEditor(data = rpart(iris), main = "visTree Editor")
#' net
#' 
#' }
#' 
#' @export
#' 
#' @importFrom  utils packageVersion
#' 
#' 
visTreeEditor <- function(data, ...){

  if(!requireNamespace("shiny")){
    stop("visTreeModule require 'shiny' package")
  } else {
    if(packageVersion("shiny") < '1.0.0'){
      stop("visTreeModule require 'shiny' 1.0.0 or more")
    }
  }
  
  if(!requireNamespace("colourpicker")){
    stop("visTreeModule require 'colourpicker' package")
  }
  
  if(!requireNamespace("shinyWidgets")){
    stop("visTreeModule require 'shinyWidgets' package")
  }
  
  if(!requireNamespace("rpart")){
    stop("visTreeModule require 'rpart' package")
  }
  
  if("rpart" %in% class(data)){
    rpartParams <- FALSE
  } else if("data.frame" %in% class(data)){
    rpartParams <- TRUE
  }
  return(shiny::runApp(shiny::shinyApp(ui = shiny::fluidPage(
    visTreeModuleUI(id = "visTreeEditor", rpartParams = rpartParams, visTreeParams = TRUE, quitButton = TRUE)), 
    server = function(input, output, session) {
      shiny::callModule(visTreeModuleServer, id = "visTreeEditor" ,data = shiny::reactive(data), ...)
    })))
}


# 
# # object =rpart(Species~., data=iris,control = rpart.control(cp = 0.02))
# object <- rpart(Petal.Length~., data=iris, control = rpart.control(cp = 0.02))
# object <- rpart(Species~., data=iris, control = rpart.control(cp = 1))

# object <- rpart(Solder~., data = solder, control = rpart.control(cp = 0.005))
# # object <- rpart(Opening~., data = solder, control = rpart.control(cp = 0.005))
# # # # #
# # # object =rpart(Petal.Length~., data=iris)
# main = ""
# submain = ""
# footer = ""
# direction = "UD"
# fallenLeaves = FALSE
# rules = TRUE
# simplifyRules = TRUE
# shapeVar = "dot"
# shapeY = "square"
# colorVar = NULL
# colorY = NULL
# # 
# # colorVar <- data.frame(variable = names(solder),
# #   color = c("#339933", "#b30000","#4747d1","#88cc00", "#9900ff","#247856"))
# # colorVar <- colorVar[1:2,]
# # # colorY <- data.frame(modality = unique(solder$Opening),
# # #  color = c("#AA00AA", "#CDAD15", "#213478"))
# # # colorY <- c("red", "green")
# # 
# colorEdges = "#8181F7"
# nodesFontSize = 16
# edgesFontSize = 14
# edgesFontAlign = "horizontal"
# legendNodesSize = 22
# legendFontSize = 16
# legend = TRUE
# legendWidth = 0.1
# legendNcol = 1
# nodesPopSize = FALSE
# minNodeSize = 15
# maxNodeSize = 30
# highlightNearest =  list(enabled = TRUE,
#                          degree = list(from = 50000, to = 0), hover = TRUE,
#                          algorithm = "hierarchical")
# collapse = list(enabled = TRUE, fit = TRUE, resetHighlight = TRUE,
#                 clusterOptions = list(fixed = TRUE, physics = FALSE))
# updateShape = TRUE
# tooltipDelay = 500
# digits = 3
# height = "500px"
# width = "100%"
# export = T
# # 
# # # # r <- rpart(carat ~ cut+color +clarity+ depth+ table +price, data = diamonds, control = rpart.control(cp = 0))
# # # # r