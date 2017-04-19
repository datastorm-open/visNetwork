#' Plotting rpart objects
#' 
#' Function to plot rpart objects
#' 
#' @param object \code{rpart}, rpart object
#' @param main \code{character}, title
#' @param direction \code{character}, The direction of the hierarchical layout.
#' The available options are: UD, DU, LR, RL. To simplify:
#' up-down, down-up, left-right, right-left. Default UD
#' @param nodesPopSize \code{boolean}, nodes sizes depends on population ?
#' @param fallenLeaves \code{boolean} position the leaf nodes at the bottom of the graph ? Default to FALSE
#' @param nodesFontSize \code{numeric}, size of labels of nodes. Defaut to 16
#' @param edgesFontSize \code{numeric}, size of labels of edges Defaut to 14
#' @param legendFontSize \code{numeric}, size of labels of nodes in legend. Defaut to 16
#' @param legendNodesSize \code{numeric}, size of nodes in legend. Defaut to 22
#' @param edgesFontAlign \code{character}, for edges only. Default tp 'horizontal'. Possible options: 'horizontal' (Defaut),'top','middle','bottom'. The alignment determines how the label is aligned over the edge.
#' @param colorVar \code{data.frame} 2 columns :
#' \itemize{
#'   \item{"variable"}{ : with names of variables X}
#'   \item{"color"}{ : with colors (in hexa)}
#' }
#' @param colorY if classification tree\code{data.frame} 2 columns :
#' \itemize{
#'   \item{"modality"}{ : with levels of Y}
#'   \item{"color"}{ : with colors (in hexa)}
#' }
#' if regression tree \code{character}, 2 colors in hexa
#' @param colorEdges \code{character} color of edges, in hexa. Default #8181F7
#' @param legend \code{boolean}, add legend ? Default TRUE
#' @param legendWidth \code{numeric}, legend width, between 0 and 1. Default 0.1
#' @param legendNcol \code{numeric}, number of column for legend. Default 1
#' @param highlightNearest \code{list}, Highlight nearest nodes. See \link{visOptions}
#' @param collapse \code{list}, collapse or not using double click on a node ? See \link{visOptions}
#' @param updateShape \code{boolean}, in case of collapse, udpate cluster node shape as terminal node ?
#' @param tooltipDelay \code{numeric}, delay before tooltips apparition in millisecond. Default 500
#' @param rules \code{boolean}, add rules in tooltips ? Default TRUE
#' @param digits \code{numeric}, number of digits. Defaut to 3
#' @param height \code{character}, defaut to "500px"
#' @param width \code{character}, defaut to "100\%"
#' @param minNodeSize \code{numeric}, in case of \code{nodesPopSize}, minimum size of a node. Defaut to 15.
#' @param maxNodeSize \code{numeric}, in case of \code{nodesPopSize}, maximum size of a node. Defaut to 30.
#' @param simplifyRules \code{boolean}, simplify rules writing
#' @param shapeVar \code{character}, shape for variables nodes See \link{visNodes}
#' @param shapeY \code{character}, shape for terminal nodes See \link{visNodes}
#' 
#' @return a visNetwork object 
#' 
#' @examples
#' 
#' \dontrun{
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
#' res <- rpart(Opening~., data=solder, control = (rpart.control(cp = 0.00005)))
#' visTree(res, height = "800px", nodesPopSize = TRUE, minNodeSize = 10, maxNodeSize = 30)
#' 
#' # ----- Options
#' res <- rpart(Opening~., data=solder, control = (rpart.control(cp = 0.005)))
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
#' # Change color
#' colorVar <- data.frame(variable = names(solder), 
#'  color = c("#339933", "#b30000","#4747d1","#88cc00", "#9900ff","#247856"))
#'  
#' colorY <- data.frame(modality = unique(solder$Opening), 
#'  color = c("#AA00AA", "#CDAD15", "#213478"))
#' 
#' visTree(res, colorEdges = "#000099", colorVar = colorVar, 
#'         colorY = colorY)
#' 
#' }
#' 
#' @export
#' @importFrom grDevices hcl
#' @importFrom grDevices colorRamp
#' @importFrom grDevices rgb
#' 
visTree <- function(object,
                    main = "",
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
                    legendNodesSize = 22,
                    legendFontSize = 16,
                    legend = TRUE,
                    legendWidth = 0.1,
                    legendNcol = 1,
                    nodesPopSize = FALSE,
                    minNodeSize = 15,
                    maxNodeSize = 30,
                    highlightNearest =  list(enabled = TRUE,
                                             degree = list(from = 50000, to = 0), hover = TRUE,
                                             algorithm = "hierarchical"),
                    collapse = list(enabled = TRUE, fit = TRUE, resetHighlight = TRUE, 
                                    clusterOptions = list(fixed = TRUE, physics = FALSE)),
                    updateShape = TRUE,
                    tooltipDelay = 500,
                    digits = 3, 
                    height = "500px",
                    width = "100%"){
  
  stopifnot("rpart"%in%class(object))
  stopifnot("character"%in%class(main))
  stopifnot("character"%in%class(direction))
  stopifnot(direction%in% c("UD", "LR", "RL", "DU"))
  stopifnot(length(direction) == 1)
  stopifnot("logical"%in%class(nodesPopSize))
  if(!is.null(minNodeSize))stopifnot("numeric"%in%class(minNodeSize)|"integer"%in%class(minNodeSize))
  if(!is.null(maxNodeSize))stopifnot("numeric"%in%class(maxNodeSize)|"integer"%in%class(maxNodeSize))
  stopifnot("logical"%in%class(fallenLeaves))
  stopifnot("logical"%in%class(simplifyRules))
  stopifnot("numeric"%in%class(nodesFontSize)|"integer"%in%class(nodesFontSize))
  stopifnot("numeric"%in%class(edgesFontSize)|"integer"%in%class(edgesFontSize))
  stopifnot("numeric"%in%class(legendFontSize)|"integer"%in%class(legendFontSize))
  stopifnot("character"%in%class(edgesFontAlign))
  
  if(!is.null(colorVar))stopifnot("data.frame"%in%class(colorVar))
  if(object$method == "class")
  {
    if(!is.null(colorY))stopifnot("data.frame"%in%class(colorY))
  }
  if(object$method == "anova")
  {
    if(!is.null(colorY))stopifnot("character"%in%class(colorY))
  }
  if(!is.null(colorEdges))stopifnot("character"%in%class(colorEdges))
  stopifnot("logical"%in%class(legend))
  stopifnot("numeric"%in%class(legendWidth)|"integer"%in%class(legendWidth))
  stopifnot("numeric"%in%class(legendNcol)|"integer"%in%class(legendNcol))
  stopifnot("list"%in%class(highlightNearest))
  stopifnot("list"%in%class(collapse))
  stopifnot("numeric"%in%class(tooltipDelay)|"integer"%in%class(tooltipDelay))
  stopifnot("logical"%in%class(rules))
  stopifnot("numeric"%in%class(digits)|"integer"%in%class(digits))
  stopifnot("character"%in%class(height))
  stopifnot("character"%in%class(width))
  stopifnot("character"%in%class(shapeVar))
  stopifnot("character"%in%class(shapeY))
  
  # terminal <- object$frame$var=="<leaf>"
  rowNam <- row.names(object$frame)
  
  tt <- sapply(as.numeric(rowNam[2:length(rowNam)]), function(X){
    re <- .parent(X)
    list(re[length(re)-1], re[length(re)], length(re))
  })
  
  from <- unlist(tt[1,])
  to <- unlist(tt[2,])
  level <- c(1, unlist(tt[3,]))
  clas <- NULL
  parentsDec <- list(lapply(rowNam, function(X)(.parent(as.numeric(X)))))
  res2 <- object$frame$var
  te <- .vis_give_rules(object)
  tt <- .rpart_lists(object)
  vv <- attributes(object$terms)$term.labels
  ww <- attributes(object$terms)$dataClasses
  ww <- ww[names(ww)%in%vv]
  decision <- NULL
  decisionLabels <- NULL
  for(i in 1:length(to)){
    re <- strsplit(te[paste0("Nam", to[i])], ",")[[1]]
    sens <- substr(re, 1, 1)
    num <- as.numeric(substr(re, 2, nchar(re)))
    regl <- tt[[sens]][[num]]
    comp <- attributes(regl)$compare
    if(names(comp)%in%names(ww[which(ww%in%c("factor", "character", "ordered"))])){
      comp <- ""
    }else{
      regl <- round(regl, digits)
    }
    
    decision <- c(decision, paste(comp, regl, collapse = ","))
    
    decisionLabels <- c(decisionLabels,
                        paste0('<div style="text-align:center;"><b>', names(attr(regl, "compare")),  "</b></div>", 
                               paste0('<div style="text-align:center;">', comp, regl, "</div>", collapse = "")))
  }
  
  decision2 <- decision
  Truncc <- function(VV)ifelse(nchar(VV)>10,paste0(substr(VV, 1, 7), "..."), VV)
  decision <- sapply(decision, Truncc)
  eff <- object$frame$n[match(to, rowNam)]
  vardecided <- ifelse(res2 != "<leaf>", as.character(res2), "Terminal")
  shape <- ifelse(res2 != "<leaf>", shapeVar, shapeY)
  SortLabel <-  sort(unique(vardecided))
  if(is.null(colorVar)){
    color <- .generateVarColor(vardecided = vardecided, SortLabel = SortLabel)
    colNod <- color[match(vardecided, SortLabel)]
  }else{
    colNod <- as.character(colorVar$color[match(vardecided, colorVar$variable)])
  }
  probsend <- NULL
  if(!is.null(attributes(object)$ylevels))
  {
    #Classification TREE
    clas <- attributes(object)$ylevels
    nbClas <- length(clas)
    VAL <- object$frame[,"yval2"]
    effectif <- VAL[,2:(nbClas+1)]
    effectif <- data.frame(effectif)
    probs <- VAL[,(nbClas+2):(ncol(VAL)-1)]
    probs <- data.frame(probs)
    probs2 <- data.frame(probs)
    for(i in 1:length(clas)){
      probs[,i] <- paste0(clas[i], " : <b>",
                          round(probs[,i], digits)*100, "%</b>",
                          " (", effectif[,i], ")")
    }
    totDta <- cbind(effectif, probs)
    probsend <- apply(probs, 1, function(X){paste0(X,
                                                   collapse = "<br>")})
    probsend <- paste('<hr>', probsend)
  }else{
    #Regression TREE
    varianceNodes <- round(object$frame$dev/(object$frame$n - 1),digits)
    varianceNodes[which(varianceNodes == Inf)] <- NA
    probsend <- paste0("Mean : <b>" ,
                       round(object$frame$yval,digits),
                       "</b><br>Variance : <b>",varianceNodes, "</b>")
  }
  returndec <- list()
  for(i in 2:length(parentsDec[[1]])){
    use <- parentsDec[[1]][[i]]
    varDecisions <- vardecided[match(as.character(use[-length(use)]), rowNam)]
    decisionsrules <- decision2[match(as.character(use), rowNam)-1]
    varDecisionBegin <- unique(varDecisions)
    if(simplifyRules)
    {
      
      filtre <- paste0(varDecisions, substr(decisionsrules, 1 ,1))
      tabFiltre <- table(filtre)>1
      if(length(which(tabFiltre))>0)
      {
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
      if(length(which(tabFiltre))>0)
      {
        filtres <- names(tabFiltre)[which(tabFiltre)]
        for(j in filtres){
          rulesNumSimpl <-decisionsrules[which( varDecisions== j)] 
          down <- which(substr(rulesNumSimpl,1,1)==">")
          newLib <- paste0("",substr(rulesNumSimpl[down],4,nchar(rulesNumSimpl[down])),
                           " <= <b>", j, "</b> < ", substr(rulesNumSimpl[-down],3,nchar(rulesNumSimpl[-down])))
          decisionsrules <- decisionsrules[-which(varDecisions== j)]
          varDecisions <- varDecisions[-which(varDecisions== j)]
          varDecisionsOrder <- varDecisionsOrder[-which(varDecisionsOrder== j)]
          varDecisionsOrder <- c(varDecisionsOrder, j)
          varDecisions <- c(varDecisions, "")
          decisionsrules <- c(decisionsrules, newLib)
        }
      }
      varDecisions <- varDecisions[match(varDecisionBegin,varDecisionsOrder )]
      decisionsrules <- decisionsrules[match(varDecisionBegin,varDecisionsOrder )]
    }
    
    returndec[[i]] <- paste0(paste("<b>",varDecisions,"</b>"
                                   ,decisionsrules
    ),
    collapse = "<br>")
  }
  
  ind_terminal <- which(vardecided=="Terminal")
  if(!is.null(attributes(object)$ylevels))
  {
    #Classification tree
    vardecidedClust <- clas[apply(probs2, 1, which.max)]
    
    if(is.null(colorY)){
      colorTerm <- .generateYColor(clas)
      colNodClust <- colorTerm[match(vardecidedClust, clas)]
      colNod[ind_terminal] <- colNodClust[ind_terminal]
    }else{
      colNodClust <- as.character(colorY$color[match(vardecidedClust, colorY$modality)])
      colNod[ind_terminal] <- colNodClust[ind_terminal]
    }
    
    vardecided[ind_terminal] <- vardecidedClust[ind_terminal]
    
  }else{
    #Regression tree
    
    # label as terminal nodes for all
    vardecidedClust <- round(object$frame$yval,digits)
    
    # palette
    meanV <- object$frame$yval-min(object$frame$yval)
    meanV <- meanV/max(meanV)
    
    colRamp <- .creatColorRampY(colorY)
    colorTerm <- rgb(colRamp(meanV), maxColorValue=255)
    # for legend color
    colorMin <-  rgb(colRamp(0), maxColorValue=255)
    colorMax <-   rgb(colRamp(1), maxColorValue=255)
    # terminal nodes
    colNod[ind_terminal] <- colorTerm[ind_terminal]
    classTerminal <- round(object$frame$yval, digits)
    vardecided[ind_terminal] <- vardecidedClust[ind_terminal]
    
    # cluster
    colNodClust <- colorTerm
  }
  
  if(rules) 
  {
    rul <-  paste0('<hr class="rPartvisNetwork"><div class="rPartvisNetworkTooltipShowhim" style="color:blue;"><U>RULES</U>
                   <div class="rPartvisNetworkTooltipShowme" style="color:black;">',
                   returndec,'</div></div>')
  }else{
    rul <- ""
  }
  
  labelsNode <- paste0(
    '<div style="text-align:center;">', "N% : <b>",
    round(object$frame$n/object$frame$n[1],digits)*100
    , "%</b> (", object$frame$n,")<br>",
    "Complexity : <b>",
    round(object$frame$complexity, digits),
    "</b><br>",
    probsend,
    ifelse(!unlist(lapply(returndec, is.null)),
           rul
           , ""),
    '</div>'
  )
  
  legendVisVar <- sapply(SortLabel[SortLabel!="Terminal"], function(X){
    if(is.null(colorVar))
    {
      col <- color[which(SortLabel== X)]
    }else{
      col <- as.character(colorVar$color[match(X, colorVar$variable)])
    }
    list(label = X, color = col, shape = "dot", size = legendNodesSize, font = list(size = legendFontSize))
  }, simplify = FALSE, USE.NAMES = FALSE)
  
  legendVisRep <- sapply(clas, function(X){
    if(is.null(colorY))
    {
      col <- colorTerm[which(clas== X)]
    }else{
      col <- as.character(colorY$color[match(X, colorY$modality)])
    }
    list(label = X, color = col, shape = "square", size = legendNodesSize, font = list(size = legendFontSize))
  }, simplify = FALSE, USE.NAMES = FALSE)
  
  legelDvIS <- c(legendVisVar, legendVisRep)
  legelDvIS <- do.call(rbind,(lapply(legelDvIS, data.frame)))
  
  smooth <- list(enabled = TRUE, type = "cubicBezier", roundness = 0.5)
  if(nodesPopSize){
    value = object$frame$n
    
    if(is.null(minNodeSize)){
      minNodeSize = min(object$frame$n)
    }
    if(is.null(maxNodeSize)){
      maxNodeSize = max(object$frame$n)
    }
    
  }else{
    value = 1
    if(is.null(minNodeSize)){
      minNodeSize = 25
    }
    if(is.null(maxNodeSize)){
      maxNodeSize = 25
    }
  }
  
  legelDvIS$id <- 10000:(10000 + (nrow(legelDvIS))-1)
  nodes <- data.frame(id = as.numeric(rowNam), label =vardecided,
                      level = level, color = colNod, value = value,
                      shape = shape, title = labelsNode, fixed = TRUE,
                      colorClust = colNodClust, labelClust = vardecidedClust,Leaf = 0) 
  nodes$Leaf[ind_terminal] <- 1
  if(fallenLeaves){
    nodes$level[which(nodes$shape %in%"square")] <- max(nodes$level)
  }
  
  
  edges <- data.frame(id = 1:length(from),from = from,
                      to = to,
                      label = decision,
                      value = eff,
                      title = decisionLabels,
                      color = colorEdges)
  
  net <- visNetwork(nodes = nodes, edges = edges, height = height, width = width, main = main)%>% 
    visHierarchicalLayout(direction = direction) %>%
    visLegend(addNodes = legelDvIS, 
              useGroups = FALSE, enabled = legend,
              width = legendWidth,
              ncol = legendNcol) %>%
    visOptions(highlightNearest =  highlightNearest, collapse = collapse) %>% 
    visInteraction(tooltipDelay = tooltipDelay,
                   dragNodes = FALSE, selectConnectedEdges = FALSE,
                   tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;
                      white-space: nowrap;
                      font-family: cursive;font-size:12px;font-color:purple;background-color: #E6E6E6;
                      border-radius: 15px;') %>% 
    visEdges(font = list(size = edgesFontSize, align = edgesFontAlign), value = 3, smooth = smooth,
             scaling = list(label = list(enabled = FALSE))) %>%
    visNodes(font = list(size = nodesFontSize),
             scaling = list(min = minNodeSize, max = maxNodeSize)) %>%
    visEvents(type = "once", stabilized = "function() { 
        this.setOptions({layout:{hierarchical:false}, physics:{solver:'barnesHut', enabled:true, stabilization : false}, nodes : {physics : false, fixed : true}});
  }")
  
  # rajout informations class tree
  net$x$tree <- list(updateShape = updateShape, shapeVar = shapeVar, shapeY = shapeY)
  
  net
}

#Legend regression tree gradient color, still in dev
# ' <div style= "background: red;
# background: -webkit-linear-gradient(colorMax,',',colorMin,');
#                        background: -o-linear-gradient(colorMax,',',colorMin,');
#                        background: -moz-linear-gradient(colorMax,',',colorMin,');
#                        background: linear-gradient(colorMax,',',colorMin,');">Test gradient color</div>'
# ,



.parent <- function(x) {
  if (x[1] != 1)
    c(Recall(if (x %% 2 == 0L) x / 2 else (x - 1) / 2), x) else x
}

.vis_give_rules <- function (object) 
{
  frame <- object$frame
  ruleNums <- as.numeric(row.names(frame))
  is.leaf <- (frame$var == "<leaf>")
  frame[!is.leaf, "order"] <- seq_along(which(!is.leaf))
  ret <- sapply(as.numeric(row.names(frame))[-1], function(X){
    if (X%%2 == 0) {
      rules <- paste0("L", frame[as.character(X/2), "order"])
    }else{
      rules <- paste0("R", frame[as.character((X - 1)/2), "order"])
    }
    rules
  })
  ret <- c("No", ret)
  rownam <- as.numeric(row.names(frame))
  
  out <- ret
  names(out) <- paste0("Nam", rownam)
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

.generateVarColor <- function(vardecided, SortLabel){
  grDevices::hcl(seq(0, 250, length = length(unique(vardecided))), l = 80)
}

.generateYColor <- function(vardecided){
  grDevices::hcl(seq(250, 360, length = length(unique(vardecided))), l = 60)
}

.creatColorRampY <- function(colorY)
{
  if(is.null(colorY))
  {
    colRamp <- colorRamp(c("#E6E0F8", "#8904B1"))
  }else{
    colRamp <- colorRamp(c(colorY[1],colorY[2]))
  }
  colRamp
}

# object =rpart(Petal.Length~., data=iris)
# main = ""
# direction = "UD"
# fallenLeaves = FALSE
# rules = TRUE
# simplifyRules = TRUE
# shapeVar = "dot"
# shapeY = "square"
# colorVar = NULL
# colorY = NULL
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