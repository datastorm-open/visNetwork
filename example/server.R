###########################################################################################
#
# Sertver.R
#
# Server component for getting graphs from Neo4j via RNeo4j, modify and store back
#
# author: Hans N. Beck
# version: alpha
#
###########################################################################################
require(shiny)
require(visNetwork)

source("basis.R")
source("graphIO.R")

# nodes data frame
nodes <- globalenv()
# edges date frame
edges <- globalenv()
# a data frame (interpreted as list) of graph change commands
commandList <<- NULL

# general remark: vis.js handle ids and labels
# the label will be mapped to one (selected) node property
# NOTE: on vis.js level, a node has a type, a label and ID
# this type is called "label" at neo4j level
# a neo4j node of a selected label as properties
propDesc <<- vector() # property description


shinyServer(function(input, output, session) {

  # load a graph from neo4j
  updateGraphData <- eventReactive(input$loadButton, {
    nodeTypesFilter <- input$selectNodeTypes
    edgeTypesFilter <- input$selectEdgeTypes
    mapList <-  input$labelPropertyMap
    data <- loadGraph(graph, nodeTypesFilter, edgeTypesFilter, mapList)
    nodes <<- data$n
    edges <<- data$e
    lcc$counter <<-lcc$counter +1
    print ("Graph loaded")
  })

  # apply all change commands (add, delete etc.) to neo4j
  observeEvent(input$updateButton, {

      if (!is.null(commandList))
      {
        # handle nodes first  because neo4j provides new IDs
        # this IDs are different from the one provided by vis.js
        # sort for priority
        sortedCommands <- commandList[order(commandList$prio),]
        # print (sortedCommands)
        # now I can do all other commands, IDs in commandlist and graph are now corrected
        for (i in  1:nrow(sortedCommands))
        {
          c <- sortedCommands[i,]

          if (c$cmd == "addNode")
          {
            data <- addNode(graph, c, nodes, edges, lcc)
            nodes <<- data$nodes
            edges <<-data$edges
          }
          if (c$cmd == "changeNode")
          {
           data <- updateNode(graph, c, nodes, edges)
           nodes <<- data$nodes
           edges <<-data$edges
          }
          if (c$cmd == "addEdge")
          {
            data <- addEdge(graph, c, nodes, edges)
            nodes <<- data$nodes
            edges <<-data$edges
          }
          if (c$cmd == "deleteEdge")
          {
            data <- deleteEdge(graph, c, nodes, edges)
            nodes <<- data$nodes
            edges <<-data$edges
          }
          if (c$cmd == "deleteNode")
          {
            data <- deleteNode(graph, c, nodes, edges)
            nodes <<- data$nodes
            edges <<-data$edges
          }
        }

        lcc$counter <- lcc$counter + 1
        commandList <<- NULL
      }
  })

  # node type filter
  observeEvent(input$selectNodeTypes, {
    nodeTypesFilter = input$selectNodeTypes

    # it may be a single string, but a list is needed in any case
    if (!is.vector(nodeTypesFilter))
      nodeTypesFilter <- c(nodeTypesFilter)

    # set choices
    if (!is.null(nodeTypesFilter))
    {
      updateSelectInput(session, "labelPropertyMap", choices = createSelectList(propDesc,nodeTypesFilter), selected = NULL)
    }
  })

  # handle change events of vis.js layer
  # every change will be stored as command in a data frame
  observeEvent(input$network_graphChange,{
    if (!is.null(input$network_graphChange))
    {
      if (input$network_graphChange$cmd =="changeNode")
      {
        selId <- input$network_graphChange$id
        selLabel <- input$network_graphChange$label
        aCmd <- input$network_graphChange
        aCmd$map <- propertyOfType(input$labelPropertyMap, input$newType)
        aCmd$type <- input$newType

        # check if it is a new node
        if (selId %in% nodes$id)
        {
          # print("changed node")
          nodes$label[nodes$id==selId] <<- selLabel
        }
        else
        {
          newNode <- data.frame(id = selId, label = selLabel)
          nodes <<- rbind(nodes,newNode)
          aCmd$cmd <- "addNode"
        }
        commandList <<-  appendCommand (commandList, aCmd)
      }

      if (input$network_graphChange$cmd == "addEdge")
      {
        print("Command add edge")
        selId <- input$network_graphChange$id
        selFrom <- input$network_graphChange$from
        selTo <- input$network_graphChange$to
        # create command
        aCmd <- input$network_graphChange
        aCmd$map <- NULL
        aCmd$type <- input$newRelation

        # newID <- fetchNewId(deletedEdges, edges)
        # aCmd$id <- newID
        # print(paste("edge id is", selId))
        newEdge <- data.frame(id = selId, from= selFrom, to=selTo)
        edges <<- rbind(edges,newEdge)
        # if redraw/reload necessary uncomment this
        #lcc$counter <- lcc$counter+1
        commandList <<- appendCommand(commandList, aCmd)
      }
      if (input$network_graphChange$cmd == "deleteElements")
      {
        print("Command delete edge")
        # printGraphData(nodes, edges)
        # delete edges
        for (e in input$network_graphChange$edges)
        {
          if (e %in% edges$id)
          {
            aCmd <- list(cmd="deleteEdge", id=e)
            print (paste("To delete edge:", e))
            edges <<- edges[!edges$id == e,]
            # deletedEdges <<- c(deletedEdges, e)
            commandList <<- appendCommand(commandList, aCmd)
          }
        }
        #print (paste("deletedEdges is now ", deletedEdges))
        # delete nodes
        for (n in input$network_graphChange$nodes)
        {
          if (n %in% nodes$id)
          {
            aCmd <- list(cmd="deleteNode", id=n)
            print (paste("To delete node:", n))
            nodes <<- nodes[!nodes$id == n,]
            #deletedNodes <<- c(deletedNodes, n)
            #print (paste("deletedNodes is now ", deletedNodes))
            commandList <<- appendCommand(commandList, aCmd)
          }
        }
        #printGraphData(nodes, edges)
      }
    }
  })

  lcc <- reactiveValues( counter = 0 )

  output$network <- renderVisNetwork({

      #hole graph Beschreibung
      isolate({
        selectedN <- input$selectNodeTypes
        selectedE <- input$selectEdgeTypes
        selectedNewType <- input$newType
        selectedNewRelation <- input$newRelation
        propMap <- input$labelPropertyMap
        data <- loadGraphMeta(graph)
        nodeDesc <- data$nDesc
        edgeDesc <- data$eDesc
        propDesc <<- data$pDesc
        print("Graph meta fetch")
        updateSelectInput(session, "selectNodeTypes", choices = nodeDesc , selected = selectedN)
        updateSelectizeInput(session, "selectEdgeTypes", choices = edgeDesc, selected = selectedE)
        updateSelectizeInput(session, "newType", choices = nodeDesc, selected = selectedNewType)
        updateSelectizeInput(session, "newRelation", choices = edgeDesc, selected = selectedNewRelation)
        lcc$counter <- 0
        })

    updateGraphData()

    lcc$counter #for reactiveness, this counter will be incrementet every time a redraw and reload is necessary

    visNetwork(nodes, edges, legend = TRUE) %>%
      visPhysics(stabilization = FALSE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visOptions(manipulation = TRUE,
                 highlightNearest = TRUE, nodesIdSelection = TRUE) %>% visLayout(improvedLayout = input$improvedLayout)

  })
  output$labelMapping <- renderText({
    input$labelPropertyMap})

  output$modSteps <- renderText({
    lcc$counter})

})
