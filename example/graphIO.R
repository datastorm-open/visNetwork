###########################################################################################
#
# GraphIO.R
#
# IO component for getting graphs from Neo4j via RNeo4j, modify and store back
#
# author: Hans N. Beck
# version: alpha
#
###########################################################################################

# note: at this level, we talk about node labels and node properties
# this are terms on neo4j level

# connection to neo4j database
graph = startGraph("http://localhost:7474/db/data")

loadGraph <- function (graph, nodeLabelFilter="", edgeTypesFilter="", labelPropertyMap = NULL)
{
  nodes <- NULL
  # for all required node labels
  for (nl in nodeLabelFilter)
  {
    for (m in labelPropertyMap)
    {
      # disassemble map entries to node label and property name
      aMapEntry = unlist(strsplit(m, "--"))
      if (nl == aMapEntry[1])
      {
        queryDataNodes <- buildNodeQuery(c(nl), aMapEntry[2])
        nodesSet <- cypher(graph, queryDataNodes$nQuery)
        if (is.null(nodes))
        {
          nodes <- nodesSet
        }
        else
        {
          nodes <- merge(x=nodes, y =nodesSet, all=TRUE)
          print("merge done")
        }
      }
    }
  }

  queryDataEdges <-buildEdgeQuery(edgeTypesFilter, nodeLabelFilter)
  edges <- cypher(graph, queryDataEdges$eQuery)

  # printGraphData(nodes, edges)

  # uncomment this for removing nodes without edges
  # nodeKeys = data.frame(id=unique(c(edges$from, edges$to)))
  # nodes <- nodes[nodes$id %in% nodeKeys$id,]
  #  print(nodes)
  # print(edges)

  return (list( n = nodes, e = edges, numNodes = nrow(nodes), numEdges = nrow(edges)))
}

# load meta data (labels in database, properties of node with selected label)
loadGraphMeta <- function(graph)
{
  nQuery = "match (n) return distinct labels(n) as labels"
  eQuery = "match (n)-[r]->(m) return distinct type(r) as types"
  propQuery = "match (n:?) return distinct keys(n) as keys"
  result <- cypher(graph, nQuery)
  nodeDesc <- result$labels
  result <- cypher(graph, eQuery)
  edgeDesc <-  result$types
  print (nodeDesc)
  # for every node label
  if (length(unlist(nodeDesc)>0))
  {
    propDesc <- vector(length = length(nodeDesc))
    names(propDesc) <- nodeDesc
    for (l in nodeDesc)
    {
      aQuery <-gsub("?", l, propQuery, fixed = TRUE)
      result <- cypher(graph, aQuery)
      propDesc[l] <- result$keys[1]
    }
  }
  else
  {
    aQuery = "match (n) return distinct keys(n) as keys"
    result <- cypher(graph, aQuery)
    propDesc["deafult"] <- result$keys[1]
    nodeDesc <- list("Default")
  }

  return (list(nDesc = nodeDesc, eDesc =edgeDesc, pDesc = propDesc))
}

# add a node to neo4j
addNode <- function(graph, aCommand, nodes, edges, lcc)
{
  print("add node to db")
  aNodeID <- aCommand$id
  aNodeContent <- aCommand$label
  aProperty <- aCommand$map
  aNewNodeLabel <- aCommand$type

  aQuery = paste0("match (n) where id(n)=", -1, " return labels(n) as labels")

   # bestimme node type und ob der Knoten überhaupt existiert
  result <- cypher(graph, aQuery)

  # perform only if node doesn't exists already
  if (length(result) == 0)
  {
    # erzeuge knoten
    query = paste0("create (n:", aNewNodeLabel ,"{", aProperty, ": '", aNodeContent, "'}) return id(n) as id")
    print(query)
    result <- cypher(graph, query)
    # print (paste("reuslt ", result))
    # correct the id
    nodes$id[nodes$id==aNodeID] <- result$id
    #correct edges ids
    edges$from[edges$from==aNodeID] <- result$id
    edges$to[edges$to==aNodeID] <- result$id
    # perform id correction in all edge related commands
    # because this commands include the ids of vis.js at this time
    commandList$from[commandList$from == aNodeID] <<- result$id
    commandList$to[commandList$to== aNodeID] <<- result$id
    # print(commandList)
  }

  return (list("nodes" = nodes, "edges"= edges))
}

# updates node data in database
updateNode <- function(graph, aCommand, nodes, edges, propDesc)
{
  print("update node in db")
  aNodeID <- aCommand$id
  aNodeContent <- aCommand$label

  aQuery = paste0("match (n) where id(n)=", aNodeID, " return labels(n) as labels")
  # determine node label of selected node

  result <- cypher(graph, aQuery)
  targetLabel = result$labels[1] # assumption: only one label per node
  #search for the map
  aProperty <- propDesc[targetLabel]
  query = paste0("match (n) where id(n)=", aNodeID, " set n.",aProperty, "='", aNodeContent, "'")
  print (paste ("update query", query))

  # perform update
  result <- cypher(graph, query)

  #result processing ?
  #print (paste("result is", result))

  return (list("nodes" = nodes, "edges"= edges))
}

#add edge
addEdge <- function(graph, aCommand, nodes, edges)
{
  print("add new edge in db")
  aFromID <- aCommand$from
  aToID <- aCommand$to
  aType <- aCommand$type
  aID <- aCommand$id

  # check if edge exists already
  aQuery = paste0("match (n)-[r:", aType, "]-(m) where id(n)=", aFromID, " and id(m) = ", aToID, " return id(r) as id")
  #print(aQuery)
  result <- cypher(graph, aQuery)
  if (length(result) == 0)
  {
    aQuery = paste0("match (n), (m) where id(n)=", aFromID, " and id(m) = ", aToID, " CREATE (n)-[r:", aType, "]->(m) return id(r) as id")
    print(aQuery)
    result <- cypher(graph, aQuery)
    # print(result)

    #correct ids, because database provides new ones
    edges$id[edges$id==aID] <- result$id
  }

  return (list("nodes" = nodes, "edges"= edges))
}

#add edge
deleteEdge <- function(graph, aCommand, nodes, edges)
{
  print("delete edge in db")
  aID <- aCommand$id

  # for neo4j no problem if egde doesn't exist
  aQuery = paste0("match (n)-[r]-(m) where id(r)=", aID, " delete r")
  print(aQuery)
  result <- cypher(graph, aQuery)
  # print(result)

  return (list("nodes" = nodes, "edges"= edges))
}

#add edge
deleteNode <- function(graph, aCommand, nodes, edges)
{
  print("delete node in db")
  aID <- aCommand$id

  # for neo4j no problem if egde doesn't exist
  aQuery = paste0("match (n) where id(n)=", aID, " delete n")
  print(aQuery)
  try({result <- cypher(graph, aQuery)})
  # print(result)

  return (list("nodes" = nodes, "edges"= edges))
}


# create cyhper query for reading nodes
buildNodeQuery <- function(nodeLabels="", aPropertyName)
{
  nodeExpr = "match (n)"
  clauseExpr = buildNodeLabelExpr(nodeLabels)
  query = paste(nodeExpr, clauseExpr ," return id(n) as id, n.? as label, labels(n) as group")
  # query = paste(nodeExpr, clauseExpr ," return id(n) as id, n.? as label")
   if (!is.null(aPropertyName))
    query <- gsub("?", aPropertyName, query, fixed = TRUE)
  else
    query <- gsub("?", "id", query, fixed = TRUE)
  # print (paste("nodequery", query))

  return (list("nQuery"= query, "clauseExpr" = clauseExpr))
}

# create cypher edge query for reading
buildEdgeQuery <- function(edgeTypes, nodeLabels="")
{
  edgeExpr = "match (n)-[r?]->(m) "
  # taking selected node labels into account
  clauseExpr = buildClauseExpr(nodeLabels)
  subExpr = buildRelationExpr(edgeTypes)
  edgeExpr = gsub("?", subExpr, edgeExpr, fixed=TRUE)
  query = paste(edgeExpr, clauseExpr,  "return id(r) as id, id(n) as from, id(m) as to")

  print (paste("edge", query))

  return (list("eQuery"= query, "edgeExpr" = subExpr))
}
