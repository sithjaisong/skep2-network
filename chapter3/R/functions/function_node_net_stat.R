
# function for analysing network statistics of both node and network level
node_stat <- function(graph){
  abs.graph <- graph
  E(abs.graph)$weight <- abs(E(abs.graph)$weight)
  
node.value <- data.frame(
  
    id = V(graph)$name, # node name
    
    deg = igraph::degree(graph), # degree distribution
    
    bet = igraph::betweenness(abs.graph, normalized = FALSE, directed = FALSE), #, #betweenness centality
    
    tra = igraph::transitivity(graph, type = c("weight")) # cluster coefficients
)
  
  names(node.value) <- c("var", "degree", "betweenness","clust.coef")
  
  row.names(node.value) <- NULL
  
  return(node.value)
}

################################

net_stat <- function(graph){
  
  abs.graph <- graph
  
  E(abs.graph)$weight <- abs(E(abs.graph)$weight)
  
  network.value <- data.frame(
    
    node = vcount(graph), #1.number of nodes
    
    edge = ecount(graph), #2.number of edges
    
    avr.path = average.path.length(graph),
    
    tra = igraph::transitivity(graph, type = "global") #7. transitivity
    
  )
  
  names(network.value) <- c("Node", "Edges", "avg_p", "CC")
  
  row.names(network.value) <- NULL
  
  return(network.value)
}
#eos
