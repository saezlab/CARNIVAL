#TODO add n edges, add n measurements
createDummyVariablesSigned <- function(nNode = 3, sign = 1) {
  variables <- c()
  
  variables$nodesDf <- data.frame( nodes=c(paste("node", seq(1, nNode), sep = "")), 
                                   nodesVars=c(paste("n", seq(1, nNode), sep = "")), 
                                   nodesUpVars = c(paste("nU", seq(1, nNode), sep = "")),
                                   nodesDownVars = c(paste("nD", seq(1, nNode), sep = "")),
                                   nodesActStateVars = c(paste("nAc", seq(1, nNode), sep = "")),
                                   nodesDistanceVars = c(paste("nDs", seq(1, nNode), sep = "")),
                                   nodesType = c("P", "", "M") )
  
  variables$edgesDf <- data.frame( Node1=c("node1", "node2"), 
                                   Sign=c(rep(sign, 2)), 
                                   Node2=c("node2", "node3"), 
                                   edgesUpVars=c("eU1", "eU2"), 
                                   edgesDownVars=c("eD1", "eD2") )
  
  #TODO randomize the values
  variables$measurementsDf <- data.frame(nodes = "node3", 
                                         value = 5, 
                                         measurementsVars = "absDiff", 
                                         nodesVars = "n3" )
  return(variables)
}


createDummyVariablesMixed <- function(nNode = 3, seed = 100) {
  
  variables <- c()
  set.seed(seed)
  
  variables$nodesDf <- data.frame(nodes=c(paste("node", seq(1,nNode), sep = "")), 
                                  nodesVars=c(paste("n", seq(1,nNode), sep = "")), 
                                  nodesUpVars = c(paste("nU", seq(1,nNode), sep = "")),
                                  nodesDownVars = c(paste("nD", seq(1,nNode), sep = "")),
                                  nodesActStateVars = c(paste("nAc", seq(1,nNode), sep = "")),
                                  nodesDistanceStateVars = c(paste("nDs", seq(1,nNode), sep = "")),
                                  nodesType = c("P", "", "M", "", "M"))
  
  variables$edgesDf <- data.frame(Node1=c("node1", "node2", "node2", "node4"), 
                                  Sign=sample(c(1,-1), 4, replace = TRUE), 
                                  Node2=c("node2", "node3", "node4", "node5"), 
                                  edgesUpVars=c("eU1", "eU2", "eU3", "eU4"), 
                                  edgesDownVars=c("eD1", "eD2", "eD3", "eD4")
  )
  
  variables$measurementsDf <- data.frame(nodes = "node5", 
                                         value = 5, 
                                         measurementsVars = "absDiff", 
                                         nodesVars = "n3" )
  
  return(variables)
}
