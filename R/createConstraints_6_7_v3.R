# I coudl not reproduce the result based on the createConstraints_6_7_v2 function
# from https://github.com/saezlab/CARNIVAL/blob/4bc5fc5e4215e33d6aa7f239094306d76a4cebf7/R/constraints_create_constraints_6_7.R
# Thus, here I modified it and validated that it can produce the results same as 
# line 38-46 in the toy_lp_file_ex1.lp file 
##
## Author: Geoffrey(Dingquan) Yu, EMBL-HH, 01-Aug-2021

## Please uncomment codes and change paths to reproduce results 
## 
# setwd("D:/")
# load("./data/toy_measurements_ex1.RData")
# load("./data/toy_network_ex1.RData")
# load("./data/toy_perturbations_ex1.RData")
# nodes<-base::unique(c(toy_network_ex1$source,toy_network_ex1$target))
# idxNodes<-seq(1,length(nodes),1)
# 
# nodesVars<-paste0("n",seq(1:length(nodes)))
# nodesUpVars<-paste0("nU",idxNodes)
# nodesDownVars<-paste0("nD",idxNodes)
# nodesDf<-cbind(nodes,nodesVars,nodesUpVars,nodesDownVars)
# 
# 
# edgesDf<-toy_network_ex1
# names(edgesDf)<-c("Node1","Sign","Node2")
# edgeIdx<-seq(1,nrow(edgesDf),1)
# edgesUpVars<-paste0("eU",edgeIdx)
# edgesDownVars<-paste0("eD",edgeIdx)
# edgesDf$edgesUpVars<-edgesUpVars
# edgesDf$edgesDownVars<-edgesDownVars



createConstraints_6_7_v3<-function(variables){
  edgeDf<-variables$edgesDf
  nodeDf<-variables$nodesDf
  parentNodes<-base::setdiff(edgeDf$Node1,edgeDf$Node2)
  variableMerged<-base::merge(as.matrix(edgeDf),as.matrix(nodeDf),by.x = "Node1",by.y = "nodes")
  parentNodesEdges<-variableMerged[variableMerged$Node1 %in% parentNodes,]
  
  if(length(parentNodes)>0){
    constraint_6<-paste(unique(parentNodesEdges$nodesUpVars),"<=",0)
    constraints_7<-paste(unique(parentNodesEdges$nodesDownVars),"<=",0)
  }else{
    constraints_6<-c()
    constraints_7<-c()
  }
  
  variablesMergedTwoNodes<-merge(as.matrix(edgeDf),as.matrix(nodeDf),by.x = "Node2",by.y = "nodes")
  

  allIncomingEdges<-variablesMergedTwoNodes[variablesMergedTwoNodes$Node2%in%unique(variablesMergedTwoNodes$Node2),]
  
  c6_7_nodeVars<-unique(allIncomingEdges$nodesUpVars)
  for (x in c6_7_nodeVars){
    incomingedges<-allIncomingEdges[allIncomingEdges$nodesUpVars==x,]
    edgesVars<-paste("-",paste(incomingedges$edgesUpVars,collapse = " - "))
    constraintLeft<-paste(x,edgesVars,"<=",0)
    constraint_6<-c(constraint_6,constraintLeft)
  }
  
  c6_7_nodeVars<-unique(allIncomingEdges$nodesDownVars)
  for (x in c6_7_nodeVars){
    incomingedges<-allIncomingEdges[allIncomingEdges$nodesDownVars==x,]
    edgesVars<-paste("-",paste(incomingedges$edgesDownVars,collapse = " - "))
    constraintLeft<-paste(x,edgesVars,"<=",0)
    constraints_7 <- c(constraints_7, constraintLeft)
  }
  constraints6_7<-list(constraint_6,constraints_7)
  names(constraints6_7)<-c("c6","c7")
  return(constraints6_7)
}

variables<-list(nodesDf,edgesDf)
names(variables)<-c("nodesDf","edgesDf")
createConstraints_6_7_v3(variables)
