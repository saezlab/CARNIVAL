# I coudl not reproduce the result based on the createConstraints_6_7_v2 function
# from https://github.com/saezlab/CARNIVAL/blob/4bc5fc5e4215e33d6aa7f239094306d76a4cebf7/R/constraints_create_constraints_6_7.R
# Thus, here I modified it and validated that it can produce the results same as 
# line 38-46 in the toy_lp_file_ex1.lp file 
##
## Author: Geoffrey(Dingquan) Yu, EMBL-HH, 01-Jul-2021

## Please uncomment codes and change paths to reproduce results 
## 
# setwd("D:/")
# load("./data/toy_measurements_ex1.RData")
# load("./data/toy_network_ex1.RData")
# load("./data/toy_perturbations_ex1.RData")
# nodes<-base::unique(c(toy_network_ex1$source,toy_network_ex1$target))
# idxNodes<-seq(1,length(nodes),1)
# 
# nodeVars<-paste0("n",seq(1:length(nodes)))
# nodeUpVars<-paste0("nU",idxNodes)
# nodeDownVars<-paste0("nD",idxNodes)
# nodeDf<-cbind(nodes,nodeVars,nodeUpVars,nodeDownVars)
# 
# 
# edgeDf<-toy_network_ex1
# names(edgeDf)<-c("Node1","Sign","Node2")
# edgeIdx<-seq(1,nrow(edgeDf),1)
# edgeUpVars<-paste0("eU",edgeIdx)
# edgeDownVars<-paste0("eD",edgeIdx)
# edgeDf$edgeUpVars<-edgeUpVars
# edgeDf$edgeDownVars<-edgeDownVars



createConstraint_6_7_v3<-function(variables){
  parentNodes<-base::setdiff(variables$edgeDf$Node1,variables$edgeDf$Node2)
  
  variableMerged<-base::merge(edgeDf,nodeDf,by.x = "Node1",by.y = "nodes")
  parentNodesEdges<-variableMerged[variableMerged$Node1 %in% parentNodes,]
  
  if(length(parentNodes)>0){
    constraint_6<-paste(unique(parentNodesEdges$nodeUpVars),"<=",0)
    constraints_7<-paste(unique(parentNodesEdges$nodeDownVars),"<=",0)
  }else{
    constraints_6<-c()
    constraints_7<-c()
  }
  
  variablesMergedTwoNodes<-merge(edgeDf,nodeDf,by.x = "Node2",by.y = "nodes")
  

  allIncomingEdges<-variablesMergedTwoNodes[variablesMergedTwoNodes$Node2%in%unique(variablesMergedTwoNodes$Node2),]
  
  c6_7_nodeVars<-unique(allIncomingEdges$nodeUpVars)
  for (x in c6_7_nodeVars){
    incomingedges<-allIncomingEdges[allIncomingEdges$nodeUpVars==x,]
    edgeVars<-paste("-",paste(incomingedges$edgeUpVars,collapse = " - "))
    constraintLeft<-paste(x,edgeVars,"<=",0)
    constraint_6<-c(constraint_6,constraintLeft)
  }
  
  c6_7_nodeVars<-unique(allIncomingEdges$nodeDownVars)
  for (x in c6_7_nodeVars){
    incomingedges<-allIncomingEdges[allIncomingEdges$nodeDownVars==x,]
    edgeVars<-paste("-",paste(incomingedges$edgeDownVars,collapse = " - "))
    constraintLeft<-paste(x,edgeVars,"<=",0)
    constraints_7 <- c(constraints_7, constraintLeft)
  }
  constraints6_7<-list(constraint_6,constraints_7)
  names(constraints6_7)<-c("c6","c7")
  return(constraints6_7)
}