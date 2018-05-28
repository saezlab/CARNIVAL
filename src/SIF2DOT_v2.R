SIF2DOT_v2 <- function(sif,meas,DOTfilename="SIF_Network.dot",act=NULL) {
  
  ColorNode <- c("black","red")
  ColorNodeAll <- c("lavender","mistyrose")
  
  # Load network and measurement
  network_raw <- read.csv(sif,header=TRUE,sep="\t",stringsAsFactors = F)
  # network <- read.csv(sif,header=TRUE,sep="\t",stringsAsFactors = F)
  measurement_raw <- read.table(meas,header = T,sep="\t",stringsAsFactors = F)
  actMat <- matrix(NA,length(measurement_raw),2)
  colnames(actMat) <- c("Node","Activity")
  actMat[,1] <- names(as.vector(measurement_raw))
  actMat[,2] <- as.integer(as.vector(measurement_raw))
  measurement_raw <- actMat

  # Include only the interactions ending at measured nodes (remove out-going interaction(s) from measure nodes - doesn't take into account in the pipeline)
  measName <- measurement_raw[,1]
  ExcludeIntActIDX <- NULL
  for (counter in 1:nrow(network_raw)) {
    if (length(intersect(network_raw[counter,1],measName))>0 # source is measurement
        & !length(intersect(network_raw[counter,3],measName))>0) { # target is non-measured
      ExcludeIntActIDX <- c(ExcludeIntActIDX,counter) # collect in the exclusion list
    }
  }
  if (!is.null(ExcludeIntActIDX)) {
    network <- network_raw[-ExcludeIntActIDX,]
  } else {
    network <- network_raw
  }
    
  # Include only the measurements which are present in the network (prevent floting measured nodes)
  AllNodes <- unique(c(network[,1],network[,3]))
  # MappedMeasIDX <- NULL
  # # for (counter in 1:nrow(measurement_raw)) {
  #   # if (sum(measurement_raw[counter,1]==AllNodes)>0) {
  # for (counter in 1:nrow(measurement)) {
  #   if (sum(measurement[counter,1]==AllNodes)>0) {
  #     MappedMeasIDX <- c(MappedMeasIDX,counter)
  #   }
  # }
  # measurement <- measurement_raw[MappedMeasIDX,]
  measurement <- measurement_raw
  measName <- measurement[,1]
  
  
  # Opening DOT file description
  Dot_text <- NULL
  Dot_text <- c(Dot_text,"digraph {")
  Dot_text <- c(Dot_text,"")
  
  # Map network
  for (counter in 1:nrow(network)) {
    if (network[counter,2]==1) {
      Dot_text <- c(Dot_text,paste0(network[counter,1],"->",network[counter,3]," [penwidth=",toString(1),", color=black, arrowhead=vee]"))
    } else if (network[counter,2]==-1) {
      Dot_text <- c(Dot_text,paste0(network[counter,1],"->",network[counter,3]," [penwidth=",toString(1),", color=red, arrowhead=tee]"))
    } else if (network[counter,2]==0) {
      Dot_text <- c(Dot_text,paste0(network[counter,1],"->",network[counter,3]," [penwidth=",toString(1),", color=grey, arrowhead=none]"))
    }
  }
  
  # Map measurement
  for (counter in 1:nrow(measurement)) {
    Dot_text <- c(Dot_text,paste0(measName[counter]," [style=filled, color=",
                                    if (measurement[counter,2]>0) {paste0(ColorNode[1],", fillcolor=",ColorNodeAll[1])} 
                                    else if (measurement[counter,2]<0) {paste0(ColorNode[2],", fillcolor=",ColorNodeAll[2])}
                                    else if (measurement[counter,2]==0) {paste0("black, fillcolor=white")},", shape=doublecircle];"))
  }
  
  
  if (!is.null(act)) { # if inferred nodes' activities file exists
  # if (file.exists(act)) { # if inferred nodes' activities file exists
      
    nodeActAll <- read.csv(act,header=TRUE,sep="\t",stringsAsFactors = F)
    measIDX <- NULL
    for (counter in 1:nrow(measurement_raw)) {
      measIDX <- c(measIDX,which(measurement_raw[counter,1]==nodeActAll[,1]))
    }
    nodeAct_noMeas <- nodeActAll[-measIDX,] # remove measured nodes
    if (length(which(nodeAct_noMeas[,2]==0))>0) {
      nodeAct_mapDOT <- nodeAct_noMeas[-which(nodeAct_noMeas[,2]==0),] # remove nodes without activity
    } else {
      nodeAct_mapDOT <- nodeAct_noMeas
    }
    
    # Map corresponding inferred activation and inhibitions
    for (counter in 1:nrow(nodeAct_mapDOT)) {
      Dot_text <- c(Dot_text,paste0(nodeAct_mapDOT[counter,1]," [style=filled, color=",
                                    if (nodeAct_mapDOT[counter,2]>0) {paste0(ColorNode[1],", fillcolor=",ColorNodeAll[1])} 
                                    else if (nodeAct_mapDOT[counter,2]<0) {paste0(ColorNode[2],", fillcolor=",ColorNodeAll[2])},"];"))
    }
    
  }
  
  # Closing DOT file description and write to a file
  Dot_text <- c(Dot_text,"")
  Dot_text <- c(Dot_text,"")
  Dot_text <- c(Dot_text,"}")
  
  fileConn <- file(DOTfilename)
  writeLines(Dot_text,fileConn)
  
}
  