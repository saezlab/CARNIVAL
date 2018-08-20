WriteDOTfig <- function(res,idxModel=0,dir_name,inputs,measurements,UP2GS=F){
  
  UP2GStag <- ifelse (UP2GS,"GeneSymbol","Uniprot")
  
  sif_input=NULL;act_input=NULL
  if (sum(idxModel==0)>0) {
    sif_input <- res$weightedSIF
    act_input <- res$nodesAttributes
  } else {
    if (length(res$sifAll)==1) { # only one model identified
      sif_input <- res$sifAll
      act_input <- res$attributesAll
    } else {
      for (counter in 1:length(idxModel)) {
        sif_input[[counter]] <- res$sifAll[[counter]]
        act_input[[counter]] <- res$attributesAll[[counter]] 
      }
    }
  }
  
  for (counter_mod in if (!is.list(sif_input)){1}else{1:length(sif_input)}) {
      
    if (!is.list(sif_input)) {
      sif <- sif_input
      act_all <- act_input
    } else {
      sif <- sif_input[[counter_mod]]
      act_all <- act_input[[counter_mod]]
    }
    
    act <- act_all[,c(1,2)]
    act[,2] <- sign(as.numeric(act_all[,which(colnames(act_all)=="AvgAct" | colnames(act_all)=="Activity")]))
    if (length(which(as.numeric(act[,2])==0))>0) {
      act <- act[-which(as.numeric(act[,2])==0),] # remove zero entries
    }
    
    Dot_text <- NULL
    Dot_text <- c(Dot_text,"digraph {")
    Dot_text <- c(Dot_text,"")
    
    for (counter in 1:nrow(sif)) {
      ArrowType <- ifelse(as.numeric(sif[counter,2])==1,'"vee"','"tee"')
      # if (as.numeric(sif[counter,2])==1) {
      ArrowColor <- ifelse(as.numeric(sif[counter,2])*
                             as.numeric(act[which(sif[counter,1]==act[,1]),2])==1,'black','red')
      Dot_text <- c(Dot_text,paste0(sif[counter,1],"->",sif[counter,3]," [penwidth=1",
                                    ", color=",ArrowColor[1],", arrowhead=",ArrowType[1],"]"))
    }
    
    # Map input(s)' activities
    inputsName <- names(inputs)
    if (UP2GS) {
      IDmap <- read.table(file = "data_processing/resources/HUMAN_9606_idmapping_onlyGeneName.dat",header = F,sep = "\t",stringsAsFactors = F)
      for (counter in 1:length(inputsName)) {
        inputsName[counter] <- IDmap[which(IDmap[,1] == inputsName[counter]),3][1]
      }
    }
    
    # select only the inputs in the measured file
    NoInputName <- setdiff(inputsName, act[,1])
    if (length(NoInputName)>0) {
      NoInputIdx <- NULL
      for (counter_input in 1:length(NoInputName)) {
        NoInputIdx <- c(NoInputIdx,which(NoInputName[counter_input]==inputsName))
      }
      inputsName <- inputsName[-NoInputIdx]
    }
    
    ColorNode <- c("black","red")
    ColorNodeAll <- c("lavender","mistyrose")
    IdxMapped <- NULL
    
    for (counter in 1:length(inputsName)) {
      # if (length(which(inputsName[counter]==activityNodes[,1]))>0) {
      if (abs(as.numeric(act[which(inputsName[counter]==act[,1]),2][1]))>0) {
        IdxInput <- which(inputsName[counter]==act[,1])[1]
        Dot_text <- c(Dot_text,paste0(inputsName[counter]," [style=filled, color=",
                                      if (act[IdxInput,2]>0) {paste0(ColorNode[1],", fillcolor=",ColorNodeAll[1])}
                                      else if (act[IdxInput,2]<0) {paste0(ColorNode[2],", fillcolor=",ColorNodeAll[2])},", shape=invhouse];"))
        IdxMapped <- c(IdxMapped, IdxInput)
      }
    }
    
    AllMeas <- colnames(measurements)
    if (UP2GS) {
      IDmap <- read.table(file = "data_processing/resources/HUMAN_9606_idmapping_onlyGeneName.dat",header = F,sep = "\t",stringsAsFactors = F)
      for (counter in 1:length(AllMeas)) {
        AllMeas[counter] <- IDmap[which(IDmap[,1] == AllMeas[counter]),3][1]
      }
    }
    measName <- NULL
    AllNodeSIF <- unique(c(sif[,1],sif[,3]))
    for (counter in 1:length(AllMeas)) {
      if (sum(AllMeas[counter]==AllNodeSIF)>0) {
        measName <- c(measName,AllMeas[counter])
      }
    }
    
    for (counter in 1:length(measName)) {
      if (abs(as.numeric(act[which(measName[counter]==act[,1]),2][1]))>0) {
        IdxMeas <- which(measName[counter]==act[,1])[1]
        Dot_text <- c(Dot_text,paste0(measName[counter]," [style=filled, color=",
                                      if (act[IdxMeas,2]>0) {paste0(ColorNode[1],", fillcolor=",ColorNodeAll[1])} 
                                      else if (act[IdxMeas,2]<0) {paste0(ColorNode[2],", fillcolor=",ColorNodeAll[2])},", shape=doublecircle];"))
        IdxMapped <- c(IdxMapped, IdxMeas)
      }
    }
    
    # Map the rest of nodes activities
    
    IdxAllNodeSIF <- NULL
    for (counter in 1:length(AllNodeSIF)) {
      IdxAllNodeSIF <- c(IdxAllNodeSIF,which(AllNodeSIF[counter]==act[,1])[1])
    }
    
    # Remove NA for the nodes where activities couldn't be mapped
    if (sum(is.na(IdxAllNodeSIF))>0) {
      IdxAllNodeSIF <- IdxAllNodeSIF[-which(is.na(IdxAllNodeSIF))]
    }
    
    RemainingNodeIdx <- NULL
    if(length(IdxMapped) > 0){
      RemainingNodeIdx <- setdiff(IdxAllNodeSIF,IdxMapped)
    }
    RemainingNodeName <- act[RemainingNodeIdx,1]
    
    if (length(RemainingNodeIdx)>0) {
      for (counter in 1:length(RemainingNodeIdx)) {
        Dot_text <- c(Dot_text,paste0(act[RemainingNodeIdx[counter],1]," [style=filled, fillcolor=",
                                      if (act[RemainingNodeIdx[counter],2]>0) {ColorNodeAll[1]}
                                      else if (act[RemainingNodeIdx[counter],2]<0) {ColorNodeAll[2]},"];"))
      }
    }
    
    
    Dot_text <- c(Dot_text,"")
    Dot_text <- c(Dot_text,"")
    Dot_text <- c(Dot_text,"}")
    
    fileConn <- file(paste0("results/",dir_name,"/ActivityNetwork_model_Nr",idxModel[counter_mod],"_",UP2GStag,".dot"))
    writeLines(Dot_text,fileConn)
    close(fileConn)
    
  }
  
} 
