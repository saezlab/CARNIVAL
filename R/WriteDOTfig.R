#'\code{WriteDOTfig}
#'
#' This function takes results from ILP optimisation and write out a figure in 
#' DOT format
#'
#'Panuwat Trairatphisan, 2020

WriteDOTfig <- function(res, idxModel=0, dir_name, 
                        inputs, measurements, UP2GS=FALSE){

  if(!is.null(UP2GS)){UP2GStag <- ifelse (UP2GS,"GeneSymbol","Uniprot")}

  sif_input=NULL;act_input=NULL
  ## default case; if idxModel is not provided, plot the combined and average 
  ## network
  if (sum(idxModel==0)>0) {
    sif_input <- res$weightedSIF
    act_input <- res$nodesAttributes
  } else { ## if the number/index of network is specified
    if (length(res$sifAll)==1) { ## if only one model was specified
      sif_input <- res$sifAll[[idxModel]]
      act_input <- res$attributesAll[[idxModel]]
    } else { ## if multiple models were specified
      for (counter in 1:length(idxModel)) {
        sif_input[[counter]] <- res$sifAll[[idxModel[counter]]]
        act_input[[counter]] <- res$attributesAll[[idxModel[counter]]]
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
    act[,2] <- 
      sign(as.numeric(act_all[,which(colnames(act_all)=="AvgAct" | 
                                       colnames(act_all)=="Activity")]))
    if (length(which(as.numeric(act[,2])==0))>0) {
      act <- act[-which(as.numeric(act[,2])==0),] ## remove zero entries
    }

    ## Check for Perturbation node -> if 0 then still add back (required)
    if (length(which(act_all[,1]=="Perturbation"))>0) {
      if (act_all[which(act_all[,1]=="Perturbation"),
                  which(colnames(act_all)=="AvgAct" | 
                        colnames(act_all)=="Activity")]==0) {
        act <- rbind(act,
                     c("Perturbation",
                       act_all[which(act_all[,1]=="Perturbation"),
                               which(colnames(act_all)=="AvgAct" | 
                                       colnames(act_all)=="Activity")]))
      }
    }

    Dot_text <- NULL
    Dot_text <- c(Dot_text,"digraph {")
    Dot_text <- c(Dot_text,"")

    for (counter in 1:nrow(sif)) {
      ArrowType <- ifelse(as.numeric(sif[counter,2])==1,'"vee"','"tee"')
      ArrowColor <- ifelse(as.numeric(sif[counter,2])*
                             as.numeric(
                               act[which(sif[counter,1]==act[,1]),2])==1,
                           'black','red')
      Dot_text <- c(Dot_text,
                    paste0(sif[counter,1],"->",sif[counter,3]," [penwidth=1",
                                    ", color=",ArrowColor[1],", arrowhead=",
                           ArrowType[1],"]"))
    }

    # Map input(s)' activities
    inputsName <- colnames(inputs)
    if(!is.null(UP2GS)){
      if (UP2GS) {
        IDmap <- 
          read.table(file = 
                       system.file("HUMAN_9606_idmapping_onlyGeneName.dat",
                                   package="CARNIVAL"), header = FALSE, 
                     sep = "\t", stringsAsFactors = FALSE)
        for (counter in 1:length(inputsName)) {
          ## check first if the ID could be mapped
          if (length(which(IDmap[,1] == inputsName[counter])>0)) {
            inputsName[counter] <- IDmap[which(IDmap[,1] == 
                                                 inputsName[counter]),3][1]
          }
        }
      }
    }

    ## select only the inputs in the measured file
    NoInputName <- setdiff(inputsName, act[,1])
    if (length(NoInputName)>0) {
        NoInputIdx <- NULL
      for (counter_input in 1:length(NoInputName)) {
        NoInputIdx <- c(NoInputIdx,
                        which(NoInputName[counter_input]==inputsName))
      }
      inputsName <- inputsName[-NoInputIdx]
    }

    ColorNode <- c("black","red")
    ColorNodeAll <- c("lavender","mistyrose")
    IdxMapped <- NULL

    for (counter in 1:length(inputsName)) {
      ## special case for perturbation (allow 0)
      if (inputsName[counter]=="Perturbation") {
        IdxInput <- which(inputsName[counter]==act[,1])[1]
        Dot_text <- c(Dot_text,
                      paste0(inputsName[counter],
                             " [style=filled, color=",
                             if (act[IdxInput,2]>0) {paste0(ColorNode[1],
                                                            ", fillcolor=",
                                                            ColorNodeAll[1])}
                                      else if (act[IdxInput,2]<0) {paste0(
                                        ColorNode[2],", fillcolor=",
                                        ColorNodeAll[2])}
                                      else if (act[IdxInput,2]==0) {
                                        "black, fillcolor=white"},
                             ", shape=invhouse];"))
        IdxMapped <- c(IdxMapped, IdxInput)
      } else {
        if (length(which(inputsName[counter]==act[,1]))>0) {
          IdxInput <- which(inputsName[counter]==act[,1])[1]
          Dot_text <- c(Dot_text,paste0(inputsName[counter],
                                        " [style=filled, color=",
                                        if (act[IdxInput,2]>0) {
                                          paste0(ColorNode[1],
                                                 ", fillcolor=",
                                                 ColorNodeAll[1])}
                                        else if (act[IdxInput,2]<0) {paste0(
                                          ColorNode[2],", fillcolor=",
                                          ColorNodeAll[2])},
                                        ", shape=invhouse];"))
          IdxMapped <- c(IdxMapped, IdxInput)
        }
      }
    }

    AllMeas <- colnames(measurements)
    if(!is.null(UP2GS)){
      if (UP2GS) {
        IDmap <- 
          read.table(
            file = system.file("HUMAN_9606_idmapping_onlyGeneName.dat",
                               package="CARNIVAL"),header = FALSE,sep = "\t",
            stringsAsFactors = FALSE)
        for (counter in 1:length(AllMeas)) {
          ## check first if the ID could be mapped
          if (length(which(IDmap[,1] == AllMeas[counter])>0)) {
            AllMeas[counter] <- IDmap[which(IDmap[,1] == AllMeas[counter]),3][1]
          }
        }
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
        Dot_text <- c(Dot_text,paste0(measName[counter],
                                      " [style=filled, color=",
                                      if (act[IdxMeas,2]>0) {paste0(
                                        ColorNode[1],", fillcolor=",
                                        ColorNodeAll[1])}
                                      else if (act[IdxMeas,2]<0) {paste0(
                                        ColorNode[2],", fillcolor=",
                                        ColorNodeAll[2])},
                                      ", shape=doublecircle];"))
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
        Dot_text <- c(Dot_text,
                      paste0(act[RemainingNodeIdx[counter],1],
                             " [style=filled, fillcolor=",
                             if (act[RemainingNodeIdx[counter],2]>0) {
                               ColorNodeAll[1]}
                                      else if (
                                        act[RemainingNodeIdx[counter],2]<0) 
                                        {ColorNodeAll[2]},"];"))
      }
    }

    Dot_text <- c(Dot_text,"")
    Dot_text <- c(Dot_text,"")
    Dot_text <- c(Dot_text,"}")

    if(!is.null(UP2GS)){
      dir.create(dir_name)
      fileConn <- file(paste0(dir_name,
                              "/ActivityNetwork_model_Nr",
                              idxModel[counter_mod],"_",UP2GStag,".dot"))
    } else {
      dir.create(dir_name)
      fileConn <- file(paste0(dir_name,"/ActivityNetwork_model_Nr",
                              idxModel[counter_mod],".dot"))
    }
    writeLines(Dot_text,fileConn)
    close(fileConn)

  }

}
