#'\code{exportResult}
#'
#' Extract and export the optimisation results from the cplex solution file (XML) as files and variables for further plotting functions
#'
#' @param cplexSolutionFileName Path to the cplex solution file (XML)
#' @param variables The list of mapping indices of LP variables
#' @param pknList The provided prior knowledge network
#' @param dir_name The name of directory to store results
#' @param inputs The list of known or potential target of perturbation
#' @param measurements The discretised observations (here transcription factor activities) of values [-1,0,1]
#' @param Export_all An option to define whether all detailed mapppd LP variables will be written as individual files
#' @param writeIndividualResults An option to define whether the results of individual solutions will be written; if FALSE, only the global combined solution will be written
#'
#' @return Output files of ILP solutions and a list of networks and node activities to be written into figures
#'
#' @export

exportResultAllConditions <- function(cplexSolutionFileName = cplexSolutionFileName, variables = variables,
                         pknList = pknList, dir_name = dir_name, inputs=inputs, measurements=measurements, Export_all = Export_all, writeIndividualResults=F){
  
  solution <- read.delim(file = cplexSolutionFileName)
  solution[, 1] <- as.character(solution[, 1])
  idxVarStart <- which(grepl(pattern = "<variables>", x = solution[, 1]))[-1]
  idxVarEnd <- which(grepl(pattern = "</variables>", x = solution[, 1]))[-1]
  
  solMatrix <- matrix(data = , nrow = idxVarEnd[1]-idxVarStart[1]-1, ncol = length(idxVarStart))
  colnames(solMatrix) <- paste0("Solution-", 1:ncol(solMatrix))
  ss1 <- sapply(strsplit(solution[(idxVarStart[1]+1):(idxVarEnd[1]-1), 1], split = " "), "[", 5)
  rownames(solMatrix) <- sapply((strsplit(ss1, split = "=")), "[", 2)
  
  for(ii in 1:ncol(solMatrix)){
    
    ss1 <- sapply(strsplit(solution[(idxVarStart[ii]+1):(idxVarEnd[ii]-1), 1], split = " "), "[", 7)
    solMatrix[, ii] <- gsub(pattern = "/>", replacement = "", x = sapply(strsplit(ss1, split = "="), "[", 2))
    
  }
  
  sifAll <- list()
  nodesAll <- list()
  nodesActAll <- list()
  edgesAll <- list()
  vars <- rownames(solMatrix)
  
  for(ii in 1:ncol(solMatrix)){
    
    values <- solMatrix[, ii]
    
    edgesVar <- paste0("y", 1:nrow(pknList))
    
    sif <- matrix(data = , nrow = 1, ncol = 3)
    
    for(jj in 1:length(edgesVar)){
      
      if(solMatrix[which(rownames(solMatrix)==edgesVar[jj]), ii]==1){
        
        cnt <- 0
        ss <- strsplit(x = strsplit(x = variables$`Reaction Variables`$Explanation[which(variables$`Reaction Variables`$Variables==edgesVar[jj])], split = " ", fixed = TRUE)[[1]][2], split = "=", fixed = TRUE)[[1]][1]
        tt <- strsplit(x = strsplit(x = variables$`Reaction Variables`$Explanation[which(variables$`Reaction Variables`$Variables==edgesVar[jj])], split = " ", fixed = TRUE)[[1]][2], split = "=", fixed = TRUE)[[1]][2]
        for(kk in 1:(length(variables)-1)){
          
          ssVar <- variables[[kk]]$variables[which(variables[[kk]]$exp==paste0("Species ", ss, " in experiment ", kk))]
          ttVar <- variables[[kk]]$variables[which(variables[[kk]]$exp==paste0("Species ", tt, " in experiment ", kk))]
          
          if((solMatrix[which(rownames(solMatrix)==ssVar), ii]!=0) && solMatrix[which(rownames(solMatrix)==ttVar), ii]!=0){
            
            cnt <- cnt + 1
            
          }
          
        }
        
        if(cnt==(length(variables)-1)){
          
          idx1 <- which(pknList$Node1==ss)
          idx2 <- which(pknList$Node2==tt)
          idx <- intersect(x = idx1, y = idx2)
          
          sif <- unique(rbind(sif, t(as.matrix(c(ss, pknList$Sign[idx], tt)))))
          
        }
        
      }
      
    }
    
    
    sifAll[[length(sifAll)+1]] <- sif[-1, ]
    
  }
  
  # print(sifAll)
  
  if(length(sifAll)==0){
    
    print("No network was generated for this setting..")
    
    RES <- NULL
    
    return(RES)
    
  } else {
    
    for(ii in 1:length(sifAll)){
      
      if(ii ==1){
        
        SIF <- sifAll[[ii]]
        
      } else {
        
        SIF <- unique(rbind(SIF, sifAll[[ii]]))
        
      }
      
    }
    
    ##
    weightedSIF <- matrix(data = , nrow = nrow(SIF), ncol = 4)
    weightedSIF[, 1:3] <- SIF
    for(i in 1:nrow(SIF)){
      
      cnt <- 0
      
      for(j in 1:length(sifAll)){
        
        idxNode1 <- which(sifAll[[j]][, 1]==SIF[i, 1])
        idxSign <- which(sifAll[[j]][, 2]==SIF[i, 2])
        idxNode2 <- which(sifAll[[j]][, 3]==SIF[i, 3])
        
        idx1 <- intersect(idxNode1, idxNode2)
        if(length(idx1) > 0){
          
          idx2 <- intersect(idxSign, idx1)
          
          if(length(idx2) > 0){
            
            cnt <- cnt + 1
            
          }
          
        }
        
      }
      
      weightedSIF[i, 4] <- as.character(cnt*100/length(sifAll))
      
    }
    
    colnames(weightedSIF) <- c("Node1", "Sign", "Node2", "Weight")
    
    # write.table(x = weightedSIF, file = "weightedModel.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
    # fileConn2 <- file(paste0("results/",dir_name,"/weightedModel_", conditionIDX, ".txt"))
    fileConn2 <- file(paste0(dir_name,"/weightedModel.txt"))
    write.table(x = weightedSIF, file = fileConn2, quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
    
    RES <- list()
    RES[[length(RES)+1]] <- weightedSIF
    RES[[length(RES)+1]] <- sifAll
    
    names(RES) <- c("weightedSIF","sifAll")
    
    return(RES)
    
  }
  
}
