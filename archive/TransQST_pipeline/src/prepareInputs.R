prepareInputs <- function(PROGENyTable = PROGENyTable, PROGENyProtein = PROGENyProtein, pknList = pknList, thresh = thresh) {
  
  inputs <- matrix(data = 0, nrow = nrow(PROGENyTable), ncol = length(unique(c(pknList$Node1, pknList$Node2))))
  colnames(inputs) <- unique(c(pknList$Node1, pknList$Node2))
  rownames(inputs) <- rownames(PROGENyTable)
  
  for(i in 1:nrow(PROGENyTable)){
    
    for(j in 1:ncol(PROGENyTable)){
      
      if(abs(as.numeric(PROGENyTable[i, j])) > thresh){
        
        if(as.numeric(PROGENyTable[i, j]) < 0){
          
          inputs[i, which(colnames(inputs)==PROGENyProtein$Target[which(PROGENyProtein$Score==colnames(PROGENyTable)[j])])] <- -1
          
        }
        else{
          
          inputs[i, which(colnames(inputs)==PROGENyProtein$Target[which(PROGENyProtein$Score==colnames(PROGENyTable)[j])])] <- 1
          
        }
        
      }
      
    }
    
  }
  
  return(inputs)
  
}