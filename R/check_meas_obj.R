## Returning error message in case of errors in the inputs
##
## Enio Gjerga, 2020

checkMeasurements <- function(measurements = measurements, 
                              priorKnowledgeNetwork = priorKnowledgeNetwork){
  
  nSpecies = unique(c(as.character(as.matrix(priorKnowledgeNetwork)[, 1]), 
                      as.character(as.matrix(priorKnowledgeNetwork)[, 3])))
  
  if (is.null(measurements)) {
    stop("Please provide a valid measurement object.")
  } else {
    ## allowedClass = c("matrix", "data.frame")
    if((!is(measurements, "matrix")) && (!is(measurements, "data.frame"))){
      stop("Measurement object should either be of matrix or data.frame class")
    } else {
      if(ncol(measurements)>0){
        
        colnames(measurements) <- gsub(pattern = "-", replacement = "_", 
                            x = colnames(measurements), fixed = TRUE)
        colnames(measurements) <- gsub(pattern = "+", replacement = "_", 
                            x = colnames(measurements), fixed = TRUE)
        colnames(measurements) <- gsub(pattern = "*", replacement = "_", 
                            x = colnames(measurements), fixed = TRUE)
        colnames(measurements) <- gsub(pattern = "/", replacement = "_", 
                            x = colnames(measurements), fixed = TRUE)
        colnames(measurements) <- gsub(pattern = "<", replacement = "_", 
                            x = colnames(measurements), fixed = TRUE)
        colnames(measurements) <- gsub(pattern = ">", replacement = "_", 
                            x = colnames(measurements), fixed = TRUE)
        colnames(measurements) <- gsub(pattern = "=", replacement = "_", 
                            x = colnames(measurements), fixed = TRUE)
        colnames(measurements) <- gsub(pattern = " ", replacement = "_", 
                            x = colnames(measurements), fixed = TRUE)
        
        mSpecies = colnames(measurements)
        
        idx = which(mSpecies%in%nSpecies)
        idx2rem = setdiff(seq_len(length(mSpecies)), idx)
        
        if(length(idx2rem)==length(mSpecies)){
          stop("Something wrong with your measurements object/network object. 
               No measurements is present in the network")
        } else {
          if(length(idx2rem)>0){
            if((nrow(measurements)==1) && (is(measurements, "matrix"))){
              measurements = measurements[, -idx2rem]
              measurements = t(as.matrix(measurements))
            } else {
              measurements = measurements[, -idx2rem]
            }
          }
        }
      } else {
        stop("Something wrong with your measurements object. Please check.")
      }
    }
  }
  
  return(measurements)
  
}
