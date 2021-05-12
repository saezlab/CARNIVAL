## Vectorizing the objective function.
##
## Enio Gjerga, 2020

transformObjectiveFunction <- function(mt, lpFile){
  
  idx <- which(grepl(pattern = "Obj:", x = lpFile), arr.ind = FALSE)
  
  problem <- as.character(lpFile[idx])
  problem <- gsub(pattern = "Obj:\t ", replacement = "", x = problem, 
                  fixed = TRUE)
  
  coefficients <- as.numeric(
    unlist(
      lapply(
        strsplit(
          x = strsplit(
            x = problem, split = "+ ", fixed = TRUE)[[1]], 
          split = " ", fixed = TRUE), "[[", 1)))
  
  varId <- as.character(
    unlist(
      lapply(
        strsplit(
          x = strsplit(
            x = problem, split = "+ ", fixed = TRUE)[[1]], 
          split = " ", fixed = TRUE), "[[", 2)))
  
  idx <- rep(NA, length(varId))
  for( ii in seq_len(length(varId)) ) {
    idx[ii] <- match(varId[ii],  mt[, 1])
  }
  
  f.obj <- rep(0, nrow(mt))
  f.obj[idx] <- coefficients
  
  return(f.obj)
}