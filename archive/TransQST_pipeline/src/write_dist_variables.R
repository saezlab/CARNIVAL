write_dist_variables <- function(pknList=pknList){
  
  dist <- matrix(NA, nrow = length(unique(c(as.character(pknList[, 1]), as.character(pknList[, 3])))), ncol = length(unique(c(as.character(pknList[, 1]), as.character(pknList[, 3])))))
  rownames(dist) <- unique(c(as.character(pknList[, 1]), as.character(pknList[, 3])))
  colnames(dist) <- unique(c(as.character(pknList[, 1]), as.character(pknList[, 3])))

  for(i in 1:nrow(dist)) {
    dist[i, ] <- paste0("dist_", rownames(dist)[i], "_", colnames(dist))
  }
  
  return(dist)
  
}