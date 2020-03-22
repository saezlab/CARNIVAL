## Warning message in case of unwanted symbols in node id's
##
## Enio Gjerga, 2020

controlNodeIdentifiers <- function(netObj = netObj){
  
  ##
  idx1 <- which(grepl(pattern = "-", x = netObj$source, fixed = TRUE))
  idx2 <- which(grepl(pattern = "-", x = netObj$target, fixed = TRUE))
  if(length(idx1)>0 || length(idx2)>0){
    warning("Your network contains identifiers with '-' symbol and they will
            be replaced with '_'")
    netObj$source = gsub(pattern = "-", replacement = "_", 
                         x = netObj$source, fixed = TRUE)
    netObj$target <- gsub(pattern = "-", replacement = "_", 
                          x = netObj$target, fixed = TRUE)
  }
  
  ##
  idx1 <- which(grepl(pattern = "+", x = netObj$source, fixed = TRUE))
  idx2 <- which(grepl(pattern = "+", x = netObj$target, fixed = TRUE))
  if(length(idx1)>0 || length(idx2)>0){
    warning("Your network contains identifiers with '+' symbol and they will
            be replaced with '_'")
    netObj$source = gsub(pattern = "+", replacement = "_", 
                         x = netObj$source, fixed = TRUE)
    netObj$target <- gsub(pattern = "+", replacement = "_", 
                          x = netObj$target, fixed = TRUE)
  }
  
  ##
  idx1 <- which(grepl(pattern = "*", x = netObj$source, fixed = TRUE))
  idx2 <- which(grepl(pattern = "*", x = netObj$target, fixed = TRUE))
  if(length(idx1)>0 || length(idx2)>0){
    warning("Your network contains identifiers with '*' symbol and they will
            be replaced with '_'")
    netObj$source = gsub(pattern = "*", replacement = "_", 
                         x = netObj$source, fixed = TRUE)
    netObj$target <- gsub(pattern = "*", replacement = "_", 
                          x = netObj$target, fixed = TRUE)
  }
  
  ##
  idx1 <- which(grepl(pattern = "/", x = netObj$source, fixed = TRUE))
  idx2 <- which(grepl(pattern = "/", x = netObj$target, fixed = TRUE))
  if(length(idx1)>0 || length(idx2)>0){
    warning("Your network contains identifiers with '/' symbol and they will
            be replaced with '_'")
    netObj$source = gsub(pattern = "/", replacement = "_", 
                         x = netObj$source, fixed = TRUE)
    netObj$target <- gsub(pattern = "/", replacement = "_", 
                          x = netObj$target, fixed = TRUE)
  }
  
  ##
  idx1 <- which(grepl(pattern = "<", x = netObj$source, fixed = TRUE))
  idx2 <- which(grepl(pattern = "<", x = netObj$target, fixed = TRUE))
  if(length(idx1)>0 || length(idx2)>0){
    warning("Your network contains identifiers with '<' symbol and they will
            be replaced with '_'")
    netObj$source = gsub(pattern = "<", replacement = "_", 
                         x = netObj$source, fixed = TRUE)
    netObj$target <- gsub(pattern = "<", replacement = "_", 
                          x = netObj$target, fixed = TRUE)
  }
  
  ##
  idx1 <- which(grepl(pattern = ">", x = netObj$source, fixed = TRUE))
  idx2 <- which(grepl(pattern = ">", x = netObj$target, fixed = TRUE))
  if(length(idx1)>0 || length(idx2)>0){
    warning("Your network contains identifiers with '>' symbol and they will
            be replaced with '_'")
    netObj$source = gsub(pattern = ">", replacement = "_", 
                         x = netObj$source, fixed = TRUE)
    netObj$target <- gsub(pattern = ">", replacement = "_", 
                          x = netObj$target, fixed = TRUE)
  }
  
  ##
  idx1 <- which(grepl(pattern = "=", x = netObj$source, fixed = TRUE))
  idx2 <- which(grepl(pattern = "=", x = netObj$target, fixed = TRUE))
  if(length(idx1)>0 || length(idx2)>0){
    warning("Your network contains identifiers with '=' symbol and they will
            be replaced with '_'")
    netObj$source = gsub(pattern = "=", replacement = "_", 
                         x = netObj$source, fixed = TRUE)
    netObj$target <- gsub(pattern = "=", replacement = "_", 
                          x = netObj$target, fixed = TRUE)
  }
  
  ##
  idx1 <- which(grepl(pattern = " ", x = netObj$source, fixed = TRUE))
  idx2 <- which(grepl(pattern = " ", x = netObj$target, fixed = TRUE))
  if(length(idx1)>0 || length(idx2)>0){
    warning("Your network contains identifiers with spaces and they will
            be replaced with '_'")
    netObj$source = gsub(pattern = " ", replacement = "_", 
                         x = netObj$source, fixed = TRUE)
    netObj$target <- gsub(pattern = " ", replacement = "_", 
                          x = netObj$target, fixed = TRUE)
  }
  
  return(netObj)
  
}