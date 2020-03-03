#'\code{Uniprot2GeneSymbol}
#'
#' Conversion of Uniprot ID (e.g. from Omnipath) to official gene symbol in the 
#' plotting step
#'
#'Panuwat Trairatphisan, 2020

Uniprot2GeneSymbol <- function(res){

  # Select mapping file
  IDmap <- 
    read.table(file = system.file("HUMAN_9606_idmapping_onlyGeneName.dat",
                                  package="CARNIVAL"),header = FALSE,sep = "\t",
               stringsAsFactors = FALSE)

  ## Map each element in 'res' -> always take the first ID if there are many hit
  ## entries and collect unmapped nodes
  Unmapped <- NULL

  # Common SIF
  for (counter in 1:length(res[[1]][,1])) {
    if (length(IDmap[which(IDmap[,1] == res[[1]][counter,1]),3])>0) {
      res[[1]][counter,1] <- IDmap[which(IDmap[,1] == res[[1]][counter,1]),3][1]
    } else {
      Unmapped <- c(Unmapped,res[[1]][counter,1])
    }
    if (length(IDmap[which(IDmap[,1] == res[[1]][counter,3]),3])>0) {
      res[[1]][counter,3] <- IDmap[which(IDmap[,1] == res[[1]][counter,3]),3][1]
    } else {
      Unmapped <- c(Unmapped,res[[1]][counter,3])
    }
  }
  # Common node activity
  for (counter in 1:length(res[[2]][,1])) {
    if (length(IDmap[which(IDmap[,1] == res[[2]][counter,1]),3])>0) {
      res[[2]][counter,1] <- IDmap[which(IDmap[,1] == res[[2]][counter,1]),3][1]
    } else {
      Unmapped <- c(Unmapped,res[[2]][counter,1])
    }
  }
  ## Individual SIF
  for (counter in 1:length(res[[3]])) {
    for (counter2 in 1:length(res[[3]][[counter]][,1])) {
      if (length(IDmap[which(IDmap[,1] == 
                             res[[3]][[counter]][counter2,1]),3])>0) {
        res[[3]][[counter]][counter2,1] <- 
          IDmap[which(IDmap[,1] == res[[3]][[counter]][counter2,1]),3][1]
      } else {
        Unmapped <- c(Unmapped,res[[3]][[counter]][counter2,1])
      }
      if (length(IDmap[which(IDmap[,1] == 
                             res[[3]][[counter]][counter2,3]),3])>0) {
        res[[3]][[counter]][counter2,3] <- 
          IDmap[which(IDmap[,1] == res[[3]][[counter]][counter2,3]),3][1]
      } else {
        Unmapped <- c(Unmapped,res[[3]][[counter]][counter2,3])
      }
    }
  }
  # Individual node activity
  for (counter in 1:length(res[[4]])) {
    for (counter2 in 1:length(res[[4]][[counter]][,1])) {
      if (length(IDmap[which(IDmap[,1] == 
                             res[[4]][[counter]][counter2,1]),3])>0) {
        res[[4]][[counter]][counter2,1] <- 
          IDmap[which(IDmap[,1] == res[[4]][[counter]][counter2,1]),3][1]
      } else {
        Unmapped <- c(Unmapped,res[[4]][[counter]][counter2,1])
      }
    }
  }

  print(paste0("The following node couldn't be mapped to gene symbol: ",
               paste(unique(Unmapped),collapse = " ")))

  return(res)

}
