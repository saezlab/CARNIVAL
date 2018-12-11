#'\code{Uniprot2GeneSymbol}
#'
#' Conversion of Uniprot ID (e.g. from Omnipath) to official gene symbol in the plotting step
#' 
#' @param res A list of variables in Uniprot ID (network and node activities)
#' 
#' @return A list of mapped variables from Uniprot to official gene symbol for a better readibility
#'
#' @export

Uniprot2GeneSymbol <- function(res){

  # Select mapping file
  IDmap <- read.table(file = system.file("HUMAN_9606_idmapping_onlyGeneName.dat",package="CARNIVAL"),header = F,sep = "\t",stringsAsFactors = F)

  # Map each element in 'res' -> always take the first ID if there are many hit entries
  # Common SIF
  for (counter in 1:length(res[[1]][,1])) {
    if (length(IDmap[which(IDmap[,1] == res[[1]][counter,1]),3])>0) {
      res[[1]][counter,1] <- IDmap[which(IDmap[,1] == res[[1]][counter,1]),3][1]
    } else {
      print(paste0("The Uniprot ID: ",res[[1]][counter,1]," couldn't be mapped"))
    }
    if (length(IDmap[which(IDmap[,1] == res[[1]][counter,3]),3])>0) {
      res[[1]][counter,3] <- IDmap[which(IDmap[,1] == res[[1]][counter,3]),3][1]
    } else {
      print(paste0("The Uniprot ID: ",res[[1]][counter,3]," couldn't be mapped"))
    }
  }
  # Common node activity
  for (counter in 1:length(res[[2]][,1])) {
    if (length(IDmap[which(IDmap[,1] == res[[2]][counter,1]),3])>0) {
      res[[2]][counter,1] <- IDmap[which(IDmap[,1] == res[[2]][counter,1]),3][1]
    } else {
      print(paste0("The Uniprot ID: ",res[[2]][counter,1]," couldn't be mapped"))
    }
  }
  # Individual SIF
  for (counter in 1:length(res[[3]])) {
    for (counter2 in 1:length(res[[3]][[counter]][,1])) {
      if (length(IDmap[which(IDmap[,1] == res[[3]][[counter]][counter2,1]),3])>0) {
        res[[3]][[counter]][counter2,1] <- IDmap[which(IDmap[,1] == res[[3]][[counter]][counter2,1]),3][1]
      } else {
        print(paste0("The Uniprot ID: ",res[[3]][[counter]][counter2,1]," couldn't be mapped"))
      }
      if (length(IDmap[which(IDmap[,1] == res[[3]][[counter]][counter2,3]),3])>0) {
        res[[3]][[counter]][counter2,3] <- IDmap[which(IDmap[,1] == res[[3]][[counter]][counter2,3]),3][1]
      } else {
        print(paste0("The Uniprot ID: ",res[[3]][[counter]][counter2,3]," couldn't be mapped"))
      }
    }
  }
  # Individual node activity
  for (counter in 1:length(res[[4]])) {
    for (counter2 in 1:length(res[[4]][[counter]][,1])) {
      if (length(IDmap[which(IDmap[,1] == res[[4]][[counter]][counter2,1]),3])>0) {
        res[[4]][[counter]][counter2,1] <- IDmap[which(IDmap[,1] == res[[4]][[counter]][counter2,1]),3][1]
      } else {
        print(paste0("The Uniprot ID: ",res[[4]][[counter]][counter2,1]," couldn't be mapped"))
      }
    }
  }

  return(res)

}
