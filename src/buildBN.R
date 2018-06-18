# Background network generation by Enio (extended by Pan)

# Clear screen and variables (if any)
cat("\014"); rm(list=ls())

# Assign measurement file and choose background PKN
meas <- "Case3_Measurement.txt"
bgPKN <- 3 # 1 = Omnipath, 2 = IMIM's generic, 3 = Signor

# ================================================ #

library(readr)

# Read network and measurements
if (bgPKN==1) {
  pkn      <- read.table("OmniPathSIF_NoHyphen.tsv", sep = "\t", header = FALSE,stringsAsFactors = F); netname  <- "omnipath"
} else if (bgPKN==2) {
  pkn      <- read.table("Network_Generic_FIN_RemovedTFGenes_NoHyphen.sif", sep = "\t", header = FALSE,stringsAsFactors = F); netname <- "generic"
} else if (bgPKN==3) {
  pkn      <- read.delim("SignorSIF_NoHyphenNoSlashNoColonNoSpace.tsv", sep = "\t", header = FALSE,stringsAsFactors = F); netname <- "signor"
} else {
  error("Please select the provided background network or add the one of your own")
}
measurement <- read.table(meas,header = T,sep="\t",stringsAsFactors = F)

# Define starting source nodes  
receptors <- setdiff(x = pkn[,1], y = pkn[,3])
receptors <- receptors[which(receptors%in%pkn[,1])]

library(igraph)
gg <- graph_from_data_frame(d = pkn[, c(1, 3)])
adj <- get.adjacency(graph = gg)
pknList <- matrix(data = , nrow = 1, ncol = 3)

rm(ptm); ptm <- proc.time()

for(i in 1:length(receptors)){
  
  print(paste0("Finding the path of input Nr: ",i,"/",length(receptors)))
  
  for(j in 1:nrow(measurement)){
    
    sP <- get.all.shortest.paths(graph = gg, from = which(rownames(adj)==receptors[i]), to = which(rownames(adj)==measurement[j, 1]))
    
    if(length(sP$res)>0){
      
      for(k in 1:length(sP$res)){
        
        currPath <- sP$res[[k]]
        
        if((length(currPath)<=8) && (all(rownames(adj)[currPath]%in%unique(c(pknList[, 1], pknList[, 3])))==FALSE) && (length(currPath)>1)){
          
          for(l in 1:(length(currPath)-1)){
            
            idx <- intersect(x = which(as.character(pkn[,1])==rownames(adj)[currPath[l]]), y = which(as.character(pkn[,3])==rownames(adj)[currPath[l+1]]))
            
            pknList <- unique(rbind(pknList, as.matrix(pkn[idx, ])))
            
          }
          
        }
        
      }
      
    }
    
  }
  
}

ElapsedTime <- proc.time() - ptm

pknList <- pknList[-1, ] # remove first row containing NA

# Preparation of input files for CARNIVAL

# write sif file
write.table(x = pknList,file = paste0("pkn_reduced_",netname,".sif"),quote = F,sep = "\t",row.names = F,col.names = F)

# write input file
sourceList <- sort(unique(pknList[,1]))
targetList <- sort(unique(pknList[,3]))
inputList <-  setdiff(sourceList,targetList)
inputMatrix <- matrix(NaN,2,length(inputList))
inputMatrix[1,] <- inputList
write.table(x = inputMatrix,file = paste0("inputs_for_pkn_reduced_",netname,".sif"),quote = F,sep = "\t",row.names = F,col.names = F)

# write measurement file
measMatrix <- t(measurement)
rownames(measMatrix) <- NULL

# For some reason, the empty space before 1 and 0 need to be removed
for (counter in 1:ncol(measMatrix)) {
  if (measMatrix[2,counter]==" 1") {
    measMatrix[2,counter]="1"
  } else if (measMatrix[2,counter]==" 0") {
    measMatrix[2,counter]="0"
  }
}
write.table(x = measMatrix,file = paste0("meas_for_pkn_reduced_",netname,".sif"),quote = F,sep = "\t",row.names = F,col.names = F)

print("=== Input files for CARNIVAL are generated! ===")
print(paste0("Elapsed time to build network: ",ElapsedTime," seconds"))

# --- End of the script --- #
