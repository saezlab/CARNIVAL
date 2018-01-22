# SIF file extraction from networks

setwd("~/Desktop/CARNIVAL_Validation/E-MTAB-2091/network") # set working directory (relative)

# OmniPathRefHGNC.csv
OmniPath <- read.table("OmniPathRefHGNC.csv",header = T,sep=",")
OmniPathSIF <- OmniPath[,2:4]
OmniPathSIF <- OmniPathSIF[order(OmniPathSIF[,1],OmniPathSIF[,3]),]
write.table(x = OmniPathSIF,file = "OmniPathSIF.tsv",quote = F,sep = "\t",col.names = F,row.names = F)

which(grepl(pattern = "-",x = OmniPathSIF[,1],fixed = T)) # indices to replace
OmniPathSIF[,1] <- gsub("-","~",OmniPathSIF[,1],fixed = T)
OmniPathSIF[,3] <- gsub("-","~",OmniPathSIF[,3],fixed = T)
write.table(x = OmniPathSIF,file = "OmniPathSIF_NoHyphen.tsv",sep="\t",quote = F,row.names = F,col.names = F)

# SIGNORfiltered.csv
SIGNOR <- read.delim("SIGNORfiltered.csv",header = T,sep=",")
SIGNORSIF <- SIGNOR[,2:4]
SIGNORSIF <- SIGNORSIF[order(SIGNORSIF[,1],SIGNORSIF[,3]),]
write.table(x = SIGNORSIF,file = "SignorSIF.tsv",quote = F,sep = "\t",col.names = F,row.names = F)

# BaburPPI.csv (PPI)
Babur <- read.delim("BaburPPI.csv",header = T,sep=",")
BaburSIF <- Babur[,2:4]
BaburSIF <- BaburSIF[order(BaburSIF[,1],BaburSIF[,3]),]
BaburSIF[,2] <- 1
BaburSIF <- rbind(BaburSIF,BaburSIF) # doubled interactions
BaburSIF[((nrow(BaburSIF)/2)+1):nrow(BaburSIF),2] <- -1 # set doubled interactions to inhibition

write.table(x = BaburSIF,file = "BaburSIF.tsv",quote = F,sep = "\t",col.names = F,row.names = F)

# --- End of script --- #
