# SIF file extraction from networks

setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/archive/CARNIVAL_Validation/E-MTAB-2091/network") # set working directory (relative)

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
Signor <- read.delim("SIGNORfiltered.csv",header = T,sep=",")
SignorSIF <- Signor[,2:4]
SignorSIF <- SignorSIF[order(SignorSIF[,1],SignorSIF[,3]),]
write.table(x = SignorSIF,file = "SignorSIF.tsv",quote = F,sep = "\t",col.names = F,row.names = F)

which(grepl(pattern = "-",x = SignorSIF[,1],fixed = T)) # indices to replace
SignorSIF[,1] <- gsub("-","~",SignorSIF[,1],fixed = T)
SignorSIF[,3] <- gsub("-","~",SignorSIF[,3],fixed = T)
SignorSIF[,1] <- gsub("/","_sl_",SignorSIF[,1],fixed = T)
SignorSIF[,3] <- gsub("/","_sl_",SignorSIF[,3],fixed = T)
SignorSIF[,1] <- gsub(":","_cl_",SignorSIF[,1],fixed = T)
SignorSIF[,3] <- gsub(":","_cl_",SignorSIF[,3],fixed = T)
write.table(x = SignorSIF,file = "SignorSIF_NoHyphenNoSlashNoColon.tsv",sep="\t",quote = F,row.names = F,col.names = F)

# BaburPPI.csv (PPI)
Babur <- read.delim("BaburPPI.csv",header = T,sep=",")
BaburSIF <- Babur[,2:4]
BaburSIF <- BaburSIF[order(BaburSIF[,1],BaburSIF[,3]),]
BaburSIF[,2] <- 1
BaburSIF <- rbind(BaburSIF,BaburSIF) # doubled interactions
BaburSIF[((nrow(BaburSIF)/2)+1):nrow(BaburSIF),2] <- -1 # set doubled interactions to inhibition
write.table(x = BaburSIF,file = "BaburSIF.tsv",quote = F,sep = "\t",col.names = F,row.names = F)

which(grepl(pattern = "-",x = BaburSIF[,1],fixed = T)) # indices to replace
BaburSIF[,1] <- gsub("-","~",BaburSIF[,1],fixed = T)
BaburSIF[,3] <- gsub("-","~",BaburSIF[,3],fixed = T)
write.table(x = BaburSIF,file = "BaburSIF_NoHyphen.tsv",sep="\t",quote = F,row.names = F,col.names = F)


# --- End of script --- #
