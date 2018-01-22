# Measurement data formatting from csv files

setwd("~/Desktop/CARNIVAL_Validation/Validation/") # set working directory (relative)

# E-MTAB-2091_PP5min.csv & E-MTAB-2091_PP25min.csv
PP5min <- read.table("E-MTAB-2091_PP5min.csv",header=T,sep=",",stringsAsFactors = F)
PP25min <- read.table("E-MTAB-2091_PP25min.csv",header=T,sep=",",stringsAsFactors = F)
MeasuredPP <- colnames(PP5min)[4:ncol(PP5min)]

# nodesActivity_Betaxolol.txt
# NodeAct_Betaxolol <- read.delim("nodesActivity_Betaxolol.txt",header=T,sep="\t",stringsAsFactors = F)
NodeAct_Betaxolol <- read.delim("nodesActivity_Betaxolol_PROGENy.txt",header=T,sep="\t",stringsAsFactors = F)
Overlapped_Proteins <- intersect(MeasuredPP,NodeAct_Betaxolol[,1])

# Writing results for the overlapped proteins
Result_Matrix <- matrix(NA,length(Overlapped_Proteins),4)
colnames(Result_Matrix) <- c("Protein","CARNIVAL","PP5min","PP25min")
Idx_Condition <- which("betaxolol"==PP5min[,1])
for (counter in 1:length(Overlapped_Proteins)) {
  Current_Overlapped_Protein <- Overlapped_Proteins[counter]
  Current_CARNIVAL_output <- NodeAct_Betaxolol$Activity[NodeAct_Betaxolol$Nodes==Current_Overlapped_Protein]
  Current_PP5min_meas <- PP5min[Idx_Condition,Current_Overlapped_Protein==colnames(PP5min)]
  Current_PP25min_meas <- PP25min[Idx_Condition,Current_Overlapped_Protein==colnames(PP25min)]
  Result_Matrix[counter,] <- c(Current_Overlapped_Protein,Current_CARNIVAL_output,Current_PP5min_meas,Current_PP25min_meas)
}

write.table(x = Result_Matrix,file = "Validation_Results_Betaxolol.tsv",quote = F,sep = "\t",col.names = T,row.names = F)

# --- End of script --- #
