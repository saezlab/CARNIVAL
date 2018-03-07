# Processing and combining results from time/dose

rm(list=ls()); cat("\014")

# Select drug's name (one drug at a time)
DrugName <- "ACETAMINOPHEN"
Network <- "Omnipath"
Threshold <- 1.5

# ================================================ #

GeneActAll <- NULL

# Time points and experimental conditions
Time_Points <- c("2h","8h","24h")
Exp_Conds <- c("low","mid","high")
#Exp_Conds <- c("ctrl","low","mid","high")

# Select the drug of interest

for (counter in 1:length(Time_Points)) {
  for (counter2 in 1:length(Exp_Conds)) {
    
    ResultsDIR <- paste0("../../../Modelling_Results/CARNIVAL/TransQST/ThresholdSD",toString(Threshold),"/validation_",DrugName,"_",Network,"_",Exp_Conds[counter2],"_",Time_Points[counter],"/")
    ProtAct <- read.table(paste0(ResultsDIR,"nodesActivity_1.txt"),header = T,sep = "\t")
    ProtAct <- ProtAct[which(round(ProtAct[,2])!=0),]
    "../../../Modelling_Results/CARNIVAL/TransQST/ThresholdSD1.5/validation_ACETAMINOPHEN_omnipath_high_24h/nodesActivity_1.txt"
    

    # Part2 : Script to prepare gene activity annotation file (XS) for visualisation
    
    # XS_Raw <- read.table(paste("xs_TGG_", DrugName, "_Human_ivt_", Time_Points[counter], "_", Exp_Conds[counter2], "_Generic.txt",sep=""))
    # GenAct <- substr(x = as.character(XS_Raw[,1]),start = 4,nchar(as.character(XS_Raw[,1]))-3)
    # ANNO <- cbind(GenAct,XS_Raw[,2])
    
    # write.table(x = ANNO,file = paste("GeneAct_", DrugName, "_" , Time_Points[counter], "_", Exp_Conds[counter2],".txt",sep=""),quote = FALSE,row.names = FALSE,col.names = FALSE)
    
    colnames(ProtAct)[1] <- "ProtName"
    colnames(ProtAct)[2] <- paste(Time_Points[counter], "_", Exp_Conds[counter2],sep="")
    
    if (counter==1 & counter2==1) {
      GeneActAll <- ProtAct
    } else {
      GeneActAll <- merge(GeneActAll,ProtAct,by="ProtName",all=TRUE)
    }
    print(GeneActAll)
    
  }
}

rownames(GeneActAll) <- GeneActAll[,1]
GeneActAll <- as.matrix(GeneActAll[,-1])

for (counter in 1:ncol(GeneActAll)) {
  IdxNa <- which(is.na(GeneActAll[,counter]))
  GeneActAll[IdxNa,counter] <- 0
  GeneActAll[,counter] <- as.numeric(GeneActAll[,counter])
}
GeneActAll_Numeric <- matrix(as.numeric(GeneActAll),nrow = nrow(GeneActAll),ncol=ncol(GeneActAll))

rownames(GeneActAll_Numeric) <- rownames(GeneActAll)
colnames(GeneActAll_Numeric) <- colnames(GeneActAll)

AllDownEntries <- which(rowSums(GeneActAll_Numeric) == -1*ncol(GeneActAll_Numeric))
AllUpEntries   <- which(rowSums(GeneActAll_Numeric) == 1*ncol(GeneActAll_Numeric))

UpDownEntries <- c(AllDownEntries,AllUpEntries)

GeneActAll_Numeric_OnlyDiff <- GeneActAll_Numeric[-UpDownEntries,]

library(gplots)
library(RColorBrewer)
MyColours <- brewer.pal(11,"RdYlGn")
par(oma = c(2, 0, 0, 0))
# heatmap.2(GeneActAll_Numeric)
pdf(file="APAP_Node_Activities_OnlyDiff.pdf")
heatmap.2(GeneActAll_Numeric_OnlyDiff,col=MyColours,tracecol=NA,cexRow = 0.2)
dev.off()

