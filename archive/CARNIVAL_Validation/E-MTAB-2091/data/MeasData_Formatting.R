# Measurement data formatting from csv files

rm(list=ls());cat("\014")

setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/archive/CARNIVAL_Validation/E-MTAB-2091/data") # set working directory (relative)

# E-MTAB-2091_DoRothEA.csv
Dorothea <- read.table("E-MTAB-2091_DoRothEA.csv",header=T,sep=",",stringsAsFactors = F)

# Select discretisation measure
DiscretDRT <- 3 # 1=absolute value, 2=mean+/-2.5*SD (Gaussian), 3= median+/-2.5*mean_abs_diff
DRT1_Cutoff <- 1; DRT2_MulFactor <- 2.5; DRT3_MulFactor <- 2.5

# Generate continuous mismatched weight?
MismatchWeight <- 1 # 1=yes, 0=no

# =========================== #

# Extract all compounds names
Dorothea_Compounds <- Dorothea[,1]
Dorothea_Compounds_All <- NULL
for (counter in 1:length(Dorothea_Compounds)) {
  Dorothea_Compounds_All[[counter]] <- strsplit(Dorothea_Compounds[counter],split = " ",fixed = T)
}
Dorothea_Compounds_Names <- NULL
for (counter in 1:length(Dorothea_Compounds_All)) {
  if (length(Dorothea_Compounds_All[[counter]][[1]])==1) {
    Dorothea_Compounds_Names[[counter]] <- Dorothea_Compounds_All[[counter]][[1]][1]
  } else {
    if (grepl(pattern = "(",x = Dorothea_Compounds_All[[counter]][[1]][2],fixed = T)) {
      Dorothea_Compounds_Names[[counter]] <- Dorothea_Compounds_All[[counter]][[1]][1]
    } else {
      Dorothea_Compounds_Names[[counter]] <- paste0(Dorothea_Compounds_All[[counter]][[1]][1],"_",Dorothea_Compounds_All[[counter]][[1]][2])
    }
  }
}
Dorothea_Compounds_Names <- unlist(Dorothea_Compounds_Names)
write.table(x = Dorothea_Compounds_Names,file = "All_Compound_Names.tsv",quote = F,sep = "\t",col.names = F,row.names = F)

# Plot distribution of DoRothEA score

# par(mfrow=c(8,7))
colname <- colnames(Dorothea[2:length(Dorothea)])
# pdf("Dorothea_Density_Plot.pdf")
# 
Dorothea_Density_List <- list()
for (counter in 1:(ncol(Dorothea)-1)) {
  d <- density(as.numeric(Dorothea[,counter+1]))
  # plot(d, type="n",main=colname[counter])
  # polygon(d, col="red",border="gray")
  # hist(as.numeric(Dorothea[1,2:nrow(Dorothea)]))
  Dorothea_Density_List[[counter]] <- as.numeric(Dorothea[,counter+1])
}
# dev.off()
# 
# par(mfrow=c(1,1))
Dorothea_Density_All <- unlist(Dorothea_Density_List)
# pdf("Dorothea_All_Histogram.pdf")
# hist(Dorothea_Density_All)
# dev.off()
# d_all <- density(Dorothea_Density_All)
# plot(d_all,type="n")
# polygon(d_all,col="blue")

# Defining cut-off with different methods

if (DiscretDRT == 1) {

  CutOffDorothea <- DRT1_Cutoff # 1; n=3152 / 1.5; n=1990(from n=6708)
  sum(Dorothea_Density_All < (-1*CutOffDorothea) | Dorothea_Density_All>CutOffDorothea) 

  CutOff_DRT_Up <- rep(DRT1_Cutoff,ncol(Dorothea)-1)
  CutOff_DRT_Down <- rep((-1)*DRT1_Cutoff,ncol(Dorothea)-1)

} else if (DiscretDRT==2) {
  
  DRT_Means <- colMeans(Dorothea[,2:ncol(Dorothea)])
  DRT_SD <- apply(Dorothea[,2:ncol(Dorothea)],2,sd)
  
  CutOff_DRT_Up   <- DRT_Means+(DRT2_MulFactor*DRT_SD)
  CutOff_DRT_Down <- DRT_Means-(DRT2_MulFactor*DRT_SD)
  
} else if (DiscretDRT==3) {

  DRT_Median <- apply(Dorothea[,2:ncol(Dorothea)],2,median)
  DRT_Means <- colMeans(Dorothea[,2:ncol(Dorothea)])
  DRT_MAD <- NULL
  for (counter in 1:(ncol(Dorothea)-1)) {
    DRT_MAD <- c(DRT_MAD,sum(abs((Dorothea[,counter+1] - DRT_Means[counter])))/nrow(Dorothea))
  }
  
  CutOff_DRT_Up  <- DRT_Median+(DRT3_MulFactor*DRT_MAD)
  CutOff_DRT_Down <- DRT_Median-(DRT3_MulFactor*DRT_MAD)
    
}

# Combined data measurement file writing
Dorothea_Cutoff <- Dorothea[,2:ncol(Dorothea)]
Dorothea_Cutoff[Dorothea_Cutoff<CutOff_DRT_Up & Dorothea_Cutoff>CutOff_DRT_Down] <- NaN
Dorothea_Cutoff[Dorothea_Cutoff>=CutOff_DRT_Up] <- 1
Dorothea_Cutoff[Dorothea_Cutoff<=CutOff_DRT_Down] <- -1
Dorothea_Cutoff[is.nan(as.matrix(Dorothea_Cutoff))] <- 0
ColNamesNew <- substring(colnames(Dorothea_Cutoff),4)
colnames(Dorothea_Cutoff) <- ColNamesNew
write.table(x = Dorothea_Cutoff,file = paste0("Dorothea_All_",
  if (DiscretDRT==1) {paste0("AbsCutOff_",toString(DRT1_Cutoff))} 
  else if (DiscretDRT==2) {paste0("MeanSDCutOff_",toString(DRT2_MulFactor))} 
  else if (DiscretDRT==3) {paste0("MedianMADCutOff_",toString(DRT3_MulFactor))}
  ,".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)

# # Single data measurement file writing

# # e.g. betaxolol
# Dorothea_Cutoff_betaxolol <- Dorothea_Cutoff[1,]
# ColNamesSingle <- ColNamesNew[Dorothea_Cutoff_betaxolol!=0]
# Dorothea_Cutoff_betaxolol <- matrix(Dorothea_Cutoff_betaxolol[Dorothea_Cutoff_betaxolol!=0],nrow = 1,ncol = length(Dorothea_Cutoff_betaxolol[Dorothea_Cutoff_betaxolol!=0]))
# colnames(Dorothea_Cutoff_betaxolol) <- ColNamesSingle
# write.table(x = Dorothea_Cutoff_betaxolol,file = paste0("Dorothea_betaxolol_Cutoff_",toString(CutOffDorothea),".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)

# for all compounds
for (counter in 1:length(Dorothea_Compounds_Names)) {
  Dorothea_Cutoff_current <- Dorothea_Cutoff[counter,]
  ColNamesSingle <- ColNamesNew[Dorothea_Cutoff_current!=0]
  Dorothea_Cutoff_current <- matrix(Dorothea_Cutoff_current[Dorothea_Cutoff_current!=0],nrow = 1,ncol = length(Dorothea_Cutoff_current[Dorothea_Cutoff_current!=0]))
  colnames(Dorothea_Cutoff_current) <- ColNamesSingle
  write.table(x = Dorothea_Cutoff_current,
              file = paste0("Dorothea_",Dorothea_Compounds_Names[counter],"_",
                if (DiscretDRT==1) {paste0("AbsCutOff_",toString(DRT1_Cutoff))} 
                else if (DiscretDRT==2) {paste0("MeanSDCutOff_",toString(DRT2_MulFactor))} 
                else if (DiscretDRT==3) {paste0("MedianMADCutOff_",toString(DRT3_MulFactor))}
                ,".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)
}

if (MismatchWeight == 1) {
  
  # For all molecules as a matrix
  DRT_MM_min <- apply(Dorothea[,2:ncol(Dorothea)],2,min)
  DRT_MM_max <- apply(Dorothea[,2:ncol(Dorothea)],2,max)
  Dorothea_Weight <- matrix(NA,nrow(Dorothea),ncol(Dorothea)-1)
  ColNamesNew <- substring(colnames(Dorothea[,2:ncol(Dorothea)]),4)
  colnames(Dorothea_Weight) <- ColNamesNew
  for (counter in 1:(ncol(Dorothea)-1)) {
    Dorothea_Weight[,counter] <- 1-((Dorothea[,counter+1]-DRT_MM_min[counter])/(DRT_MM_max[counter]-DRT_MM_min[counter]))
  }
  write.table(x = Dorothea_Weight,file = paste0("Dorothea_InverseWeightAll.tsv"),quote = F,sep = "\t",col.names = T,row.names = F)
  
  # For each individual compound
  
  for (counter in 1:length(Dorothea_Compounds_Names)) {
    Dorothea_Weight_current <- Dorothea_Weight[counter,]
    write.table(x = Dorothea_Weight_current,
                file = paste0("Dorothea_",Dorothea_Compounds_Names[counter],"_InverseWeight.tsv"),quote = F,sep = "\t",col.names = F,row.names = F)
  }
  
}

# --- End of script --- #
