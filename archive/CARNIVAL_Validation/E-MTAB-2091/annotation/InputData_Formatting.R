# Input data formatting from csv files

rm(list=ls());cat("\014")

setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/archive/CARNIVAL_Validation/E-MTAB-2091/data/") # set working directory (relative)

Dorothea <- read.table("E-MTAB-2091_DoRothEA.csv",header=T,sep=",",stringsAsFactors = F)
Progeny <- read.table("E-MTAB-2091_PROGENy.csv",header=T,sep=",",stringsAsFactors = F)

# Select discretisation measure
DiscretPGN <- 2 # 1=absolute value, 2=mean+/-2.5*SD (Gaussian), 3= median+/-2.5*mean_abs_diff
PGN1_Cutoff <- 50; PGN2_MulFactor <- 2; PGN3_MulFactor <- 2

# Generate continuous mismatched weight?
MismatchWeight <- 1 # 1=yes, 0=no

# =========================== #

# Plot distribution of PROGENy score

Conditions <- Dorothea[,1]

# par(mfrow=c(8,7))
colname <- colnames(Progeny[2:length(Progeny)])
# pdf("Progeny_Density_Plot.pdf")

Progeny_Density_List <- list()
for (counter in 1:(ncol(Progeny)-1)) {
  d <- density(as.numeric(Progeny[,counter+1]))
  # plot(d, type="n",main=colname[counter])
  # polygon(d, col="red",border="gray") 
  # hist(as.numeric(Dorothea[1,2:nrow(Progeny)]))
  Progeny_Density_List[[counter]] <- as.numeric(Progeny[,counter+1])
}
# dev.off()

# par(mfrow=c(1,1))
Progeny_Density_All <- unlist(Progeny_Density_List)
# pdf("Progeny_All_Histogram.pdf")
# hist(Progeny_Density_All)
# dev.off()
# d_all <- density(Progeny_Density_All)
# plot(d_all,type="n")
# polygon(d_all,col="blue")

# Defining cut-off with different methods

if (DiscretPGN == 1) {
  
  CutOffProgeny <- PGN1_Cutoff # 50; n=493 (from n=728)
  sum(Progeny_Density_All < (-1*CutOffProgeny) | Progeny_Density_All>CutOffProgeny) 
  
  CutOff_PGN_Up <- rep(PGN1_Cutoff,ncol(Progeny)-1)
  CutOff_PGN_Down <- rep((-1)*PGN1_Cutoff,ncol(Progeny)-1)
  
} else if (DiscretPGN==2) {
  
  PGN_Means <- colMeans(Progeny[,2:ncol(Progeny)])
  PGN_SD <- apply(Progeny[,2:ncol(Progeny)],2,sd)
  
  CutOff_PGN_Up   <- PGN_Means+(PGN2_MulFactor*PGN_SD)
  CutOff_PGN_Down <- PGN_Means-(PGN2_MulFactor*PGN_SD)
  
} else if (DiscretPGN==3) {
  
  PGN_Median <- apply(Progeny[,2:ncol(Progeny)],2,median)
  PGN_Means <- colMeans(Progeny[,2:ncol(Progeny)])
  PGN_MAD <- NULL
  for (counter in 1:(ncol(Progeny)-1)) {
    PGN_MAD <- c(PGN_MAD,sum(abs((Progeny[,counter+1] - PGN_Means[counter])))/nrow(Progeny))
  }
  
  CutOff_PGN_Up   <- PGN_Median+(PGN3_MulFactor*PGN_MAD)
  CutOff_PGN_Down <- PGN_Median-(PGN3_MulFactor*PGN_MAD)
  
}

# Combined data measurement file writing
Progeny_Cutoff <- Progeny[,2:ncol(Progeny)]

for (counter in 1:ncol(Progeny_Cutoff)) {
  Progeny_Cutoff[which(Progeny_Cutoff[,counter]<CutOff_PGN_Up[counter] & Progeny_Cutoff[,counter]>CutOff_PGN_Down[counter]),counter] <- NaN
  Progeny_Cutoff[which(Progeny_Cutoff[,counter]>=CutOff_PGN_Up[counter]),counter] <- 1
  Progeny_Cutoff[which(Progeny_Cutoff[,counter]<=CutOff_PGN_Down[counter]),counter] <- -1
  Progeny_Cutoff[which(is.nan(as.matrix(Progeny_Cutoff[,counter]))),counter] <- 0
}

# ColNamesNew <- substring(colnames(Progeny_CutOff),5)
# colnames(Progeny_CutOff) <- ColNamesNew
write.table(x = Progeny_CutOff,file = paste0("Progeny_All_",
                                             if (DiscretPGN==1) {paste0("AbsCutOff_",toString(PGN1_Cutoff))} 
                                             else if (DiscretPGN==2) {paste0("MeanSDCutOff_",toString(PGN2_MulFactor))} 
                                             else if (DiscretPGN==3) {paste0("MedianMADCutOff_",toString(PGN3_MulFactor))}
                                             ,".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)

setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/archive/CARNIVAL_Validation/E-MTAB-2091/annotation/") # set working directory (relative)
# Stimuli.csv
Stimuli <- read.table("Stimuli.csv",header = T,sep = ",",stringsAsFactors = F)

# Combined list of inputs
All_Targets <- Stimuli$Gene[nchar(Stimuli$Gene)>0]
All_Targets <- sort(unique(All_Targets))
All_Targets_Matrix <- matrix(data = 0,nrow = nrow(Stimuli),ncol = length(All_Targets))
# rownames(All_Targets_Matrix) <- Conditions
colnames(All_Targets_Matrix) <- All_Targets
for (counter in 1:nrow(Stimuli)) {
  IdxCond <- which(Stimuli[counter,2]==Conditions)
  IdxTarget <- which(Stimuli[counter,3]==All_Targets)
  All_Targets_Matrix[IdxCond,IdxTarget] <- Stimuli[counter,4]
}

write.table(x = All_Targets_Matrix,file = "DrugTarget_All",quote = F,sep = "\t",col.names = T,row.names = F)

# Single input file writing
# betaxolol
All_Targets_betaxolol <- All_Targets_Matrix[1,]
Idx_NonZero <- which(All_Targets_betaxolol!=0)
All_Targets_betaxolol <- matrix(All_Targets_betaxolol[Idx_NonZero],1,length(Idx_NonZero))
colnames(All_Targets_betaxolol) <- All_Targets[Idx_NonZero]
write.table(x = All_Targets_betaxolol,file = "DrugTarget_betaxolol.tsv",quote = F,sep = "\t",col.names = T,row.names = F)

# === New variant : integration of Progeny score as additoinal inputs === #

# Build progeny catalogue
ProgenyProtein <- read.table("PROGENy_Protein.csv",header = T,sep = ",",stringsAsFactors = F)
ProgenyProtein <- ProgenyProtein[order(ProgenyProtein[,2],ProgenyProtein[,3]),]
ProgenyProtein[,2] <- gsub("-",".",ProgenyProtein[,2],fixed=T)
ProgenyProtein_List <- vector(mode="list",length=length((unique(ProgenyProtein[,2]))))
for (counter in 1:length(unique(ProgenyProtein[,2]))) {
  ProgenyProtein_List[[counter]]$pw <- unique(ProgenyProtein[,2])[counter]
  Idx_pw_proteins <- which(unique(ProgenyProtein[,2])[counter]==ProgenyProtein[,2])
  ProgenyProtein_List[[counter]]$prot <- ProgenyProtein[Idx_pw_proteins,3]
}

# Load all compound names
setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/archive/CARNIVAL_Validation/E-MTAB-2091/data/") # set working directory (relative)
Compounds <- t(read.table("All_Compound_Names.tsv",sep = "\r"))
setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/archive/CARNIVAL_Validation/E-MTAB-2091/annotation/") # set working directory (relative)

# betaxolol
Progeny_CutOff_Betaxolol <- Progeny_CutOff[1,]

Progeny_Input_ToAdd <- NULL
for (counter in 1:ncol(Progeny_CutOff_Betaxolol)) {
  if (Progeny_CutOff_Betaxolol[counter]!=0) {
    Idx_ProgenyList <- which(colnames(Progeny_CutOff_Betaxolol)[counter]==unique(ProgenyProtein[,2]))
    Matrix_ToAdd <- matrix(NA,2,length(ProgenyProtein_List[[Idx_ProgenyList]]$prot))
    Matrix_ToAdd[1,] <- ProgenyProtein_List[[Idx_ProgenyList]]$prot
    Matrix_ToAdd[2,] <- Progeny_CutOff_Betaxolol[counter]
    Progeny_Input_ToAdd <- cbind(Progeny_Input_ToAdd,Matrix_ToAdd)
  }
}
Progeny_Input <- matrix(Progeny_Input_ToAdd[2,],1,ncol(Progeny_Input_ToAdd))
colnames(Progeny_Input) <- Progeny_Input_ToAdd[1,]
write.table(x = cbind(All_Targets_betaxolol,Progeny_Input),file = paste0("DrugTarget_betaxolol_plusPROGENy_CutOff_50.tsv"),quote = F,sep = "\t",col.names = T,row.names = F)

# all compounds
for (counter_compound in 1:length(Compounds)) {
  
  # current compounds
  All_Targets_current <- All_Targets_Matrix[counter_compound,]
  Idx_NonZero <- which(All_Targets_current!=0)
  All_Targets_current <- matrix(All_Targets_current[Idx_NonZero],1,length(Idx_NonZero))
  colnames(All_Targets_current) <- All_Targets[Idx_NonZero]
  write.table(x = All_Targets_current,file = paste0("DrugTarget_",Compounds[counter_compound],".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)
  
  Progeny_CutOff_current <- Progeny_CutOff[counter_compound,]
  
  Progeny_Input_ToAdd <- NULL
  for (counter in 1:ncol(Progeny_CutOff_current)) {
    if (Progeny_CutOff_current[counter]!=0) {
      Idx_ProgenyList <- which(colnames(Progeny_CutOff_current)[counter]==unique(ProgenyProtein[,2]))
      Matrix_ToAdd <- matrix(NA,2,length(ProgenyProtein_List[[Idx_ProgenyList]]$prot))
      Matrix_ToAdd[1,] <- ProgenyProtein_List[[Idx_ProgenyList]]$prot
      Matrix_ToAdd[2,] <- Progeny_CutOff_current[counter]
      Progeny_Input_ToAdd <- cbind(Progeny_Input_ToAdd,Matrix_ToAdd)
    }
  }
  if (!is.null(Progeny_Input_ToAdd)) {
    Progeny_Input <- matrix(Progeny_Input_ToAdd[2,],1,ncol(Progeny_Input_ToAdd))
    colnames(Progeny_Input) <- Progeny_Input_ToAdd[1,]
  } else {
    Progeny_Input <- NULL
  }
  write.table(x = cbind(All_Targets_current,Progeny_Input),file = paste0("DrugTarget_",Compounds[counter_compound],"_",
                                                                         if (DiscretPGN==1) {paste0("AbsCutOff_",toString(PGN1_Cutoff))} 
                                                                         else if (DiscretPGN==2) {paste0("MeanSDCutOff_",toString(PGN2_MulFactor))} 
                                                                         else if (DiscretPGN==3) {paste0("MedianMADCutOff_",toString(PGN3_MulFactor))}
                                                                         ,".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)
}


if (MismatchWeight == 1) {
  
  # For all molecules as a matrix
  PGN_MM_min <- apply(Progeny[,2:ncol(Progeny)],2,min)
  PGN_MM_max <- apply(Progeny[,2:ncol(Progeny)],2,max)
  Progeny_Weight <- matrix(NA,nrow(Progeny),ncol(Progeny)-1)
  # ColNamesNew <- substring(colnames(Progeny[,2:ncol(Progeny)]),5)
  # colnames(Progeny_Weight) <- ColNamesNew
  colnames(Progeny_Weight) <- colnames(Progeny[,2:ncol(Progeny)])
  for (counter in 1:(ncol(Progeny)-1)) {
    Progeny_Weight[,counter] <- 1-((Progeny[,counter+1]-PGN_MM_min[counter])/(PGN_MM_max[counter]-PGN_MM_min[counter]))
  }
  write.table(x = Progeny_Weight,file = paste0("Progeny_InverseWeightAll.tsv"),quote = F,sep = "\t",col.names = T,row.names = F)
  
  # For each individual compound
  
  for (counter in 1:length(Compounds)) {
    Progeny_Weight_current <- Progeny_Weight[counter,]
    write.table(x = Progeny_Weight_current,
                file = paste0("Progeny_",Compounds[counter],"_InverseWeight.tsv"),quote = F,sep = "\t",col.names = F,row.names = F)
  }
  
}


# --- End of script --- #
