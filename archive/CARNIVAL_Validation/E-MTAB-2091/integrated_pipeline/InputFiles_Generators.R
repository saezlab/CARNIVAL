# Input files generator

rm(list=ls());cat("\014");if (length(dev.list()>0)){dev.off()}

# Select discretisation measure for Dorothea (DRT) & Progeny (PGN)
# 1=absolute Cutoff value, 2=mean+/-Cutoff*SD (Gaussian), 3= median+/-Cutoff*mean_abs_diff
DiscretDRT <- 2; DRT_Cutoff <- 2
DiscretPGN <- 2; PGN_Cutoff <- 2 

# ==================================== #

# Load all source scripts
setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/"); source("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/src/CRILPR_Functions.R")
setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/archive/CARNIVAL_Validation/E-MTAB-2091/integrated_pipeline/")

# Load and discretize Dorothea and Progeny score
Dorothea <- read.table("resources/E-MTAB-2091_DoRothEA.csv",header=T,sep=",",stringsAsFactors = F)
DorotheaZScore <- scale(Dorothea[,2:ncol(Dorothea)],center=TRUE,scale = TRUE); rownames(DorotheaZScore) <- Dorothea[,1]
Dorothea_Cutoff <- Discretize_CARNIVAL(DorotheaZScore,DiscretDRT,DRT_Cutoff)
# rowSums(abs(Dorothea_Cutoff))

Progeny <- read.table("resources/E-MTAB-2091_PROGENy.csv",header=T,sep=",",stringsAsFactors = F)
ProgenyZScore <- scale(Progeny[,2:ncol(Progeny)],center=TRUE,scale = TRUE); rownames(ProgenyZScore) <- Progeny[,1]
Progeny_Cutoff <- Discretize_CARNIVAL(ProgenyZScore,DiscretPGN,PGN_Cutoff)
# rowSums(abs(Progeny_Cutoff))

# Load and process compounds names
Compounds <- Dorothea[,1]
Compounds_All <- NULL
for (counter in 1:length(Compounds)) {
  Compounds_All[[counter]] <- strsplit(Compounds[counter],split = " ",fixed = T)
}
Compounds_Names <- NULL
for (counter in 1:length(Compounds_All)) {
  if (length(Compounds_All[[counter]][[1]])==1) {
    Compounds_Names[[counter]] <- Compounds_All[[counter]][[1]][1]
  } else {
    if (grepl(pattern = "(",x = Compounds_All[[counter]][[1]][2],fixed = T)) {
      Compounds_Names[[counter]] <- Compounds_All[[counter]][[1]][1]
    } else {
      Compounds_Names[[counter]] <- paste0(Compounds_All[[counter]][[1]][1],"_",Compounds_All[[counter]][[1]][2])
    }
  }
}
Compounds_Names <- unlist(Compounds_Names)

# Build progeny catalogue
ProgenyProtein <- read.table("resources/PROGENy_Protein.csv",header = T,sep = ",",stringsAsFactors = F)
ProgenyProtein <- ProgenyProtein[order(ProgenyProtein[,2],ProgenyProtein[,3]),]
ProgenyProtein[,2] <- gsub("-",".",ProgenyProtein[,2],fixed=T)
ProgenyProtein_List <- vector(mode="list",length=length((unique(ProgenyProtein[,2]))))
for (counter in 1:length(unique(ProgenyProtein[,2]))) {
  ProgenyProtein_List[[counter]]$pw <- unique(ProgenyProtein[,2])[counter]
  Idx_pw_proteins <- which(unique(ProgenyProtein[,2])[counter]==ProgenyProtein[,2])
  ProgenyProtein_List[[counter]]$prot <- ProgenyProtein[Idx_pw_proteins,3]
}

# Create a directory to store results (directory's name generated based on cutoff)
Result_dir <- paste0("DRT_",
                     if (DiscretDRT==1) {paste0("AbsCutOff_",toString(DRT_Cutoff))} 
                     else if (DiscretDRT==2) {paste0("MeanSDCutOff_",toString(DRT_Cutoff))} 
                     else if (DiscretDRT==3) {paste0("MedianMADCutOff_",toString(DRT_Cutoff))}
                     ,"_PGN_",
                     if (DiscretPGN==1) {paste0("AbsCutOff_",toString(PGN_Cutoff))} 
                     else if (DiscretPGN==2) {paste0("MeanSDCutOff_",toString(PGN_Cutoff))} 
                     else if (DiscretPGN==3) {paste0("MedianMADCutOff_",toString(PGN_Cutoff))}) 

current_dir <- getwd()
dir.create("inputs",showWarnings = FALSE)
dir.create("measurements",showWarnings = FALSE)
setwd(paste(current_dir,"/measurements",sep=""))
if (is.null(Result_dir)) {
  dir_name <- paste("measurements_",Sys.time(),sep="")
} else {
  dir_name <- Result_dir
}
dir.create(dir_name,showWarnings = FALSE); setwd(current_dir)


for (counter in 1:nrow(Dorothea)) {

  # Dorothea as measurement
  Dorothea_Cutoff_current <- Dorothea_Cutoff[counter,]
  ColNamesNew <- substring(colnames(Dorothea_Cutoff),4)
  ColNamesSingle <- ColNamesNew[Dorothea_Cutoff_current!=0]
  Dorothea_Cutoff_current <- matrix(Dorothea_Cutoff_current[Dorothea_Cutoff_current!=0],nrow = 1,ncol = length(Dorothea_Cutoff_current[Dorothea_Cutoff_current!=0]))
  colnames(Dorothea_Cutoff_current) <- ColNamesSingle
  
  # Progeny as measurement
  Progeny_Cutoff_current <- Progeny_Cutoff[counter,]
  Progeny_Input_ToAdd <- NULL
  for (counter2 in 1:length(Progeny_Cutoff_current)) {
    if (Progeny_Cutoff_current[counter2]!=0) {
      Idx_ProgenyList <- which(colnames(Progeny_Cutoff)[counter2]==unique(ProgenyProtein[,2]))
      Matrix_ToAdd <- matrix(NA,2,length(ProgenyProtein_List[[Idx_ProgenyList]]$prot))
      Matrix_ToAdd[1,] <- ProgenyProtein_List[[Idx_ProgenyList]]$prot
      Matrix_ToAdd[2,] <- Progeny_Cutoff_current[counter2]
      Progeny_Input_ToAdd <- cbind(Progeny_Input_ToAdd,Matrix_ToAdd)
    }
  }
  if (!is.null(Progeny_Input_ToAdd)) {
    Progeny_Input <- matrix(Progeny_Input_ToAdd[2,],1,ncol(Progeny_Input_ToAdd))
    colnames(Progeny_Input) <- Progeny_Input_ToAdd[1,]
  } else {
    Progeny_Input <- NULL
  }  
  
  write.table(x = cbind(Dorothea_Cutoff_current,Progeny_Input),
              file = paste0("measurements/",Result_dir,"/Meas_DRT_PGN_",Compounds_Names[counter],".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)

  # Extract main input targets
  Stimuli <- read.table("resources/Stimuli.csv",header = T,sep = ",",stringsAsFactors = F)
  
  # Combined list of inputs
  All_Targets <- Stimuli$Gene[nchar(Stimuli$Gene)>0]
  All_Targets <- sort(unique(All_Targets))
  All_Targets_Matrix <- matrix(data = 0,nrow = nrow(Stimuli),ncol = length(All_Targets))
  # rownames(All_Targets_Matrix) <- Conditions
  colnames(All_Targets_Matrix) <- All_Targets
  for (counter3 in 1:nrow(Stimuli)) {
    IdxCond <- which(Stimuli[counter3,2]==Dorothea[,1])
    IdxTarget <- which(Stimuli[counter3,3]==All_Targets)
    All_Targets_Matrix[IdxCond,IdxTarget] <- Stimuli[counter3,4]
  }
  # current compounds
  All_Targets_current <- All_Targets_Matrix[counter,]
  Idx_NonZero <- which(All_Targets_current!=0)
  All_Targets_current <- matrix(All_Targets_current[Idx_NonZero],1,length(Idx_NonZero))
  colnames(All_Targets_current) <- All_Targets[Idx_NonZero]

  write.table(x = All_Targets_current, # cbind(All_Targets_current,STITCH_Targets_current)
              file = paste0("inputs/Inputs_Main_",Compounds_Names[counter],".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)
  
}

# --- End of script --- #
