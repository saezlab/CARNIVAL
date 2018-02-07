# Input files generator

rm(list=ls());cat("\014");if (length(dev.list()>0)){dev.off()}

# Try loading biomaRt multiple times (if it returns any error) and comment this part before proceeding
library(biomaRt)
try(mart <- useMart(biomart = "ensembl", dataset = "hsapiens_gene_ensembl"))
if(inherits(mart, "try-error")) {mart <- useMart(biomart = "ensembl", dataset = "hsapiens_gene_ensembl")}

# Select discretisation measure for Dorothea (DRT) & Progeny (PGN)
# 1=absolute Cutoff value, 2=mean+/-Cutoff*SD (Gaussian), 3= median+/-Cutoff*mean_abs_diff
DiscretDRT <- 2; DRT_Cutoff <- 2
DiscretPGN <- 2; PGN_Cutoff <- 2

# Select parameters for STITCH as additional inputs
STITCH_Input <- F # include STITCH target as inputs (if any)
AcceptedScore <- 800 # Accepted score to be included in the list (700 [Ioannis] to 800)

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

# Extract compounds' targets from STITCH

if (STITCH_Input) {
  
  # Load STITCH database interactions and map CID(m)
  print("Loading STITCH database... will take a few minutes...")
  load("resources/STITCH.RData") # still takes a few minutes
  # load("STITCH_Mouse.RData") # still takes a few minutes
  # STITCH <- STITCH_Mouse
  STITCH_List <- read.table("resources/Compound_CID_plusManualCuration.csv",header=T,sep=",",stringsAsFactors = F)
  STITCH_CID <- rep("CIDm00000000",nrow(STITCH_List))
  
  for (counter in 1:nrow(STITCH_List)) {
    if (!is.na(STITCH_List[counter,2])) {
      substr(STITCH_CID[counter],12-nchar(STITCH_List[counter,2])+1,12) <- toString(STITCH_List[counter,2])
    } else {
      STITCH_CID[counter] <- NA
    }
  }
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

  print("")
  print("=========================================")
  print(paste0("Writing files for compound Nr ", toString(counter),"/",toString(length(Compounds))," : " ,Compounds[counter]))
  print("=========================================")
  print("")
  
  
  # === Writing measurements file ==== #
  
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

  
  # === Writing input file ==== #
  
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

  if (STITCH_Input) {
  
    # Add STITCH as target molecules
    
    CPName <- Compounds_Names[counter]
    CPID <- STITCH_CID[counter]
  
    # Extract STITCH entry from the full flat file
  
    if (!is.na(CPID)) {
      
      # Select entries where compound 1) is a source of interaction, 2) has known effect, 3) score >= accepted score
      STITCH_CP <- STITCH[STITCH$item_id_a==CPID & STITCH$a_is_acting=="t" & nchar(STITCH$action)!=0 & STITCH$score>=AcceptedScore,]
      # STITCH_CP <- STITCH[STITCH$item_id_a==CPID & STITCH$a_is_acting=="t" & nchar(STITCH$action)!=0,] # this variant take also expression entries
      
      if (nrow(STITCH_CP)>0) {
      
        for (counter4 in 1:nrow(STITCH_CP)) {
          # print(paste(toString(counter), "/", toString(nrow(STITCH_CP)),sep=" "))
          STITCH_CP[counter4,2] <- substring(as.character(STITCH_CP[counter4,2]),6)
          # STITCH_CP[counter,2] <- substring(as.character(STITCH_CP[counter,2]),7)
        }
        
        # Mapping with Biomart
        # Define biomart object
        # mart <- useMart(biomart = "ensembl", dataset = "mmusculus_gene_ensembl")
        # Query biomart
        results <- getBM(attributes = c("hgnc_symbol", "ensembl_peptide_id"),
                         filters = "ensembl_peptide_id", values = STITCH_CP[,2],
                         mart = mart)
        # results <- getBM(attributes = c("mgi_symbol", "ensembl_peptide_id"),
        #                  filters = "ensembl_peptide_id", values = STITCH_CP[,2],
        #                  mart = mart)
        # Merge entries
        colnames(STITCH_CP)[2] <- "ensembl_peptide_id"
        STITCH_CP_Merged <- merge(STITCH_CP,results,by = "ensembl_peptide_id",all = T)
      
        # Remove unannotated entries by Biomart (NA in hgnc_symbol)
        if (sum(is.na(STITCH_CP_Merged$hgnc_symbol))>0) {
          STITCH_CP_Merged <- STITCH_CP_Merged[-which(is.na(STITCH_CP_Merged$hgnc_symbol)),]
        }
        
        # Remove redundancy -> if exist -> take the entry with higher score
        
        STITCH_CP_Merged <- STITCH_CP_Merged[order(STITCH_CP_Merged$hgnc_symbol),]
        
        Idx2rm <- NULL
        AllHGNC <- unique(STITCH_CP_Merged$hgnc_symbol)
        for (counter5 in 1:length(AllHGNC)) {
          IdxEntries <- which(AllHGNC[counter5]==STITCH_CP_Merged$hgnc_symbol)
          if (length(IdxEntries)>1) {
            NonMax <- IdxEntries[which(STITCH_CP_Merged$score[IdxEntries]!=max(STITCH_CP_Merged$score[IdxEntries]))]
            Idx2rm <- c(Idx2rm, NonMax)
            if (length(NonMax)==0) { # if all values are max
              Idx2rm <- c(Idx2rm, IdxEntries[-1]) # remove also the non-first entry
            }
          }
        }
        
        if (!is.null(Idx2rm)) {
          STITCH_CP_Merged <- STITCH_CP_Merged[-Idx2rm,]
        }
        
        for (counter6 in 1:nrow(STITCH_CP_Merged)) {
          if (STITCH_CP_Merged$action[counter6]=="activation") {
            STITCH_CP_Merged$action[counter6] <- 1
          } else if (STITCH_CP_Merged$action[counter6]=="inhibition") { 
            STITCH_CP_Merged$action[counter6] <- -1
          }
        }
        
        Input_STITCH <- data.frame(matrix(NA,1,nrow(STITCH_CP_Merged)))
        colnames(Input_STITCH) <- STITCH_CP_Merged$hgnc_symbol
        Input_STITCH[1,] <- STITCH_CP_Merged$action
        
        CombinedTargets <- cbind(All_Targets_current,Input_STITCH)
        ColNamesCT <- unique(colnames(CombinedTargets))
        ColNamesCTIdx <- NULL
        for (counter7 in 1:length(ColNamesCT)) {
          Current_CTIdx <- which(ColNamesCT[counter7]==colnames(CombinedTargets))
          ColNamesCTIdx <- c(ColNamesCTIdx,Current_CTIdx[1]) # if molecule overlapped, take the first one from known target (STITCH has lower priority)
        }
        
        FinalCT <- cbind(All_Targets_current,Input_STITCH)[ColNamesCTIdx]
      
      } else {
        FinalCT <- All_Targets_current
      }
    } else {
      FinalCT <- All_Targets_current
    }

    
    write.table(x = FinalCT, 
                file = paste0("inputs/Inputs_Main_STITCH_",Compounds_Names[counter],"_CutOff_",toString(AcceptedScore),".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)
    
    # write.table(STITCH_CP_Merged[order(STITCH_CP_Merged$mgi_symbol),],file = paste("STITCH_", CPName , "_Targets_Mouse.tsv",sep=""),quote = FALSE,sep = "\t",row.names = FALSE,col.names = TRUE)
  
  } else {
  
    write.table(x = All_Targets_current, 
                file = paste0("inputs/Inputs_Main_",Compounds_Names[counter],".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)
  }
  
}

# --- End of script --- #
