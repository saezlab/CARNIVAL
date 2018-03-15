# ========================================================================= #
# Integrated pipeine for CARNIVAL input file generator from DrugBank/STITCH #
# ========================================================================= #

# Steps to proceed
# 1) Extract information from DrugBank and map IDs with HGNC symbol
# 2) Extract information from STITCH and map IDs with HGNC symbol
# 3) Select +/- combine results and write input file

# Clear variables, screen and figures
rm(list=ls()); cat('\014'); if (length(dev.list()>0)) {dev.off()}

# Select parameters for STITCH as additional inputs
STITCH_Input <- T # include STITCH target as inputs (if any)
AcceptedScore <- 800 # Accepted score to be included in the list (700 [Melas et al.] to 800)

# Try loading biomaRt multiple times (if it returns any error) and comment this part before proceeding
library(biomaRt)
try(mart <- useMart(biomart = "ensembl", dataset = "hsapiens_gene_ensembl"))
if(inherits(mart, "try-error")) {mart <- useMart(biomart = "ensembl", dataset = "hsapiens_gene_ensembl")}

# ===================================================== #
# ===== Step 1: Extract information from DrugBank ===== #
# ===================================================== #

# Assign the list of compounds and their corresponding DrugBankID

# Manual assignment
DrugBankNameID <- matrix(NA,5,2)
DrugBankNameID[,1] <- c("APAP","CCl4","CYCA","DFN","PPNL")
# DrugBankNameID[,1] <- c("ACETAMINOPHEN","CARBON_TETRACHLORIDE","CYCLOSPORINE","DICLOFENAC","PROPRANOLOL")
DrugBankNameID[,2] <- c("DB00316",NA,"DB00091","DB00586","DB00571")

# Automated assignment
ListOfDrugs <- c("ACETAMINOPHEN","CARBON_TETRACHLORIDE","CYCLOSPORINE","DICLOFENAC","PROPRANOLOL")
DrugNameIDCHEML <- read.table("resources/DrugName_DrugBankID_CHEMBL_mapping.csv",header = T,sep = "\t",stringsAsFactors = F)
DrugNameIDCHEML <- unique(DrugNameIDCHEML)
DrugBankNameID <- matrix(NA,length(ListOfDrugs),2)
for (counter in 1:length(ListOfDrugs)) {
  DrugBankNameID[counter,1] <- ListOfDrugs[counter]
  if (length(which(ListOfDrugs[counter]==DrugNameIDCHEML$name))>0) {
    DrugBankNameID[counter,2] <- DrugNameIDCHEML$drugId[which(ListOfDrugs[counter]==DrugNameIDCHEML$name)]
  } else {
    DrugBankNameID[counter,2] <- NA
  }
}

colnames(DrugBankNameID) <- c("Name","DrugBankID")

# Load target list from Himmelstein's processed DrugBank data
DrugBankTargets <- read.table("resources/Himmelstein_DrugBank.tsv",header = T,sep = "\t",stringsAsFactors = F)

SelectedDBtargets <- list()
for (counter in 1:nrow(DrugBankNameID)) {
  if (!is.na(DrugBankNameID[counter,2])) {
    IdxTargetsID <- which(DrugBankTargets$drugbank_id==DrugBankNameID[counter,2])
    SelectedDBtargets[[counter]] <- DrugBankTargets[IdxTargetsID,]
    SelectedDBtargets[[counter]] <- SelectedDBtargets[[counter]][which(SelectedDBtargets[[counter]]$organism=="Human"),]
    SelectedDBtargets[[counter]] <- SelectedDBtargets[[counter]][which(SelectedDBtargets[[counter]]$category=="target"),]
  } else {
    SelectedDBtargets[[counter]] <- NULL
  }
}


# Load Mapping database, then map drug targets and their actions onto a new list
HGNCmapping <- read.delim("resources/hgnc_complete_set.txt",header = T,sep = "\t",stringsAsFactors = F)

MappedSelectedDBtargets <- list()
for (counter in 1:length(SelectedDBtargets)) {
  if (!is.null(SelectedDBtargets[[counter]])){
    TargetsActions <- matrix(NA,2,nrow(SelectedDBtargets[[counter]]))
    TargetsUniProt <- SelectedDBtargets[[counter]][,3]
    TargetsUpDown <- SelectedDBtargets[[counter]][,7]
    TargetsHGNC <- NULL; ActionsHGNC <- NULL
    for (counter2 in 1:length(TargetsUniProt)) {
      TargetsHGNC <- c(TargetsHGNC,HGNCmapping$symbol[which(TargetsUniProt[counter2]==HGNCmapping$uniprot_ids)])
      if (((grepl(pattern = "inhibitor",x = TargetsUpDown[counter2],fixed = T) | grepl(pattern = "antagonist",x = TargetsUpDown[counter2],fixed = T) | grepl(pattern = "negative modulator",x = TargetsUpDown[counter2],fixed = T)) 
           & (grepl(pattern = "inducer",x = TargetsUpDown[counter2],fixed = T) | grepl(pattern = "activator",x = TargetsUpDown[counter2],fixed = T) | grepl(pattern = " agonist",x = TargetsUpDown[counter2],fixed = T)))  
          | grepl(pattern = "binder",x = TargetsUpDown[counter2],fixed = T) ) { # mixed effect or binding with unknown effect
        ActionsHGNC <- c(ActionsHGNC,NaN)
      } else if (grepl(pattern = "inhibitor",x = TargetsUpDown[counter2],fixed = T) | grepl(pattern = "antagonist",x = TargetsUpDown[counter2],fixed = T) | grepl(pattern = "negative modulator",x = TargetsUpDown[counter2],fixed = T)) {
        ActionsHGNC <- c(ActionsHGNC,-1)
      } else if (grepl(pattern = "inducer",x = TargetsUpDown[counter2],fixed = T) | grepl(pattern = "activator",x = TargetsUpDown[counter2],fixed = T) | grepl(pattern = " agonist",x = TargetsUpDown[counter2],fixed = T)) {
        ActionsHGNC <- c(ActionsHGNC,1)
      } else {
        ActionsHGNC <- c(ActionsHGNC,0)
      }
    }
    TargetsActions[1,] <- TargetsHGNC
    TargetsActions[2,] <- ActionsHGNC
    MappedSelectedDBtargets[[counter]] <- TargetsActions
    if (length(which(MappedSelectedDBtargets[[counter]][2,]==0))>0) {
      MappedSelectedDBtargets[[counter]] <- MappedSelectedDBtargets[[counter]][,-which(MappedSelectedDBtargets[[counter]][2,]==0)]
    }
  } else {
    MappedSelectedDBtargets[[counter]] <- NULL
  }
}


# ==================================================== #
# ===== Step 2: Extract information from STITCH  ===== #
# ==================================================== #


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

for (counter in 1:nrow(DrugBankNameID)) {

  # Add STITCH as target molecules
  CPName <- DrugBankNameID[counter,1]
  CPID <- STITCH_CID[counter]
  
  print(paste0("Mapping Compound: ",DrugBankNameID[counter,1]," - ",toString(counter),"/",toString(nrow(DrugBankNameID))))
  
  Main_Targets_DB <- rep(NA,1,ncol(MappedSelectedDBtargets[[counter]]))
  Main_Targets_DB <- t(as.data.frame(as.numeric(MappedSelectedDBtargets[[counter]][2,])))
  colnames(Main_Targets_DB) <- MappedSelectedDBtargets[[counter]][1,]; rownames(Main_Targets_DB) <- NULL
  
  All_Targets_current <- Main_Targets_DB
  
  
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
              file = paste0("Inputs_Main_STITCH_",DrugBankNameID[counter,1],"_CutOff_",toString(AcceptedScore),".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)
  
  # write.table(STITCH_CP_Merged[order(STITCH_CP_Merged$mgi_symbol),],file = paste("STITCH_", CPName , "_Targets_Mouse.tsv",sep=""),quote = FALSE,sep = "\t",row.names = FALSE,col.names = TRUE)
  
  write.table(x = All_Targets_current, 
              file = paste0("Inputs_Main_",DrugBankNameID[counter,1],".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)
}


# --- End of the script --- #
