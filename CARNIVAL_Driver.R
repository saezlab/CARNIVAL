# ---------------------- #
# CARNIVAL driver script #
# ---------------------- #

# setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL") # set working directory
rm(list=ls()) # clear environment
cat("\014") # clear screen
if (length(dev.list())>0){dev.off()} # clear figure (if any)

# Select a case study [Note: please add your another Example and paths to inputs files for your own study below]
Example     <- 1 # c(1,2,3,4,5,6,7,8) # Ex1-3: Simplified motifs; Ex4: Feedback/Cycle motif; Ex5: Mike's example; Ex6: Propanolol example; Ex7: ToyWeight; Ex8: Inverse Causal reasoning of WGCNA modules
Case_study  <- 1 # c(1,2,3,4) # see corresponding experimental setting
Network     <- 1 # c(1,2) # see corresponding choices of networks
AddPertubationNode <- 0 # Add perturbation node (inverse causal reasoning pipeline)

# Set CPLEX stopping criteria
mipGAP      <- 0.10 # in proportion to the best estimated solution
poolrelGAP  <- 0.01 # populate more solutions in relative to the best solution
timelimit   <- 1800 # in seconds

# Choose results exporting options
Result_dir  <- paste0("Ex",toString(Example),"Case",toString(Case_study),"Net",toString(Network)) # specify a name for result directory; if NULL, then date and time will be used by default
Export_all  <- 0 # c(0,1) export all ILP variables or not; if 0, only predicted node values, sif and dot files will be written

# ============================== #

# Load necessary packages and functions
library(readr)
library(tidyr)
library(XML)
source("src/CRILPR_Functions.R")

# Create a directory to store results
current_dir <- getwd()
dir.create("results",showWarnings = FALSE)
setwd(paste(current_dir,"/results",sep=""))
if (is.null(Result_dir)) {
  dir_name <- paste("results_",Sys.time(),sep="")
} else {
  dir_name <- Result_dir
}
dir.create(dir_name); setwd(current_dir)

# Load ILP inputs
if (Example == 1) {
  if (Network == 1) { Net <- "positive" } else if (Network == 2) { Net <- "negative" }
  network      <- read.table(paste("examples/Ex1/network_Ex1_",Net,".sif",sep=""), sep = "\t", header = FALSE)
  inputs       <- read.table(paste("examples/Ex1/inputs_Case", toString(Case_study), ".txt",sep=""), sep="\t", header = TRUE)
  measurements <- read_delim(paste("examples/Ex1/measurements_Case", toString(Case_study), ".txt",sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
} else if (Example == 2) {
  if (Network == 1) { Net <- "PosPos" } else if (Network == 2) { Net <- "PosNeg" }
  network      <- read.table(paste("examples/Ex2/network_Ex2_",Net,".sif",sep=""), sep = "\t", header = FALSE)
  inputs       <- read.table(paste("examples/Ex2/inputs_Case", toString(Case_study), ".txt",sep=""), sep="\t", header = TRUE)
  measurements <- read_delim(paste("examples/Ex2/measurements_Case", toString(Case_study), ".txt",sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
} else if (Example == 3) {
  if (Network == 1) { Net <- "SameSign" } else if (Network == 2) { Net <- "InverseSign" }
  # network      <- read.table(paste("examples/Ex3/network_Ex3_",Net,".sif",sep=""), sep = "\t", header = FALSE)
  network      <- read.table(paste("examples/Ex3/network_Ex3_",Net,"_ReOrder.sif",sep=""), sep = "\t", header = FALSE)
  inputs       <- read.table(paste("examples/Ex3/inputs_Case", toString(Case_study), ".txt",sep=""), sep="\t", header = TRUE)
  measurements <- read_delim(paste("examples/Ex3/measurements_Case", toString(Case_study), ".txt",sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
} else if (Example == 4) {
  if (Network == 1) { Net <- "PosFB" } else if (Network == 2) { Net <- "PosFB_plusOne" }
  network      <- read.table(paste("examples/Ex4/network_Ex4_",Net,".sif",sep=""), sep = "\t", header = FALSE)
  inputs       <- read.table(paste("examples/Ex4/inputs_Case", toString(Case_study), ".txt",sep=""), sep="\t", header = TRUE)
  measurements <- read_delim(paste("examples/Ex4/measurements_Case", toString(Case_study), ".txt",sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
} else if (Example == 5) {
  if (Network == 1) { Net <- "Mike" } else if (Network == 2) { Net <- "Extended" }
  network      <- read.table(paste("examples/Ex5/network_Ex5_",Net,".sif",sep=""), sep = "\t", header = FALSE)
  inputs       <- read.table(paste("examples/Ex5/inputs_Case", toString(Case_study), ".txt",sep=""), sep="\t", header = TRUE)
  measurements <- read_delim(paste("examples/Ex5/measurements_Case", toString(Case_study), ".txt",sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
} else if (Example == 6) {
  if (Network == 1) {
    network      <- read.table("examples/Ex6/OmniPathSIF_NoHyphen.tsv", sep = "\t", header = FALSE,stringsAsFactors = F)
  } else if (Network == 2) {
    network      <- read.table("examples/Ex6/Network_Generic_FIN_RemovedTFGenes_NoHyphen.sif", sep = "\t", header = FALSE,stringsAsFactors = F)
  } else if (Network == 3) {
    network      <- read.delim("examples/Ex6/SignorSIF_NoHyphenNoSlashNoColonNoSpace.tsv", sep = "\t", header = FALSE,stringsAsFactors = F)
  }
  if (Case_study == 1) {
    inputs       <- read.table("examples/Ex6/Inputs_Main_ACETAMINOPHEN.tsv", sep="\t", header = TRUE)
  } else if (Case_study == 2) {
    inputs       <- read.table("examples/Ex6/Inputs_Main_STITCH_ACETAMINOPHEN_CutOff_800.tsv", sep="\t", header = TRUE)
  }
  measurements <- read_delim("examples/Ex6/ILP_Meas_APAP_high_24h_CutOff_1.5.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)
  # measurements <- read_delim("examples/Ex6/ILP_Meas_APAP_low_2h_CutOff_1.5.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)
} else if (Example == 7) {
  network      <- read.table("examples/Ex7/Network_ToyWeight.txt", sep = "\t", header = FALSE)
  inputs       <- read.table("examples/Ex7/Input_ToyWeight.txt", sep="\t", header = TRUE)
  measurements <- read_delim("examples/Ex7/Meas_ToyWeight.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
  if (Case_study==2) {
    pathwayscore <- read.table("examples/Ex7/PathwayScore_ToyWeight.txt", sep = "\t", header = TRUE)
  }
} else if (Example == 8) {
  network      <- read.table("examples/Ex8/pkn_reduced_omnipath.sif", sep = "\t", header = TRUE,stringsAsFactors = F)
  inputs       <- read.table("examples/Ex8/inputs_for_pkn_reduced_omnipath.sif", sep="\t", header = TRUE)
  measurements <- read_delim("examples/Ex8/meas_for_pkn_reduced_omnipath.sif","\t", escape_double = FALSE, trim_ws = TRUE)
} else {
  stop("Please select the provided examples or add your own example to the list")
}

# Adding perturbation node?
if (AddPertubationNode==1) {
  NameInput <- names(inputs)
  AddToNet <- data.frame(matrix(NA,length(NameInput)*2,3))
  AddToNet[,1] <- "Perturbation"
  AddToNet[1:length(NameInput),2] <- "1";AddToNet[1:length(NameInput),3] <- NameInput
  AddToNet[(length(NameInput)+1):(length(NameInput)*2),2] <- "-1";AddToNet[(length(NameInput)+1):(length(NameInput)*2),3] <- NameInput
  colnames(AddToNet) <- colnames(network)
  network <- rbind(network,AddToNet)
  inputs <- data.frame("NaN"); colnames(inputs) <- "Perturbation"
}

# Input processing
data <- measurements
pknList <- as.data.frame(network)
colnames(pknList) <- c("Node1", "Sign", "Node2")
setwd(paste0(current_dir,"/src/")) # temporary shift to src directory

if (exists("pathwayscore")) {
  if (nrow(pathwayscore)==1) {
    if (length(unique(as.numeric(pathwayscore)))!=1) {
      nodeWeights <- 1 - 2*((abs(pathwayscore) - min(abs(pathwayscore),na.rm=T)) / (max(abs(pathwayscore),na.rm=T) - min(abs(pathwayscore),na.rm=T)))
    } else { 
      nodeWeights <- pathwayscore # if all pathway scores are the same, then use pathwayscore as the nodeWeights directly
    }
    names(nodeWeights) <- colnames(pathwayscore)
    scores <- as.numeric(pathwayscore); names(scores) <- names(nodeWeights)
  } else {
    pathwayscore_current <- pathwayscore[1,]
    nodeWeights <- NULL
    for (counter_pw in 1:ncol(pathwayscoreAll)) {
      ss <- abs(as.numeric(as.matrix(pathwayscoreAll[, counter_pw])))
      nodeWeights <- c(nodeWeights, 1-2*(ss[1]-min(ss, na.rm = TRUE))/(max(ss, na.rm = TRUE)-min(ss, na.rm = TRUE)))
    }
    names(nodeWeights) <- colnames(pathwayscore)
    scores <- as.numeric(pathwayscore_current); names(scores) <- names(nodeWeights)
  }
} else {
  scores <- NULL
  nodeWeights <- NULL
}

# Remove intermediate cplex files (if any)
AllFiles <- list.files()
CloneFiles <- which(grepl(pattern = "clone",x = AllFiles,fixed = T))
if (length(CloneFiles)>0) {
  for (counter in 1:length(CloneFiles)) {
    file.remove(AllFiles[CloneFiles[counter]])
  }
}
if (file.exists("testFile.lp")) {file.remove("testFile.lp")}
if (file.exists("results_cplex.txt")) {file.remove("results_cplex.txt")}
if (file.exists("cplex.log")) {file.remove("cplex.log")}
if (file.exists("cplexCommand.txt")) {file.remove("cplexCommand.txt")}

# Write constraints as ILP inputs
ptm <- proc.time()
print("Writing constraints...")
variables <- writeLPFile(data,pknList,inputs,0.1,alphaWeight=100,betaWeight=20,scores=scores,mipGAP=mipGAP,poolrelGAP=poolrelGAP,timelimit=timelimit,nodeWeights=nodeWeights)
Elapsed_1 <- proc.time() - ptm

# Solve ILP problem with cplex, remove temp files, and return to the main directory
ptm <- proc.time()
print("Solving LP problem...")
system(paste0(getwd(), "/cplex -f cplexCommand.txt"))
Elapsed_2 <- proc.time() - ptm

if (file.exists("testFile.lp")) {file.remove("testFile.lp")} # might be useful for debugging 
if (file.exists("results_cplex.txt")) {file.copy(from = "results_cplex.txt",to = paste(current_dir,"/results/",dir_name,"/results_cplex.txt",sep="")); file.remove("results_cplex.txt")}
if (file.exists("cplex.log")) {file.copy(from = "cplex.log",to = paste(current_dir,"/results/",dir_name,"/cplex.log",sep="")); file.remove("cplex.log")}
if (file.exists("cplexCommand.txt")) {file.remove("cplexCommand.txt")}
setwd(current_dir)

# Write result files
ptm <- proc.time()
print("Writing result files...")
if (file.exists(paste("results/",dir_name,"/results_cplex.txt",sep=""))) {
  for(i in 1:length(variables)){
    sif <- readOutResult(cplexSolutionFileName = paste0("results/",dir_name,"/results_cplex.txt"), variables = variables, pknList = pknList, conditionIDX = i,dir_name = dir_name, Export_all = Export_all,inputs=inputs,measurements=measurements)
  }
} else {
  print("No result to be written")
}
Elapsed_3 <- proc.time() - ptm

# Logged computational time
ElapsedAll <- as.data.frame(matrix(t(c(Elapsed_1[3],Elapsed_2[3],Elapsed_3[3])),3,1))
rownames(ElapsedAll) <- c("WriteConstraints:","CplexSolving:","ExportResults:")
write.table(x = ElapsedAll,file = paste("results/",dir_name,"/elapsed_time.txt",sep=""),col.names = F,row.names = T,quote = F)

# --- End of script --- #
