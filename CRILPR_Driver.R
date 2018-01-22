# -------------------- #
# CRILPR driver script #
# -------------------- #

rm(list=ls()) # clear environment
cat("\014") # clear screen

# Select a case study [Note: please add your another Example and paths to inputs files for your own study below]
Example    <- 101 # c(1,2,3,4,5,6,101,102) # Ex4 = Feedback/Cycle motif, Ex5 = Mike's example; Ex6 = Propanolol example; Ex101 = Validation Single; Ex102 = Validation Multiple
Case_study <- 2 # c(1,2,3,4) or c(c(1,2),c(1,4))
Network    <- 1 # c(1,2) == c("positive","negative") / c("pos-pos","pos-neg") / c("same_sign","inverse_sign") / c("ABC","SABC") / "Mike" / "PPNL" / c("Omnipath","Signor","Babur")
Result_dir <- "Ex101Case2Net1" # specify a name for result directory; if NULL, then date and time will be used by default
Export_all <- 0 # c(0,1) export all ILP variables or not; if 0, only cplex results, predicted node values and sif file will be written

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
  network      <- read.table(paste("examples/Ex3/network_Ex3_",Net,".sif",sep=""), sep = "\t", header = FALSE)
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
  # network      <- read.table("examples/Ex6/Network_Generic_FIN_RemovedTFGenes.sif", sep = "\t", header = FALSE)
  network      <- read.table("examples/Ex6/Network_Generic_FIN_RemovedTFGenes_NoHyphen.sif", sep = "\t", header = FALSE)
  inputs       <- read.table("examples/Ex6/Propanolol_inputs_only_ADRB1.txt", sep="\t", header = TRUE)
  # inputs       <- read.table("examples/Ex6/Propanolol_inputs.txt", sep="\t", header = TRUE)
  measurements <- read_delim("examples/Ex6/TFActs_TGG_PPNL_Human_ivt_24h_high_UpDown.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
} else if (Example == 101) {
  network      <- read.table("examples/ValEx1/OmniPathSIF_NoHyphen.tsv", sep = "\t", header = FALSE)
  # inputs       <- read.table("examples/ValEx1/DrugTarget_Input_betaxolol.tsv", sep="\t", header = TRUE)
  inputs       <- read.table("examples/ValEx1/DrugTarget_Input_betaxolol_plusPROGENy.tsv", sep="\t", header = TRUE)
  measurements <- read_delim("examples/ValEx1/Dorothea_CutOff_Abs_1p5_betaxolol", "\t", escape_double = FALSE, trim_ws = TRUE)
} else {
  stop("Please select the provided examples or add your own example to the list")
}

# Input processing
data <- measurements
pknList <- as.data.frame(network)
colnames(pknList) <- c("Node1", "Sign", "Node2")
setwd(paste0(current_dir,"/src/")) # temporary shift to src directory

# Write constraints as ILP inputs
ptm <- proc.time()
variables <- writeLPFile(data,pknList,inputs,0.1)
proc.time() - ptm

# Solve ILP problem with cplex, remove temp files, and return to the main directory
system(paste0(getwd(), "/cplex -f cplexCommand.txt"))
# file.remove("testFile.lp")
file.remove("cplex.log")
file.copy(from = "results_cplex.txt",to = paste(current_dir,"/results/",dir_name,"/results_cplex.txt",sep=""))
file.remove("results_cplex.txt")
setwd(current_dir)

# Write result files
ptm <- proc.time()
for(i in 1:length(variables)){
  sif <- readOutResult(cplexSolutionFileName = paste("results/",dir_name,"/results_cplex.txt",sep=""), variables = variables, pknList = pknList, conditionIDX = i,dir_name = dir_name, Export_all = Export_all)
}
proc.time() - ptm

# --- End of script --- #

# [Debug/QC mode] Print variable names
variables$Condition_1$exp

# --- End of debugging part --- #
