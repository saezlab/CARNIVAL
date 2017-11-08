# -------------------- #
# CRIPLR driver script #
# -------------------- #

rm(list=ls()) # clear environment
cat("\014") # clear screen

Example    <- 1 # c(1,2,3)
Case_study <- 3 # c(1,2,3,4) or c(c(1,2),c(1,4))
Network    <- 2 # c(1,2) == c("positive","negative") / c("pos-pos","pos-neg") / c("same_sign","inverse_sign")

# ============================== #

# Load necessary packages and functions
library(readr)
library(tidyr)
library(XML)
source("src/CRILPR_Functions.R")

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
  network      <- read.table(paste("examples/Ex3/network_Ex1_",Net,".sif",sep=""), sep = "\t", header = FALSE)
  inputs       <- read.table(paste("examples/Ex3/inputs_Case", toString(Case_study), ".txt",sep=""), sep="\t", header = TRUE)
  measurements <- read_delim(paste("examples/Ex3/measurements_Case", toString(Case_study), ".txt",sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
}

data <- measurements
pknList <- as.data.frame(network)
colnames(pknList) <- c("Node1", "Sign", "Node2")

####
# Executing functions
dataMatrix <- buildDataMatrix(data = data, pknList = pknList, inputs = inputs, cutoff = 0.1)
variables <- create_variables_all(pknList = pknList, dataMatrix = dataMatrix)
bounds <- write_boundaries(variables = variables)
oF <- write_objective_function_all(dataMatrix = dataMatrix, variables = variables, alpha = 1, beta = 0.01)
c0 <- write_constraints_objFunction_all(variables = variables, dataMatrix = dataMatrix)
c1 <- write_constraints_1_all(variables = variables)
c2 <- write_constraints_2_all(variables = variables)
c3 <- write_constraints_3_all(variables = variables)
c4 <- write_constraints_4_all(variables = variables)
c5 <- write_constraints_5_all(variables = variables)
c6 <- write_constraints_6(variables = variables, dataMatrix = dataMatrix)
c7 <- write_constraints_7(variables = variables, dataMatrix = dataMatrix)
c8 <- write_constraints_8(variables = variables, inputs = inputs)
allC <- all_constraints(c0 = c0, c1 = c2, c2 = c2, c3 = c3, c4 = c4, c5 = c5, c6 = c6, c7 = c7, c8 = c8)

# write the .lp file
data = "testFile.lp"
write("enter Problem", data)
write("", data, append = TRUE)
write("Minimize", data, append = TRUE)
write(oF, data, append = TRUE)
write("Subject To", data, append = TRUE)
write(allC, data, append = TRUE)
write("Bounds", data, append = TRUE)
write(bounds, data, append = TRUE)
write("Integers", data, append = TRUE)
for(i in 1:length(variables)){
  
  write(variables[[i]]$variables, data, append = TRUE)
  
}
write("End", data, append = TRUE)

system(paste0(getwd(), "/cplex -f cplexCommand.txt"))

file.remove(... = "cplex.log")

cplexSolutionFileName <- "results1.txt"

for(i in 1:length(variables)){
  
  dir_name <- paste("results_",Sys.time(),sep="")
  dir.create(dir_name)
  readOutResult(cplexSolutionFileName = cplexSolutionFileName, variables = variables, pknList = pknList, conditionIDX = i,dir_name = dir_name)
  
}

variables$Condition_1$exp

# file.remove(... = "results1.txt")
file.remove(... = "testFile.lp")
