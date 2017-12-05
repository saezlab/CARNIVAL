####
# Loading stuff
# rm(list=ls())
# source("script1.R")

library(readr)
library(tidyr)
library(igraph)

measurements <- read_delim("measurements_Case3.txt", 
                                           "\t", escape_double = FALSE, trim_ws = TRUE)

inputs <- read.table("inputs_Case3.txt", sep="\t", header = TRUE)

network <- read.table("network_Ex4_PosFB_1.sif", sep = "\t", header = FALSE)


data <- measurements
pknList <- as.data.frame(network)
colnames(pknList) <- c("Node1", "Sign", "Node2")
# upNodes <- "P"

source("~/Documents/GitHub/CRILPR/src/CRILPR_Functions.R")
source("all_constraints_wLoop.R")
source("write_dist_variables.R")
source("write_loop_constraints.R")

####
# Executing functions
dataMatrix <- buildDataMatrix(data = data, pknList = pknList, inputs = inputs, cutoff = 0.1)
variables <- create_variables_all(pknList = pknList, dataMatrix = dataMatrix)
distVariables <- write_dist_variables(pknList = pknList)
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
# allC <- all_constraints(c0 = c0, c1 = c1, c2 = c2, c3 = c3, c4 = c4, c5 = c5, c6 = c6, c7 = c7, c8 = c8)
c9 <- write_loop_constraints(variables = variables, distVariables = distVariables, pknList = pknList, inputs = inputs)
allC <- all_constraints_wLoop(c0 = c0, c1 = c1, c2 = c2, c3 = c3, c4 = c4, c5 = c5, c6 = c6, c7 = c7, c8 = c8, c9 = c9)

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
  
  for(j in 1:ncol(inputs)){

    write(variables[[i]]$dist[[j]], data, append = TRUE)

  }

}
write("End", data, append = TRUE)

system(paste0(getwd(), "/cplex -f cplexCommand.txt"))

file.remove(... = "cplex.log")

library(XML)
cplexSolutionFileName <- "results1.txt"
for(i in 1:length(variables)){
  
  readOutResult(cplexSolutionFileName = cplexSolutionFileName, variables = variables, pknList = pknList, conditionIDX = i)
  
}

file.remove(... = "results1.txt")
file.remove(... = "testFile.lp")
