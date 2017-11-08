####
# Loading stuff
rm(list=ls())
source("script1.R")

library(readr)

measurements <- read_delim("measurements_Case1_AND_Case4.txt", 
                                           "\t", escape_double = FALSE, trim_ws = TRUE)

inputs <- read.table("inputs_Case1_AND_Case4.txt", sep="\t", header = TRUE)

network <- read.table("network_Ex1_positive.sif", sep = "\t", header = FALSE)


data <- measurements
pknList <- as.data.frame(network)
colnames(pknList) <- c("Node1", "Sign", "Node2")
# upNodes <- "P"

####
# Executing functions
dataMatrix <- buildDataMatrix(data = data, pknList = pknList, inputs = inputs, cutoff = 0.1)
variables <- create_variables(pknList = pknList, dataMatrix = dataMatrix)
bounds <- write_boundaries(variables = variables)
oF <- write_objective_function(dataMatrix = dataMatrix, variables = variables, alpha = 1, beta = 0.01)
c0 <- write_constraints_objFunction(variables = variables, dataMatrix = dataMatrix)
c1 <- write_constraints_1(variables = variables)
c2 <- write_constraints_2(variables = variables)
c3 <- write_constraints_3(variables = variables)
c4 <- write_constraints_4(variables = variables)
c5 <- write_constraints_5(variables = variables)
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
write(variables$variables, data, append = TRUE)
write("End", data, append = TRUE)

system(paste0(getwd(), "/cplex -f cplexCommand.txt"))

file.remove(... = "cplex.log")
# file.remove(... = "testFile.lp")

variables$exp
