#### Create Dummy Data 1 ####

#Set Amount of nodes
#Always use this vector of names to make sure all names are the same
nodenames = c(paste("Node", seq(1, 5), sep = ""))
perturbations_1 = c(1,-1, 1) 
names(perturbations_1) = c(nodenames[1], nodenames[2], nodenames[3])

measurements_1 = c(5)
names(measurements_1) = c(nodenames[5])

priorKnowledgeNetwork_1 = matrix(c(nodenames[1], 1, nodenames[4], 
                                   nodenames[2], 1, nodenames[4],
                                   nodenames[3], -1, nodenames[1],
                                   nodenames[3], -1, nodenames[2],
                                   nodenames[4], 1, nodenames[5]), 
                                   byrow = T, ncol = 3) %>% as.data.frame()
colnames(priorKnowledgeNetwork_1) = c("source", "interaction", "target")

input_1 = prerunCarnival(perturbations = perturbations_1, 
                         measurements = measurements_1,
                         priorKnowledgeNetwork = priorKnowledgeNetwork_1,
                         newDataRepresentation = T,
                         solver = supportedSolvers$cplex, 
                         solverPath = solverPath,
                         carnivalOptions = carnivalOptions)



save(perturbations_1, file = "PT_1.Rdata")
save(measurements_1, file = "MS_1")
save(priorKnowledgeNetwork_1, file = "PKN_1")

#### Create Expected Output 1 ####

constraint_1_1 <- c(createConstraint("eU1", "-", "n1", ">=", "0"),
                    createConstraint("eU2", "-", "n2", ">=", "0"),
                    createConstraint("eU5", "-", "n4", ">=", "0"),
                    createConstraint("eU3", "+", "n3", ">=", "0"),
                    createConstraint("eU4", "+", "n3", ">=", "0"))

constraint_2_1 <- c(createConstraint("eD1", "+", "n1", ">=", "0"),
                    createConstraint("eD2", "+", "n2", ">=", "0"),
                    createConstraint("eD5", "+", "n4", ">=", "0"),
                    createConstraint("eD3", "-", "n3", ">=", "0"),
                    createConstraint("eD4", "-", "n3", ">=", "0"))

output_1 = rbind(Constraint_1_1, Constraint_2_1)
save(Output_1, file = "Dummy_Outputs_1.RData")

#### Create Dummy Data 2 ####

#Set Amount of nodes
#Always use this vector of names to make sure all names are the same
nodenames = c(paste("Node", seq(1, 7), sep = ""))


perturbations_2 = c(1,-1, 1, 1) 
names(perturbations_2) = c(nodenames[1], nodenames[2], nodenames[3], nodenames[4])

measurements_2 = c(-1)
names(measurements_2) = c(nodenames[7])

priorKnowledgeNetwork_2 = matrix(c(nodenames[1], 1, nodenames[5], 
                                   nodenames[2], 1, nodenames[5],
                                   nodenames[2], -1, nodenames[6],
                                   nodenames[3], 1, nodenames[6],
                                   nodenames[3], -1, nodenames[5],
                                   nodenames[4], -1, nodenames[6],
                                   nodenames[6], 1, nodenames[7],
                                   nodenames[5], 1, nodenames[7]), byrow = T, ncol = 3) %>% as.data.frame()
colnames(priorKnowledgeNetwork_2) = c("source", "interaction", "target")

save(perturbations_2, file = "PT_2")
save(measurements_2, file = "MS_2")
save(priorKnowledgeNetwork_2, file = "PKN_2")

#### Create Expected Output 2 ####

constraint_1_2 <- c(createConstraint("eU1", "-", "n1", ">=", "0"),
                    createConstraint("eU2", "-", "n2", ">=", "0"),
                    createConstraint("eU3", "+", "n2", ">=", "0"),
                    createConstraint("eU4", "-", "n3", ">=", "0"),
                    createConstraint("eU5", "+", "n3", ">=", "0"),
                    createConstraint("eU6", "+", "n4", ">=", "0"),
                    createConstraint("eU7", "-", "n5", ">=", "0"),
                    createConstraint("eU8", "-", "n6", ">=", "0"))

constraint_2_2 <- c(createConstraint("eD1", "+", "n1", ">=", "0"),
                    createConstraint("eD2", "+", "n2", ">=", "0"),
                    createConstraint("eD3", "-", "n2", ">=", "0"),
                    createConstraint("eD4", "+", "n3", ">=", "0"),
                    createConstraint("eD5", "-", "n3", ">=", "0"),
                    createConstraint("eD6", "-", "n4", ">=", "0"),
                    createConstraint("eD7", "+", "n5", ">=", "0"),
                    createConstraint("eD8", "+", "n6", ">=", "0"))
output_2 = list(Constraint_1_2, Constraint_2_2)
save(Output_2, file = "Dummy_Outputs_2.RData")
