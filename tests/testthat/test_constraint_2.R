## This function runs a Unit test on the function createConstraints_1_2_v2
## 
## Matteo Spatuzzi 2021


#### Run Function ####

# 1
Input1 = prerunCarnival(perturbations = perturbations_1, 
                       measurements = measurements_1,
                       priorKnowledgeNetwork = priorKnowledgeNetwork_1,
                       newDataRepresentation = T,
                       solver = supportedSolvers$cplex, 
                       solverPath = solverPath,
                       carnivalOptions = carnivalOptions)

print(Input1)

#Create Constraints
Constraints_3_1 = createConstraints_3_v2(Input_1)

# 2
Input_2 = prerunCarnival(perturbations = perturbations_2, 
                       measurements = measurements_2,
                       priorKnowledgeNetwork = priorKnowledgeNetwork_2,
                       newDataRepresentation = T,
                       solver = supportedSolvers$cplex, 
                       solverPath = solverPath,
                       carnivalOptions = carnivalOptions)

# Create Constraints
Constraints_3_2 = createConstraints_3_v2(Input_2)


#### Compare Outputs ####

Diff = length(c(setdiff(Constraint_3, Constraints_3[[1]]),
                setdiff(Constraints_3[[1]], Constraint_3)))

if(Diff == 0){
  
  Test_Passed = TRUE
  print("The test was passed successfully.")
}else{
  
  Test_Passed = FALSE
  print("Error: The output do not match the expected results!")
  
}
