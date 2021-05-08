testEnvironement <- new.env()

#Loading data
load(file = system.file("toy_perturbations_ex1.RData", package = "CARNIVAL"),
     testEnvironement)
load(file = system.file("toy_network_ex1.RData", package = "CARNIVAL"),
     testEnvironement)
load(file = system.file("toy_measurements_ex1.RData", package = "CARNIVAL"),
     testEnvironement)

#Loading expected results
load(file = system.file("toy_result_expected_ex1.RData", package = "CARNIVAL"),
     testEnvironement)

perturbations <- testEnvironement$toy_perturbations_ex1
priorKnowledgeNetwork <- testEnvironement$toy_network_ex1
measurements <- testEnvironement$toy_measurements_ex1

# Setting options 
carnivalLpOptions <- defaultLpSolveCarnivalOptions()
cplexSolverPath <- "/Applications/CPLEX_Studio1210/cplex/bin/x86-64_osx/cplex"
carnivalCplexOptions <- defaultCplexCarnivalOptions(solverPath = cplexSolverPath)
carnivalCbcOptions <- defaultCbcSolveCarnivalOptions()

carnivalCplexOptions$outputFolder <- "/Users/olgaivanova/GoogleDrive/_PhD_Heidelberg/playground/carnival_style/carnival/tests"

#Testing API with lpSolve
generateLpFileCarnival(perturbations, measurements, 
                       priorKnowledgeNetwork,
                       carnivalOptions = carnivalCplexOptions)
#runFromLpCarnival()
inverseCarnival <- runInverseCarnival(measurements, priorKnowledgeNetwork,
                   carnivalOptions = carnivalCplexOptions)

isInputValidCarnival(perturbations, measurements, 
                     priorKnowledgeNetwork, 
                     carnivalOptions = carnivalLpOptions)

vanillaCarnivalOutput <- runVanillaCarnival(perturbations, measurements, 
                                            priorKnowledgeNetwork,
                                            carnivalOptions = carnivalLpOptions) 

vanillaCarnivalWeightedOutput <- runVanillaCarnival(perturbations, measurements, 
                                                    priorKnowledgeNetwork, 
                                                    weights, 
                                                    carnivalOptions = carnivalLpOptions)

testthat::expect_equal()

#cplex
vanillaCarnivalCplexOutput <- runVanillaCarnival(perturbations, measurements, 
                                                 priorKnowledgeNetwork,
                                                 carnivalOptions = carnivalCplexOptions) 

#cbc 
vanillaCarnivalCplexOutput <- runVanillaCarnival(perturbations, measurements, 
                                                 priorKnowledgeNetwork,
                                                 carnivalCbcOptions) 
