
# helper to generate network
inter <- function(x1,i,x2){
    data.frame(source = x1, interaction = i, target = x2)
}


test_that("correctNodeIdentifiersInNetwork works", {
    priorKnowledgeNetwork = rbind(inter("I1", 1, "N1"),
                                  inter("I1", 1, "N2"),
                                  inter("N2", 1, "M1"),
                                  inter("N1", 1, "M1"))
    res <- correctNodeIdentifiersInNetwork(priorKnowledgeNetwork)
    
    expect_equal(priorKnowledgeNetwork,res)
    
})


test_that("correctNodeIdentifiersInNetwork reports error", {
    priorKnowledgeNetwork = rbind(inter("I1", 1, "*N*1"),
                                  inter("I*1", 1, "N+2"),
                                  inter("N2-", 1, "M1:"),
                                  inter("<N1>", 1, "M1*"))
    
    # Error should be produced if we have "M1:" and "M1*" in the network, as they
    # both result in "M1_"
    expect_error(correctNodeIdentifiersInNetwork(network = priorKnowledgeNetwork,verbose = TRUE))
    
})

test_that("correctNodeIdentifiersInNetwork works 2", {
    priorKnowledgeNetwork = rbind(inter("I1", 1, "*N*1"),
                                  inter("I*1", 1, "N+2"),
                                  inter("N2-", 1, "M1:"),
                                  inter("<N1>", 1, "M1:"))
    expected_priorKnowledgeNetwork = rbind(inter("I1", 1, "X_N_1"),
                                           inter("I_1", 1, "N_2"),
                                           inter("N2_", 1, "M1_"),
                                           inter("X_N1_", 1, "M1_"))
    
  
    res <- correctNodeIdentifiersInNetwork(network = priorKnowledgeNetwork,verbose = TRUE)
    
    expect_equal(expected_priorKnowledgeNetwork,res)
    
})



test_that("lpSolve, 2 input, 2 output, negative edges", {
    inputs = data.frame(I1 = 1, I2 = -1)
    measurement = data.frame(M1 = -1, M2 = -1)
    network = rbind(inter("I1", 1, "N*1"),
                    inter("I2", 1, "N*1"),
                    inter("N*1", 1, "N-2"),
                    inter("N-2", -1, "N 3"),
                    inter("N 3", 1, "M1"),
                    inter("N 3", 1, "M2"))
    
    # obtain actual reesult
    result_actual = runCARNIVAL(inputObj = inputs, 
                                measObj = measurement, 
                                netObj = network,
                                solver = "lpSolve",
                                timelimit = 60,
                                dir_name = "./test_model1/test5",
                                threads = 1,
                                betaWeight = 0)
    
    # plotPKN(network, inputs, measurement)
    # plotSolution(result_actual,1,inputs, measurement)
    
    # nodesAttributes
    expect_equal(nrow(result_actual$nodesAttributes), 7)
    
    attr = result_actual$nodesAttributes
    attr <- attr[match(c("I1","I2","M1","M2","N_1","N_2","N_3"),attr$Node),]
    
    expect_equal(attr$AvgAct, c(100,-100,-100,-100,100,100,-100))
    expect_equal(attr$DownAct, c(0,100,100,100,0,0,100))
    expect_equal(attr$UpAct, c(100,0,0,0,100,100,0))
    expect_equal(attr$ZeroAct, c(0,0,0,0,0,0,0))
    
    # weightedSIF
    expect_equal(nrow(result_actual$weightedSIF), 6)
    expect_equal(sum(result_actual$weightedSIF$Weight), 500)
    
    # sifAll
    expect_length(result_actual$sifAll,1)
    expect_true(nrow(result_actual$sifAll[[1]])==5)
    
    # attributesAll
    expect_length(result_actual$attributesAll,1)
    expect_equal(sum(result_actual$attributesAll[[1]]$Activity),-1)
    expect_equal(sum(abs(result_actual$attributesAll[[1]]$Activity)),7)
    
})
