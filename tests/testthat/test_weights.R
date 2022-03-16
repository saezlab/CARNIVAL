library(dplyr)

# helper to generate network
inter <- function(x1,i,x2){
    data.frame(source = x1, interaction = i, target = x2)
}

# These helpers can be used to visualize PKN and results. 
# They are not essential part of testing but will help identifying the issues
# if we need manual checking. 
plotSolution <- function(result_actual,id,inputs, measurement){
    
    net_edge <- result_actual$sifAll[[id]] %>% as_tibble() %>%
        rename(from = Node1,to=Node2) %>%
        mutate(color = ifelse(Sign == 1, "black","red"))
    
    
    inp = names(inputs)
    measured = names(measurement)
    
    net_nodes <- result_actual$attributesAll[[id]] %>% as_tibble() %>%
        rename(id = Nodes) %>%
        mutate(label = id) %>%
        mutate(color.border = ifelse(id %in% inp, "black","white"),
               color.border = ifelse(id %in% measured,"red", color.border)) %>%
        mutate(color.background = ifelse(Activity == 1,"#9acd32", "grey"),
               color.background = ifelse(Activity == -1,"red", color.background)) %>%
        mutate(borderWidth = 3)
    
    visNetwork::visNetwork(edges = net_edge,nodes = net_nodes) %>%
        visNetwork::visEdges(arrows = 'to', scaling = list(min = 2, max = 2)) %>%
        visNetwork::visNodes(label=id,shape="ellipse")
    
}

plotPKN <- function(network, inputs, measurement){
    
    net_edge <- network %>% as_tibble() %>%
        rename(from = source,to=target) %>%
        mutate(color = ifelse(interaction == 1, "black","red"))
    
    nodes = unique(c(network$source, network$target))
    inp = names(inputs)
    measured = names(measurement)
    
    
    net_nodes <- tibble(id = nodes, label = nodes) %>%
        mutate(color.background = ifelse(id %in% inp, "#9acd32","white"),
               color.background = ifelse(id %in% measured,"lightblue", color.background))
    
    
    visNetwork::visNetwork(edges = net_edge,nodes = net_nodes) %>%
        visNetwork::visEdges(arrows = 'to', scaling = list(min = 2, max = 2)) %>%
        visNetwork::visNodes(label=id,shape="ellipse")
}


# Tests the weight with LP solve ------------------------------------------------------

# we create a small diamond shape network, going from one input to output on 
# 2 possible ways. With weights we make one path more favourable rather than the other. 


test_that("checking weight with diamond network", {
    # small chain model
    inputs = data.frame(I1 = 1)
    measurement = data.frame(M1 = 1)
    network = rbind(inter("I1", 1, "N1"),
                    inter("I1", 1, "N2"),
                    inter("N2", 1, "M1"),
                    inter("N1", 1, "M1"))
    weight_N1 = data.frame(N1 = 1)
    weight_N2 = data.frame(N2 = 1)
    weight_N1N2 = data.frame(N2 = -1, N1 = 1)
    
    
    
    # get solution with positive weight on N1
    result_N1fav_weight = runCARNIVAL(inputObj = inputs, 
                                   measObj = measurement, 
                                   netObj = network,
                                   weightObj = weight_N1,
                                   solver = "lpSolve",
                                   timelimit = 60,
                                   dir_name = "./test_model1/testweight2/",
                                   threads = 1,
                                   betaWeight = 0.1)
    
    # check that N1 is in the solution, but not N2: 
    expect_true("N1" %in%  result_N1fav_weight$attributesAll[[1]]$Nodes)
    expect_false("N2" %in%  result_N1fav_weight$attributesAll[[1]]$Nodes)
    
    
    
    # get solution with positive weight on N1
    result_N2fav_weight = runCARNIVAL(inputObj = inputs, 
                                      measObj = measurement, 
                                      netObj = network,
                                      weightObj = weight_N2,
                                      solver = "lpSolve",
                                      timelimit = 60,
                                      dir_name = "./test_model1/testweight3/",
                                      threads = 1,
                                      betaWeight = 0.1)
    
    # check that N2 is in the solution, but not N1: 
    expect_true("N2" %in%  result_N2fav_weight$attributesAll[[1]]$Nodes)
    expect_false("N1" %in%  result_N2fav_weight$attributesAll[[1]]$Nodes)
    
    
    # get solution with positive weight on N1 and negative weight on N2
    result_N1favN2_weight = runCARNIVAL(inputObj = inputs, 
                                      measObj = measurement, 
                                      netObj = network,
                                      weightObj = weight_N1N2,
                                      solver = "lpSolve",
                                      timelimit = 60,
                                      dir_name = "./test_model1/testweight4/",
                                      threads = 1,
                                      betaWeight = 0.1)
    
    # check that N1 is in the solution, but not N2: 
    expect_true("N1" %in%  result_N1favN2_weight$attributesAll[[1]]$Nodes)
    expect_false("N2" %in%  result_N1favN2_weight$attributesAll[[1]]$Nodes)
    
})

