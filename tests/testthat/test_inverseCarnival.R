
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



## Tests with LP solve ------------------------------------------------------


test_that("lpSolve, inverseCarnival, negative measurement", {
    
    
    # small chain model
    
    measurement = c(M1 = -1)
    network = rbind(inter("I1", 1, "N1"),
                    inter("I1", 1, "N2"),
                    inter("N2", 1, "M1"),
                    inter("N1", 1, "M1"))
    
    options <- defaultLpSolveCarnivalOptions()
    options$workdir = file.path(options$workdir,"work_lpsolve")
    options$outputFolder = file.path(options$workdir,"work_lpsolve")
    options$keepLPFiles = FALSE
    
    # obtain actual result using LP solve
    result_actual = runInverseCarnival(measurements = measurement,
                                       priorKnowledgeNetwork = network,
                                       weights = NULL,
                                       carnivalOptions = options)
    
    # plotSolution(result_actual,1,inputs = c(Perturbation = 1), measurement)
    
    attr = result_actual$nodesAttributes
    attr <- attr[match(c("Perturbation","I1","M1","N1","N2"),attr$Node),]
    
    expect_equal(attr$AvgAct, c(100,-100,-100,-100,0))
    expect_equal(attr$DownAct, c(0,100,100,100,0))
    expect_equal(attr$UpAct, c(100,0,0,0,0))
    expect_equal(attr$ZeroAct, c(0,0,0,0,100))
    
    # weightedSIF
    expect_equal(nrow(result_actual$weightedSIF), 6)
    expect_equal(result_actual$weightedSIF$Weight, c(100,0,100,0,100,0))
    
    # sifAll
    expect_length(result_actual$sifAll,1)
    
    # attributesAll
    expect_length(result_actual$attributesAll,1)
    expect_equal(result_actual$attributesAll[[1]]$Activity,c(-1,-1,1,-1))
    
    
})


## Tests with CPLEX solve ------------------------------------------------------

# find CPLEX folder in Applications folder. Folder name changes between versions.
cplexFolder = dir(path = "/Applications",pattern = "CPLEX_Studio",full.names = TRUE)
if(length(cplexFolder)==0){
    cplexPath = ""
}else{
    cplex_rel_path = "cplex/bin/x86-64_osx/cplex"
    cplexPath = file.path(cplexFolder,cplex_rel_path)
}

test_that("CPLEX, inverseCarnival, negative measurement", {
    
    # There are 4 possible solutions
    
    skip_if_not(file.exists(cplexPath))
    
    # small chain model
    
    measurement = c(M1 = -1)
    network = rbind(inter("I1", 1, "N1"),
                    inter("I1", 1, "N2"),
                    inter("N2", 1, "M1"),
                    inter("N1", 1, "M1"))
    
    options <-  defaultCplexCarnivalOptions()
    options$solverPath <- cplexPath
    options$workdir = file.path(options$workdir,"work_cplex")
    options$outputFolder = file.path(options$workdir,"work_cplex")
    options$keepLPFiles <- FALSE

    # obtain actual result using LP solve
    result_actual = runInverseCarnival(measurements = measurement,
                                       priorKnowledgeNetwork = network,
                                       weights = NULL,
                                       carnivalOptions = options)
    
    # plotSolution(result_actual,1,inputs = c(Perturbation = 1), measurement)
    
    attr = result_actual$nodesAttributes
    attr <- attr[match(c("Perturbation","I1","M1","N1","N2"),attr$Node),]
    
    expect_equal(attr$AvgAct, c(0,-100,-100,-50,-50))
    expect_equal(attr$DownAct, c(50,100,100,50,50))
    expect_equal(attr$UpAct, c(50,0,0,0,0))
    expect_equal(attr$ZeroAct, c(0,0,0,50,50))
    
    # weightedSIF
    expect_equal(nrow(result_actual$weightedSIF), 6)
    expect_equal(result_actual$weightedSIF$Weight, c(50,50,50,50,50,50))
    
    # sifAll
    expect_length(result_actual$sifAll,4)
    
    # attributesAll
    expect_length(result_actual$attributesAll,4)
    
})

