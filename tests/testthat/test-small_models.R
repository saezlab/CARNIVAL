
## These small networks should work with all solvers.



cplexPath = "/Applications/CPLEX_Studio128/cplex/bin/x86-64_osx/cplex"
cbcPath = ""
gurobiPath = ""
lpsolvePath = "not needed"


# helper to generate network
inter <- function(x1,i,x2){
    data.frame(source = x1, interaction = i, target = x2)
}

# These helpers can be used to visualize PKN and results. 
# They are not essential part of testing but will help identifying the issues
# if we need manual checking. 
plotSolution <- function(result_actual,id,inputs, measurements){
    
    net_edge <- result_actual$sifAll[[id]] %>% as_tibble() %>%
        rename(from = Node1,to=Node2)
    
    inp = names(inputs)
    measured = names(measurements)
    
    net_nodes <- result_actual$nodesAttributes %>% as_tibble() %>%
        rename(id = Node) %>%
        mutate(label = id) %>%
        mutate(color.background = ifelse(id %in% inp, "#9acd32","white"),
               color.background = ifelse(id %in% measured,"lightblue", color.background))
    
    
    visNetwork::visNetwork(edges = net_edge,nodes = net_nodes) %>%
        visEdges(arrows = 'to', scaling = list(min = 2, max = 2)) %>%
        visNodes(label=id,shape="ellipse")
    
}

plotPKN <- function(network, inputs, measurements){
    net_edge <- network %>% as_tibble() %>%
        rename(from = source,to=target)
    
    nodes = unique(c(network$source, network$target))
    inp = names(inputs)
    measured = names(measurements)
    
    net_nodes <- tibble(id = nodes, label = nodes) %>%
        mutate(color = ifelse(id %in% inp, list(border = "black", background = "olivedrab3"),
                              list(border = "black", background = "white")),
               color = ifelse(id %in% measured,list(border = "black", background = "lightblue"), color))
    
    
    net_nodes <- tibble(id = nodes, label = nodes) %>%
        mutate(color.background = ifelse(id %in% inp, "#9acd32","white"),
               color.background = ifelse(id %in% measured,"lightblue", color.background))
    
    
    visNetwork::visNetwork(edges = net_edge,nodes = net_nodes) %>%
        visEdges(arrows = 'to', scaling = list(min = 2, max = 2)) %>%
        visNodes(label=id,shape="ellipse")
}



# Tests wit LP solve

### Model 0: minimalist chain model with 3 solutions ---------------------------
# Only activatory edges
# Note: LP solve will find only 1. 

# small chain model
inputs = data.frame(I1 = 1)
measurement = data.frame(M1 = 1)
network = rbind(inter("I1", 1, "N1"),
                inter("I1", 1, "N2"),
                inter("N2", 1, "M1"),
                inter("N1", 1, "M1"))


# obtain actual result using LP solve
result_actual = runCARNIVAL(inputObj = inputs, 
                            measObj = measurement, 
                            netObj = network,
                            solver = "lpSolve",
                            timelimit = 60,
                            dir_name = "./test_model1",
                            threads = 1,
                            betaWeight = 0)

# Formal checking the output of runCARNIVAL

test_that("formal checks on runCARNIAL results", {
    
    expect_length(result_actual, 4)
    expect_named(result_actual,expected =  c("weightedSIF", 
                                             "nodesAttributes",
                                             "sifAll",
                                             "attributesAll"))
    expect_type(result_actual,"list")
    
    expect_named( result_actual$weightedSIF,expected = c( "Node1","Sign",
                                                          "Node2", "Weight") )
    expect_named( result_actual$nodesAttributes,expected = c( "Node", 
                                                              "NodeType",
                                                              "ZeroAct","UpAct",
                                                              "DownAct","AvgAct"))
    expect_type( result_actual$sifAll,"list")
    expect_type( result_actual$attributesAll,"list")
})


# Quantitative checks related to the case study 0 with LPsolve (1 solution expected)

test_that("check case study 0 for LP solve", {
    
    # nodesAttributes
    expect_true(nrow(result_actual$nodesAttributes)== 4)
    expect_true(sum(result_actual$nodesAttributes$AvgAct)==300)
    expect_true(sum(result_actual$nodesAttributes$DownAct)==0)
    expect_true(sum(result_actual$nodesAttributes$UpAct)==300)
    expect_true(sum(result_actual$nodesAttributes$ZeroAct)==100)
    
    # weightedSIF
    expect_true(nrow(result_actual$weightedSIF)== 4)
    expect_true(sum(result_actual$weightedSIF$Weight)==300)
    
    # sifAll
    expect_length(result_actual$sifAll,1)
    expect_true(nrow(result_actual$sifAll[[1]])==2)
    
    # attributesAll
    expect_length(result_actual$attributesAll,1)
    expect_true(sum(result_actual$attributesAll[[1]]$Activity)==3)
    
})


# plotPKN(network, inputs, measurement)
# plotSolution(result_actual,1,inputs, measurement)



### Model 0: minimalist chain model with 3 solutions ---------------------------
# With inhibitory edge
# Note: LP solve will find only 1. 

# small chain model
inputs = data.frame(I1 = 1)
measurement = data.frame(M1 = -1)
network = rbind(inter("I1", -1, "M1"))


# obtain actual result using LP solve
result_actual = runCARNIVAL(inputObj = inputs, 
                            measObj = measurement, 
                            netObj = network,
                            solver = "lpSolve",
                            timelimit = 60,
                            dir_name = "./test_model1",
                            threads = 1,
                            betaWeight = 0)


# small chain model
inputs = data.frame(I1 = 1)
measurement = data.frame(M1 = -1)
network = rbind(inter("I1", -1, "N1"),
                inter("I1", 1, "N2"),
                inter("N2", 1, "M1"),
                inter("N1", 1, "M1"))


# obtain actual result using LP solve
result_actual = runCARNIVAL(inputObj = inputs, 
                            measObj = measurement, 
                            netObj = network,
                            solver = "lpSolve",
                            timelimit = 60,
                            dir_name = "./test_model1",
                            threads = 1,
                            betaWeight = 0.1)

# Formal checking the output of runCARNIVAL

test_that("formal checks on runCARNIAL results", {
    
    expect_length(result_actual, 4)
    expect_named(result_actual,expected =  c("weightedSIF", 
                                             "nodesAttributes",
                                             "sifAll",
                                             "attributesAll"))
    expect_type(result_actual,"list")
    
    expect_named( result_actual$weightedSIF,expected = c( "Node1","Sign",
                                                          "Node2", "Weight") )
    expect_named( result_actual$nodesAttributes,expected = c( "Node", 
                                                              "NodeType",
                                                              "ZeroAct","UpAct",
                                                              "DownAct","AvgAct"))
    expect_type( result_actual$sifAll,"list")
    expect_type( result_actual$attributesAll,"list")
})


# Quantitative checks related to the case study 0 with LPsolve (1 solution expected)

test_that("check case study 0 for LP solve", {
    
    # nodesAttributes
    expect_true(nrow(result_actual$nodesAttributes)== 4)
    expect_true(sum(result_actual$nodesAttributes$AvgAct)==300)
    expect_true(sum(result_actual$nodesAttributes$DownAct)==0)
    expect_true(sum(result_actual$nodesAttributes$UpAct)==300)
    expect_true(sum(result_actual$nodesAttributes$ZeroAct)==100)
    
    # weightedSIF
    expect_true(nrow(result_actual$weightedSIF)== 4)
    expect_true(sum(result_actual$weightedSIF$Weight)==300)
    
    # sifAll
    expect_length(result_actual$sifAll,1)
    expect_true(nrow(result_actual$sifAll[[1]])==2)
    
    # attributesAll
    expect_length(result_actual$attributesAll,1)
    expect_true(sum(result_actual$attributesAll[[1]]$Activity)==3)
    
})


# plotPKN(network, inputs, measurement)
# plotSolution(result_actual,1,inputs, measurement)






### Model 1: chain model ------------------------------------
# small chain model
inputs = data.frame(I1 = 1)
measurement = data.frame(M1 = 1)
network = rbind(inter("I1", 1, "N1"),
                inter("N1", 1, "N2"),
                inter("N2", 1, "N3"),
                inter("N3", 1, "M1"))
colnames(network) <- c("source", "interaction",'target')



# obtain actual reesult
result_actual = runCARNIVAL(inputObj = inputs, 
                            measObj = measurement, 
                            netObj = network,
                            solver = "cplex",
                            solverPath = cplexPath,
                            timelimit = 60,
                            dir_name = "./test_model1",
                            threads = 1,
                            betaWeight = 0)

plotPKN(network, inputs, measurement)
plotSolution(result_actual,1,inputs, measurement)




### Model 2: 2 inputs - 2 outputs ------------------------------------
# small chain 
inputs = data.frame(I1 = 1, I2 = 1)
measurement = data.frame(M1 = 1,M2=1)
network = rbind(inter("I1", 1, "N1"),
                inter("I2", 1, "N1"),
                inter("N1", 1, "N2"),
                inter("N2", 1, "N3"),
                inter("N3", 1, "M1"),
                inter("N3", 1, "M2"))

# obtain actual reesult
result_actual = runCARNIVAL(inputObj = inputs, 
                            measObj = measurement, 
                            netObj = network,
                            solver = "cplex",
                            solverPath = cplexPath,
                            timelimit = 60,
                            dir_name = "./test_model1",
                            threads = 1,
                            betaWeight = 0)


plotPKN(network, inputs, measurement)
plotSolution(result_actual,1,inputs, measurement)



### Model 3: small chain with fork ------------------------------------
# small model with parallel section


inputs = data.frame(I1 = 1)
measurement = data.frame(M1 = 1)
network = rbind(inter("I1", 1, "N1"),
                inter("N1", 1, "N2"),
                inter("N1", 1, "N3"),
                inter("N3", 1, "M1"),
                inter("N2", 1, "M1"))

# obtain actual reesult
result_actual = runCARNIVAL(inputObj = inputs, 
                            measObj = measurement, 
                            netObj = network,
                            solver = "cplex",
                            solverPath = cplexPath,
                            timelimit = 60,
                            dir_name = "./test_model1",
                            threads = 1,
                            betaWeight = 0.01)

plotPKN(network, inputs, measurement)
plotSolution(result_actual,1,inputs, measurement)
plotSolution(result_actual,2,inputs, measurement)



##### Larger random model --------------------------------------------------


generate_case_study <- function(N_nodes, N_measured, N_inputs){
    # erdos renyi graph: edges are uniformly distributed
    g = igraph::erdos.renyi.game(n = N_nodes,p.or.m = N_nodes*runif(1,min = 2,max = 3),type = "gnm",directed = TRUE)
    
    # scale free network
    #g = igraph::barabasi.game(n = N_nodes,power = 0.7,m = 2)
    
    # find the largest component
    clu <- igraph::components(g, mode = c("weak"))
    largest_component <- clu$membership %>% table() %>% which.max()
    g <- igraph::induced_subgraph(g,clu$membership==largest_component)
    
    # add sign with .1 probabilty
    sif = igraph::as_data_frame(g,what = "edges") %>% as_tibble() %>%
        mutate(interaction = ifelse(runif(n())>0.1,1,-1)) %>%
        mutate(source = make.names(from),
               target = make.names(to)) %>%
        dplyr::select(source, interaction,target)
    
    nodes = unique(c(sif$source,sif$target))
    
    inputs = as.data.frame(t(rep(1,N_input)))
    names(inputs) = sample(nodes,N_input)
    
    measurement = as.data.frame(t(rep(1,N_measured)))
    names(measurement) = sample(setdiff(nodes,names(inputs)),N_measured)
    
    return(list(sif = sif,
                inputs = inputs,
                measurement = measurement))
}

N_nodes = 100
N_measured = 10
N_input = 20
set.seed(123)
case_study <- generate_case_study(N_nodes,N_measured,N_inputs)

# obtain actual reesult
result_actual = runCARNIVAL(inputObj = case_study$inputs, 
                            measObj = case_study$measurement, 
                            netObj = case_study$sif,
                            solver = "cplex",
                            solverPath = cplexPath,
                            timelimit = 60,
                            dir_name = "./test_model1",
                            threads = 1,
                            betaWeight = 0)

plotPKN(network = case_study$sif,inputs =  case_study$inputs,measurements =  case_study$measurement)
plotSolution(result_actual,1,case_study$inputs, case_study$measurement)
plotSolution(result_actual,2,case_study$inputs, case_study$measurement)

result_actual = runCARNIVAL(inputObj = case_study$inputs, 
                            measObj = case_study$measurement, 
                            netObj = case_study$sif,
                            solver = "cbc",
                            timelimit = 60,
                            dir_name = "./test_model1",
                            threads = 1,
                            betaWeight = 0)

carnivalOptions = defaultCbcCarnivalOptions()

result_actual = runVanillaCarnival(perturbations = unlist(case_study$inputs), 
                                   measurements = unlist(case_study$measurement), 
                                   priorKnowledgeNetwork = case_study$sif,
                                   carnivalOptions = carnivalOptions)

