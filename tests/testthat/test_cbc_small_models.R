
## These small networks should work with all solvers.

library(dplyr)

# TODO: test gurobi
cbcPath = "~/Documents/SaezGroup/LocalGitRepo/cbc_optimizers/cbc-osx/cbc"  

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







# Tests with CBC solve ------------------------------------------------------

### Model -1 empty network solution  ---------------------


test_that("CBC, empty solution", {
    
    skip_if_not(file.exists(cbcPath))
    # small chain model
    inputs = data.frame(I1 = 1)
    measurement = data.frame(M1 = -1)
    network = rbind(inter("I1", 1, "N1"),
                    inter("I1", 1, "N2"),
                    inter("N2", 1, "M1"),
                    inter("N1", 1, "M1"))
    
    # obtain actual result using LP solve
    result_actual = runCARNIVAL(inputObj = inputs, 
                                measObj = measurement, 
                                netObj = network,
                                solver = "cbc",
                                solverPath = cbcPath,
                                timelimit = 60,
                                dir_name = "./test_model1/cbc1",
                                threads = 1,
                                betaWeight = 0.1)
    
    
    
    
    attr = result_actual$nodesAttributes
    attr <- attr[match(attr$Node,c("I1","M1","N1","N2")),]
    
    expect_equal(attr$AvgAct, c(100,0,0,0))
    expect_equal(attr$DownAct, c(0,0,0,0))
    expect_equal(attr$UpAct, c(100,0,0,0))
    expect_equal(attr$ZeroAct, c(0,100,100,100))
    
    # weightedSIF
    expect_true(nrow(result_actual$weightedSIF)== 4)
    expect_equal(result_actual$weightedSIF$Weight, c(0,0,0,0))
    
    # sifAll
    expect_length(result_actual$sifAll,1)
    
    # attributesAll
    expect_length(result_actual$attributesAll,1)
    expect_true(result_actual$attributesAll[[1]]$Activity==1)
    
    
})


### Model 0: minimalist chain model with 2 solutions ---------------------------
# Only activatory edges

# Quantitative checks related to the case study 0 with CBC (1 solution expected)

test_that("CBC, diamond shape, positive edges", {
    
    skip_if_not(file.exists(cbcPath))
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
                                solver = "cbc",
                                solverPath = cbcPath,
                                timelimit = 60,
                                dir_name = "./test_model1/cbc2",
                                threads = 1,
                                betaWeight = 0.1)
    
    # cplex should find 2 solutions with beta weight > 0
    
    attr = result_actual$nodesAttributes
    attr <- attr[match(attr$Node,c("I1","M1","N1","N2")),]
    
    expect_equal(sum(attr$AvgAct), 300)
    expect_equal(sum(attr$DownAct), 0)
    expect_equal(sum(attr$UpAct), 300)
    expect_equal(sum(attr$ZeroAct), 100)
    
    # weightedSIF
    expect_true(nrow(result_actual$weightedSIF)== 4)
    expect_true(sum(result_actual$weightedSIF$Weight)==200)
    
    
    # sifAll
    expect_length(result_actual$sifAll,1)
    expect_true(nrow(result_actual$sifAll[[1]])==2)
    
    
    # attributesAll
    expect_length(result_actual$attributesAll,1)
    expect_true(sum(result_actual$attributesAll[[1]]$Activity)==3)
    
})


### Model 1: minimalist chain model with 1 solutions ---------------------------
# Only activatory edges
# Note: CBC solve will find only 1. 


test_that("CBC, 2 input, 2 output, negative edges", {
    
    skip_if_not(file.exists(cbcPath))
    
    inputs = data.frame(I1 = 1, I2 = -1)
    measurement = data.frame(M1 = -1, M2 = -1)
    network = rbind(inter("I1", 1, "N1"),
                    inter("I2", 1, "N1"),
                    inter("N1", 1, "N2"),
                    inter("N2", -1, "N3"),
                    inter("N3", 1, "M1"),
                    inter("N3", 1, "M2"))
    
    # obtain actual reesult
    
    result_actual = runCARNIVAL(inputObj = inputs, 
                                measObj = measurement, 
                                netObj = network,
                                solver = "cbc",
                                solverPath = cbcPath,
                                timelimit = 60,
                                dir_name = "./test_model1/cbc3",
                                threads = 1,
                                betaWeight = 0.1)
    
    # plotPKN(network, inputs, measurement)
    # plotSolution(result_actual,1,inputs, measurement)
    
    # nodesAttributes
    expect_true(nrow(result_actual$nodesAttributes)== 7)
    
    attr = result_actual$nodesAttributes
    attr <- attr[match(attr$Node,c("I1","I2","M1","M2","N1","N2","N3")),]
    
    expect_equal(attr$AvgAct, c(100,-100,-100,-100,100,100,-100))
    expect_equal(attr$DownAct, c(0,100,100,100,0,0,100))
    expect_equal(attr$UpAct, c(100,0,0,0,100,100,0))
    expect_equal(attr$ZeroAct, c(0,0,0,0,0,0,0))
    
    # weightedSIF
    expect_true(nrow(result_actual$weightedSIF)== 6)
    expect_true(sum(result_actual$weightedSIF$Weight)==500)
    
    # sifAll
    expect_length(result_actual$sifAll,1)
    expect_true(nrow(result_actual$sifAll[[1]])==5)
    
    # attributesAll
    expect_length(result_actual$attributesAll,1)
    expect_true(sum(result_actual$attributesAll[[1]]$Activity)==-1)
    expect_true(sum(abs(result_actual$attributesAll[[1]]$Activity))==7)
    
})



##### Larger random model --------------------------------------------------


generate_case_study <- function(N_nodes, N_measured, N_inputs, p_negative){
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
        mutate(interaction = ifelse(runif(n())>p_negative,1,-1)) %>%
        mutate(source = make.names(from),
               target = make.names(to)) %>%
        dplyr::select(source, interaction,target)
    
    nodes = unique(c(sif$source,sif$target))
    
    inputs = as.data.frame(t(rep(1,N_inputs)))
    names(inputs) = sample(nodes,N_inputs)
    
    measurement = as.data.frame(t(rep(1,N_measured)))
    names(measurement) = sample(setdiff(nodes,names(inputs)),N_measured)
    
    return(list(sif = sif,
                inputs = inputs,
                measurement = measurement))
}



test_that("check larger model with CBC solver", {
    
    skip_if_not(file.exists(cbcPath))
    
    N_nodes = 50 # Not the same as in CPLEX!
    N_measured = 10
    N_inputs = 2
    set.seed(123)
    case_study <- generate_case_study(N_nodes,N_measured,N_inputs,p_negative=0.1)
    
    # obtain actual reesult
    
    result_actual = runCARNIVAL(inputObj = case_study$inputs, 
                                measObj = case_study$measurement, 
                                netObj = case_study$sif,
                                solver = "cbc",
                                solverPath = cbcPath,
                                timelimit = 60,
                                dir_name = "./test_model1/cbc4",
                                threads = 1,
                                betaWeight = 0.1)
    
    
    
    # nodesAttributes
    expect_equal(nrow(result_actual$nodesAttributes), 50)
    
    
    expect_equal(sum(result_actual$nodesAttributes$AvgAct), 1600)
    expect_equal(sum(result_actual$nodesAttributes$DownAct), 0)
    expect_equal(sum(result_actual$nodesAttributes$UpAct), 1600)
    expect_equal(sum(result_actual$nodesAttributes$ZeroAct), 3400)
    
    # weightedSIF
    expect_equal(nrow(result_actual$weightedSIF), 114)
    expect_equal(sum(result_actual$weightedSIF$Weight), 1800)
    
    # sifAll
    expect_length(result_actual$sifAll,1)
    expect_equal(nrow(result_actual$sifAll[[1]]),18)
    
    # attributesAll
    expect_length(result_actual$attributesAll,1)
    expect_equal(sum(result_actual$attributesAll[[1]]$Activity), 16)
    expect_equal(sum(abs(result_actual$attributesAll[[1]]$Activity)),16)
    
})

