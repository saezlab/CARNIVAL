## Extract and export the optimisation results from the solution matrix.
##
## Enio Gjerga, Olga Ivanova 2020-2021
## fixed and formatted to tidy by A. Gabor
#' @importFrom  dplyr group_by bind_rows mutate summarise rename group_split add_column pivot_longer left_join
#' @importFrom  tibble group_by add_column
#' @importFrom  tidyr pivot_longer

exportIlpSolutionFromSolutionMatrix <- function(solutionMatrix_chr, variables) {
  
  solutionMatrix <- apply(solutionMatrix_chr,2,as.numeric)
  rownames(solutionMatrix) <- rownames(solutionMatrix_chr)
  colnames(solutionMatrix) <- paste("soluton", 1:ncol(solutionMatrix_chr),sep = "_")
  
  solutionTable <- solutionMatrix %>% 
    dplyr::as_tibble() %>% 
    tibble::add_column(opt_variable = rownames(solutionMatrix), .before=1) %>%
    tidyr::pivot_longer(cols = -1, names_to = "solution", values_to = "value")
  
  # get the values of the edge variables from the solution table
  edgeSolutions = variables$edgesDf %>% 
    dplyr::left_join(solutionTable, by = c("edgesUpVars" = "opt_variable")) %>%
    dplyr::rename(edgesUpValue = "value") %>%
    dplyr::left_join(solutionTable, by = c("edgesDownVars" = "opt_variable",solution = "solution")) %>%
    dplyr::rename(edgesDownValue = "value")
  
  # get the values of the node variables from the solution table
  nodeSolutions = variables$nodesDf %>% 
    dplyr::left_join(solutionTable, by = c("nodesVars" = "opt_variable")) %>%
    dplyr::rename(nodesValue = "value") %>%
    dplyr::left_join(solutionTable, by = c("nodesUpVars" = "opt_variable",solution = "solution")) %>%
    dplyr::rename(nodesUpValue = "value") %>%
    dplyr::left_join(solutionTable, by = c("nodesDownVars" = "opt_variable",solution = "solution")) %>%
    dplyr::rename(nodesDownValue = "value") %>%
    dplyr::left_join(solutionTable, by = c("nodesActStateVars" = "opt_variable",solution = "solution")) %>%
    dplyr::rename(nodesActStateValue = "value") %>%
    dplyr::left_join(solutionTable, by = c("nodesDistanceVars" = "opt_variable",solution = "solution")) %>%
    dplyr::rename(nodesDistanceValue = "value") 
  
  
  # Process inputs
  # edges:  A -e-> B
  # due to the ILP formalism, the edge (e) always takes a non-zero value if the
  # upstream node (A) has a non-zero value, i.e. the edge cannot be 0 if A is 1 or -1.  
  # The decision, if this edge is activating/inhibiting the downstream node B is decided on the 
  # level of downstream node B. Even though the edge "e" is 1, B can be zero. 
  # However, this is counter-intuitive, and we probably should not report an 
  # edge in the solution if the downstream node is not activated by it.
  # To implement this, we check the downstream node value and if the value is 
  # according to the edge then we report the edge, otherwise we remove it. 
  processed_edgeSolutions <- edgeSolutions %>% 
    #dplyr::mutate(presents = as.numeric(edgesUpValue | edgesDownValue)) %>%
    dplyr::left_join(select(nodeSolutions,nodes,nodesValue,solution), by = c("Node2"="nodes","solution"="solution")) %>%
    dplyr::rename(Node2Value = "nodesValue") %>%
    dplyr::mutate(presents = ifelse(Node2Value == 1 & edgesUpValue == 1, 1, 0)) %>%
    dplyr::mutate(presents = ifelse(Node2Value == -1 & edgesDownValue == 1, 1, presents))
  
  pocessed_nodeSolution <- nodeSolutions %>%
    dplyr::mutate(Activity = nodesValue)
    
  # Individual solutions as list:
    
  sifAll <- processed_edgeSolutions %>%
    dplyr::filter(presents>0) %>%
    dplyr::select(Node1,Sign,Node2,solution) %>%
    dplyr::group_by(solution) %>% 
    dplyr::group_split(.keep = FALSE) # this is experimental in dplyr
    
  
  nodeAttributesAll <- pocessed_nodeSolution %>%
    dplyr::select(nodes,Activity,solution) %>% 
    dplyr::filter(Activity != 0) %>%
    dplyr::group_by(solution) %>% 
    dplyr::group_split(.keep = FALSE) # this is experimental in dplyr
    
  
  
  # Aggregated solutions: 
  weightedNodes <- pocessed_nodeSolution %>%
    dplyr::mutate(activityUp = as.numeric(Activity > 0),
                  activityDown = as.numeric(Activity < 0),
                  zeroActivity = as.numeric(Activity == 0)) %>%
    dplyr::group_by(nodes,nodesType) %>%
    dplyr::summarise(ZeroAct = sum(zeroActivity)/dplyr::n()*100,
                     UpAct = sum(activityUp)/dplyr::n()*100,
                     DownAct = sum(activityDown)/dplyr::n()*100,
                     AvgAct = UpAct-DownAct,.groups ="drop" ) %>%
    dplyr::rename(Node = "nodes",
                  NodeType = "nodesType")
  
  
  weightedSIF <- edgeSolutions %>% 
    dplyr::mutate(presents = as.numeric(edgesUpValue | edgesDownValue)) %>%
    dplyr::group_by(Node1,Sign,Node2) %>%
    dplyr::summarise(Weight = sum(presents)/dplyr::n()*100,.groups ="drop")
  
    
  
  result <- list("weightedSIF" = weightedSIF, 
                 "nodesAttributes" = weightedNodes,
                 "sifAll" = sifAll, 
                 "attributesAll" = nodeAttributesAll) 
  
  return(result)
}

