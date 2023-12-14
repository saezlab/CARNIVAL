#' carnival_visNet
#'
#' Visualization of CARNIVAL results using visNetwork
#'
#' @param evis dataframe with summary edge information from CARNIVAL
#' @param nvis dataframe with summary node information from CARNIVAL
#' @param mapIDs dataframe with values of hgnc_symbol and uniprot_id to add hgnc as labels of the nodes
#' @return visNetwork object with the visualization of the network
#' 
#' @import dplyr
#' @import visNetwork
#'
#' @export
#'
#' @examples 
#' # Sample network data
#' network_data <- data.frame(
#'   Node1 = c("A", "A", "B", "B", "C", "D", "E"),
#'   Sign = c(1, -1, 1, -1, 1, 1, -1),
#'   Node2 = c("B", "C", "C", "D", "E", "F", "F"),
#'   Weight = rep(100,7)
#' )
#' 
#' node_attribute <- data.frame(
#'   Node = c("A","B","C","D","E","F"),
#'   NodeType = c("P","","","","M","M"),
#'   ZeroAct = rep(0,6),
#'   UpAct = c(100,100,0,0,100,0),
#'   DownAct = c(0,0,100,100,0,100),
#'   AvgAct = c(100,100,-100,-100,100,-100)
#' )
#' carnival_visNet(network_data, node_attribute)
carnival_visNet <- function(evis, nvis, mapIDs=NULL){
  
  writeLines('Graphical representation of sample')
  
  # color node scale red to blue
  
  rb_scale = c("#F20404", "#EE0507", "#EA070B", "#E6090F", "#E20B12", "#DF0D16", "#DB0F1A",
               "#D7101E", "#D31221", "#D01425", "#CC1629", "#C8182D", "#C41A30", "#C11B34",
               "#BD1D38", "#B91F3C", "#B5213F", "#B22343", "#AE2547", "#AA264B", "#A6284E",
               "#A32A52", "#9F2C56", "#9B2E5A", "#97305D", "#933261", "#903365", "#8C3569",
               "#88376C", "#843970", "#813B74", "#7D3D78", "#793E7B", "#75407F", "#724283",
               "#6E4487", "#6A468A", "#66488E", "#634992", "#5F4B96", "#5B4D99", "#574F9D",
               "#5451A1", "#5053A5", "#4C55A8", "#4856AC", "#4458B0", "#415AB4", "#3D5CB7",
               "#395EBB", "#3560BF", "#3261C3", "#2E63C6", "#2A65CA", "#2667CE", "#2369D2",
               "#1F6BD5", "#1B6CD9", "#176EDD", "#1470E1", "#1072E4", "#0C74E8", "#0876EC", "#0578F0")
  
  binned = cbind.data.frame(value=-100:100,bin = cut(-100:100, breaks = length(rb_scale), labels = as.character(1:64)))
  
  # formating for visNetwork
  
  ## edges
  colnames(evis) = c('from', "color", "to", "value")
  evis$color[evis$color == 1] = '#0578F0' #blue
  evis$color[evis$color == -1] = "#F20404" #red
  evis$color[evis$color == 0] = '#777777' #gray
  
  ## legend for edges
  
  ledges <- data.frame(color = c("#233f5c", "#f20404"),
                       label = c("activation", "inhibition"), 
                       arrows = c("to", "to"),
                       font.align = "top")
  
  ## nodes
  nvis = nvis[which(nvis$ZeroAct!=100),]
  nvis$ZeroAct = NULL
  nvis = nvis[which(nvis$Node%in%union(evis$from, evis$to)),]
  
  if(!is.null(mapIDs)){
    nvis = merge.data.frame(nvis, mapIDs[,c("uniprot_id", "hgnc_symbol")], by.x = "Node", by.y = "uniprot_id")
  }else{
    nvis$label = nvis$Node
  }
  
  nvis <- nvis %>% dplyr::rename(id = "Node",
                                 group = "NodeType",
                                 color = "AvgAct")
  
  
  nvis$group = replace(nvis$group, nvis$group=='M', 'TFs')
  nvis$group = replace(nvis$group, nvis$group=='P', 'Perturbed')
  nvis$group = replace(nvis$group, nvis$group=='', 'Protein')
  
  nvis$color = sapply(nvis$color, function(x,b,rb){rb[as.integer(as.character(b$bin[b$value==as.integer(x)]))]}, binned, rb_scale)
  
  nvis$title = paste0("<p><b>", nvis$label,"</b><br>Up activity: ",nvis$UpAct,"</b><br>Down activity: ",nvis$DownAct,"</p>")
  
  #nvis = rbind.data.frame(nvis, df)
  
  nvis$level = rep(3, nrow(nvis))
  aux = unique(evis$to[which(evis$from%in%unique(nvis$id[which(nvis$group=="Perturbed")]))])
  nvis$level[which(nvis$id%in%aux)] = 2
  aux = unique(evis$from[which(evis$to%in%unique(nvis$id[which(nvis$group=="TFs")]))])
  nvis$level[which(nvis$id%in%aux)] = 4
  nvis$level[which(nvis$group=="Perturbed")] = 1
  nvis$level[which(nvis$group=="TFs")] = 5
  nvis$level[which(nvis$group=="Pathway")] = 6
  
  # Render network
  vNet <- visNetwork(nvis, evis, height = '700px', width = "100%", main = NULL) %>% 
    visNodes(color = list(background="#F5F7FA", border="gray"),
             font =  list(color="#23282a", size=30)) %>%
    visEdges(length = 200, arrowStrikethrough = FALSE, 
             arrows = list(to = list(enabled = TRUE)),
             font =  list(color="#23282a", size=30)) %>%
    visGroups(groupname = "TFs", shape = "triangle", color = "#F5F7FA") %>%
    visGroups(groupname = "Perturbed", shape = "square", color = "#F5F7FA") %>%
    visGroups(groupname = "Protein", color = "#F5F7FA") %>%
    visGroups(groupname = "Pathway",  shape = "box", color = "#F5F7FA") %>%
    visPhysics(solver = "repulsion") %>%
    visLayout(randomSeed = 9) %>%
    visLegend(addEdges = ledges, useGroups = T) %>%
    visHierarchicalLayout(levelSeparation = 500, #nodeSpacing = 500,
                          sortMethod = "directed",
                          edgeMinimization=F, blockShifting=F) %>%
    visPhysics(hierarchicalRepulsion = list(nodeDistance = 300)) %>%
    visOptions(manipulation = TRUE, collapse = TRUE)
  
  return(vNet)
}