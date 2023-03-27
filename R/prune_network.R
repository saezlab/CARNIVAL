#' prune_network
#'
#' Prune a network by upstream and downstream nodes
#'
#' This function takes a network in dataframe format with three columns
#' (source, target, and interaction), a set of upstream nodes, and a set of downstream nodes.
#' It returns a pruned network containing only nodes that can be reached downstream
#' of the upstream set of nodes and upstream of the downstream set of nodes.
#'
#' @param network A dataframe with three columns: source, target, and sign.
#' @param upstream_nodes A character vector of upstream node names.
#' @param downstream_nodes A character vector of downstream node names.
#' 
#'
#' @return A dataframe of the pruned network with three columns: source, target, and sign.
#' 
#' @import igraph
#' 
#' @export
#' 
#' @examples
#' # Sample network data
#' network_data <- data.frame(
#'   source = c("A", "A", "B", "B", "C", "D", "E"),
#'   target = c("B", "C", "C", "D", "E", "F", "F"),
#'   interaction = c(1, -1, 1, -1, 1, 1, -1)
#' )
#'
#' # Upstream and downstream node sets
#' upstream_nodes <- c("A", "B")
#' downstream_nodes <- c("E", "F")
#'
#' # Prune the network
#' pruned_network <- prune_network(network_data, upstream_nodes, downstream_nodes)
#' print(pruned_network)
prune_network <- function(network, upstream_nodes, downstream_nodes) {
  # Create an igraph object from the input network
  g <- graph_from_data_frame(network, directed = TRUE)
  
  # Get all nodes in the network
  all_nodes <- V(g)$name
  
  # Filter upstream and downstream nodes to only include nodes present in the network
  upstream_nodes <- upstream_nodes[upstream_nodes %in% all_nodes]
  downstream_nodes <- downstream_nodes[downstream_nodes %in% all_nodes]
  
  # Find nodes reachable downstream of the upstream set of nodes
  downstream_reachable_nodes <- unique(names(unlist(ego(g, upstream_nodes, mode = "out", order = 1000))))
  
  # Find nodes reachable upstream of the downstream set of nodes
  upstream_reachable_nodes <- unique(names(unlist(ego(g, downstream_nodes, mode = "in", order = 1000))))
  
  # Find the intersection of the two sets of reachable nodes
  common_reachable_nodes <- intersect(downstream_reachable_nodes, upstream_reachable_nodes)
  
  # Add the upstream and downstream nodes to the common_reachable_nodes
  final_node_set <- unique(c(upstream_nodes, downstream_nodes, common_reachable_nodes))
  
  # Subgraph the original graph using the final_node_set
  pruned_graph <- induced_subgraph(g, final_node_set)
  
  # Convert the pruned_graph back to a dataframe
  pruned_network <- get.data.frame(pruned_graph)
  
  names(pruned_network) <- names(network)
  # Return the pruned network
  return(pruned_network)
}