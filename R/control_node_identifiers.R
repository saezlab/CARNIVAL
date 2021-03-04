## Warning message in case of unwanted symbols in node id's
##
## Enio Gjerga, 2020

collect_special_characters_names <- function(names_with_special_characters, nodes_names) {
  counter <- 0
  res <- lapply(names_with_special_characters, function(x) {
    counter <<- counter + 1
    if (x[1] != -1){
      substr(names[counter], x[1], x[1])  
    }
  })
  names(res) <- nodes_names
  
  return(res)
}

#TODO keep names mapping
#TODO v.1.2
controlNodeIdentifiers_v1_2 <- function(network = network, verbose=F) {
  special_characters <- c("*", "+", "=")
  substitution_character <- c("-", "<", ">", "/", " ")
  
  prepared_pattern <- paste0("\\", special_characters, collapse="|")
  prepared_pattern <- paste0(c(prepared_pattern, substitution_character), collapse="|")
  
  if (verbose) {

    names_with_special_characters_source <- gregexpr(pattern=prepared_pattern, text = network$source) 
    names_with_special_characters_target <- gregexpr(pattern=prepared_pattern, text = network$target) 
    
    res_source <- collect_special_characters_names(names_with_special_characters, network$source)
    res_target <- collect_special_characters_names(names_with_special_characters, network$target)
    
    all_special_characters_found <- unique(unlist(res_source), unlist(res_target))
    
    all_substituted_names_source <- names(res_source[lengths(res_source) != 0])
    all_substituted_names_target <- names(res_target[lengths(res_target) != 0])
    
    warning("Your network contains node identifiers with characters ", 
            paste0(all_special_characters_found, sep=", "), " and they will
            be replaced with '_'")
    
  }  

  network$source = gsub(pattern=prepared_pattern, x = test_grepl, replacement="_")  
  network$target = gsub(pattern=prepared_pattern, x = test_grepl, replacement="_")  
  
  return(network) 
}

controlNodeIdentifiers <- function(netObj = netObj){
  
  idx1 <- which(grepl(pattern = "-", x = netObj$source, fixed = TRUE))
  idx2 <- which(grepl(pattern = "-", x = netObj$target, fixed = TRUE))
  if(length(idx1)>0 || length(idx2)>0){
    warning("Your network contains identifiers with '-' symbol and they will
            be replaced with '_'")
    netObj$source = gsub(pattern = "-", replacement = "_", 
                         x = netObj$source, fixed = TRUE)
    netObj$target <- gsub(pattern = "-", replacement = "_", 
                          x = netObj$target, fixed = TRUE)
  }
  
  idx1 <- which(grepl(pattern = "+", x = netObj$source, fixed = TRUE))
  idx2 <- which(grepl(pattern = "+", x = netObj$target, fixed = TRUE))
  if(length(idx1)>0 || length(idx2)>0){
    warning("Your network contains identifiers with '+' symbol and they will
            be replaced with '_'")
    netObj$source = gsub(pattern = "+", replacement = "_", 
                         x = netObj$source, fixed = TRUE)
    netObj$target <- gsub(pattern = "+", replacement = "_", 
                          x = netObj$target, fixed = TRUE)
  }
  
  idx1 <- which(grepl(pattern = "*", x = netObj$source, fixed = TRUE))
  idx2 <- which(grepl(pattern = "*", x = netObj$target, fixed = TRUE))
  if(length(idx1)>0 || length(idx2)>0){
    warning("Your network contains identifiers with '*' symbol and they will
            be replaced with '_'")
    netObj$source = gsub(pattern = "*", replacement = "_", 
                         x = netObj$source, fixed = TRUE)
    netObj$target <- gsub(pattern = "*", replacement = "_", 
                          x = netObj$target, fixed = TRUE)
  }
  
  idx1 <- which(grepl(pattern = "/", x = netObj$source, fixed = TRUE))
  idx2 <- which(grepl(pattern = "/", x = netObj$target, fixed = TRUE))
  if(length(idx1)>0 || length(idx2)>0){
    warning("Your network contains identifiers with '/' symbol and they will
            be replaced with '_'")
    netObj$source = gsub(pattern = "/", replacement = "_", 
                         x = netObj$source, fixed = TRUE)
    netObj$target <- gsub(pattern = "/", replacement = "_", 
                          x = netObj$target, fixed = TRUE)
  }
  
  idx1 <- which(grepl(pattern = "<", x = netObj$source, fixed = TRUE))
  idx2 <- which(grepl(pattern = "<", x = netObj$target, fixed = TRUE))
  if(length(idx1)>0 || length(idx2)>0){
    warning("Your network contains identifiers with '<' symbol and they will
            be replaced with '_'")
    netObj$source = gsub(pattern = "<", replacement = "_", 
                         x = netObj$source, fixed = TRUE)
    netObj$target <- gsub(pattern = "<", replacement = "_", 
                          x = netObj$target, fixed = TRUE)
  }
  
  ##
  idx1 <- which(grepl(pattern = ">", x = netObj$source, fixed = TRUE))
  idx2 <- which(grepl(pattern = ">", x = netObj$target, fixed = TRUE))
  if(length(idx1)>0 || length(idx2)>0){
    warning("Your network contains identifiers with '>' symbol and they will
            be replaced with '_'")
    netObj$source = gsub(pattern = ">", replacement = "_", 
                         x = netObj$source, fixed = TRUE)
    netObj$target <- gsub(pattern = ">", replacement = "_", 
                          x = netObj$target, fixed = TRUE)
  }
  
  idx1 <- which(grepl(pattern = "=", x = netObj$source, fixed = TRUE))
  idx2 <- which(grepl(pattern = "=", x = netObj$target, fixed = TRUE))
  if(length(idx1)>0 || length(idx2)>0){
    warning("Your network contains identifiers with '=' symbol and they will
            be replaced with '_'")
    netObj$source = gsub(pattern = "=", replacement = "_", 
                         x = netObj$source, fixed = TRUE)
    netObj$target <- gsub(pattern = "=", replacement = "_", 
                          x = netObj$target, fixed = TRUE)
  }
  
  idx1 <- which(grepl(pattern = " ", x = netObj$source, fixed = TRUE))
  idx2 <- which(grepl(pattern = " ", x = netObj$target, fixed = TRUE))
  if(length(idx1)>0 || length(idx2)>0){
    warning("Your network contains identifiers with spaces and they will
            be replaced with '_'")
    netObj$source = gsub(pattern = " ", replacement = "_", 
                         x = netObj$source, fixed = TRUE)
    netObj$target <- gsub(pattern = " ", replacement = "_", 
                          x = netObj$target, fixed = TRUE)
  }
  
  return(netObj)
  
}
