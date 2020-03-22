#'\code{assignPROGENyScores}
#'
#'@details This function is used to assign the PROGENy weights to pathway 
#'members for a selected set of samples.
#'
#'@param progeny contains the progeny scores as obtained from \code{runPROGENy}.
#'@param progenyMembers contains the list of members for each PROGENy pathway.
#'@param id contains the members identifiers (default: gene).
#'@param access_idx index of the sample to consider (default set to 
#'access_idx = 1)
#'
#'@return It returns a data-frame where columns are the representative nodes for
#'each pathway member (as in progenyMembers list), while the eleents are the
#'estimated normalized (from -1 to 1) pthway activities.
#'
#'@author Enio Gjerga & Panuwat Trairatphisan, 2020 \email{carnival.developers@gmail.com}
#'
#'@examples
#'library(CARNIVAL)
#'library(CARNIVAL)
#'expr <- as.matrix(read.csv(system.file("extdata", "human_input.csv", 
#'                                                   package = "progeny"), 
#'                                                   row.names = 1))
#'
#'human_def_act <- progeny(expr, scale = TRUE,  organism = "Human", top = 100, 
#'                         perm = 10000, z_scores = FALSE)
#'
#'## loading the progeny members to assign the weights
#'load(file = system.file("progenyMembers.RData",package="CARNIVAL"))
#'
#'## now assigning the PROGENy weights to pathway members only for the first 
#'## sample which we can consider for the CARNIVAL analysis
#'weightObj <- assignPROGENyScores(progeny = human_def_act, 
#'                                 progenyMembers = progenyMembers, 
#'                                 id = "gene", access_idx = 1)
#'
#'@export
#'

assignPROGENyScores <- function(progeny = progeny, 
                                progenyMembers = progenyMembers, 
                                id = "gene", access_idx = 1){
  
  if(id=="uniprot"){
    
    idx <- which(names(progenyMembers)=="uniprot")
    progenyMembers <- progenyMembers[[idx]]
    
  } else {
    
    idx <- which(names(progenyMembers)=="gene")
    progenyMembers <- progenyMembers[[idx]]
    
  }
  
  members <- matrix(data = , nrow = 1, ncol = 2)
  pathways <- colnames(progeny)
  
  ctrl <- intersect(x = access_idx, y = 1:nrow(progeny))
  if(length(ctrl)==0){
    stop("The indeces you inserted do not correspond to 
              the number of rows/samples")
  }
  
  for(ii in 1:length(pathways)){
    
    mm <- progenyMembers[[which(names(progenyMembers)==pathways[ii])]]
    for(jj in 1:length(mm)){
      
      members <- rbind(members, c(pathways[ii], mm[jj]))
      
    }
    
  }
  
  members <- members[-1, ]
  
  scores <- matrix(data = , nrow = nrow(progeny), ncol = nrow(members))
  colnames(scores) <- members[, 2]
  rownames(scores) <- rownames(progeny)
  
  members <- unique(members)
  
  for(i in 1:ncol(scores)){
    
    for(j in 1:nrow(scores)){
      
      scores[j, i] <- 
        as.numeric(
          progeny[j, members[which(members[, 2]==colnames(scores)[i]), 1]])
      
    }
    
  }
  
  pxList <- list()
  for(ii in 1:length(access_idx)){
    pxList[[length(pxList)+1]] <- 
      as.data.frame(t(as.matrix(scores[access_idx[ii], ])))
  }
  
  names(pxList) <- rownames(progeny)[ctrl]
  
  return(pxList)

  
}