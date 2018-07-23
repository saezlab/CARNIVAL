assignPROGENyScores <- function(progeny = progeny, progenyMembers = progenyMembers, id = "uniprot"){
  
  if(id=="uniprot"){
    
    idx <- which(names(progenyMembers)=="uniprot")
    progenyMembers <- progenyMembers[[idx]]
    
  } else {
    
    idx <- which(names(progenyMembers)=="gene")
    progenyMembers <- progenyMembers[[idx]]
    
  }
  
  members <- matrix(data = , nrow = 1, ncol = 2)
  pathways <- colnames(progeny)
  
  for(ii in 1:length(pathways)){
    
    mm <- progenyMembers[[which(names(progenyMembers)==pathways[ii])]]
    for(jj in 1:length(mm)){
      
      members <- rbind(members, c(pathways[ii], mm[jj]))
      
    }
    
  }
  
  members <- members[-1, ]
  
  scores <- matrix(data = , nrow = 1, ncol = nrow(members))
  colnames(scores) <- members[, 2]
  
  members <- unique(members)
  
  for(i in 1:ncol(scores)){
    
    scores[1, i] <- as.numeric(progeny[1, members[which(members[, 2]==colnames(scores)[i]), 1]])
    
  }
  
  return(scores)

  
}