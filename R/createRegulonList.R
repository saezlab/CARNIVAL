#'\code{createRegulonList}
#'
#' @details Creating regulon list similar to vipper from an interaction 
#' data-frame
#' 
#' @param regulon_table the regulon matrix with columns the source, sign and
#' target of the TF interaction.
#' 
#' @return A regulon list similar to the one required for the viper analysis
#' 
#' @author Enio Gjerga, 2020 \email{carnival.developers@gmail.com}
#' 
#' @examples
#' library(CARNIVAL)
#' library(OmnipathR)
#' 
#' regulon_df <- 
#'   import_TFregulons_Interactions(select_organism = 9606)
#' 
#' regulon_df <- 
#'   regulon_df[which((regulon_df$is_stimulation+regulon_df$is_inhibition)==1), ]
#' 
#' regulon_table <- matrix(data = , nrow = nrow(regulon_df), ncol = 3)
#' regulon_table[, 1] <- regulon_df$source_genesymbol
#' regulon_table[which(regulon_df$is_stimulation==1), 2] = "1"
#' regulon_table[which(regulon_df$is_inhibition==1), 2] = "-1"
#' regulon_table[, 3] <- regulon_df$target_genesymbol
#'
#' regulons <- createRegulonList(regulon_table = regulon_table)
#'
#' @export

createRegulonList <- function(regulon_table = regulon_table){
  
  
  if((!is.character(regulon_table[, 1])) || 
     !is.character(regulon_table[, 3])){
    
    stop("Source and Target columns should be of class character")
    
  }
  
  if(!((is.character(regulon_table[, 2])) || 
     (is.numeric(regulon_table[, 2])))){
    
    stop("Interaction sign column should either be of class character/numeric")
    
  }
  
  sgn <- as.numeric(as.character(regulon_table[, 2]))
  if(!(all(sgn%in%c(1, -1)))){
    stop("Please check your interaction sign column. 
         They should be either 1/-1.")
  }
  
  regulon <- list()
  sourceList <- unique(regulon_table[, 1])
  
  for(ii in 1:length(sourceList)){
    
    currRegulon <- list()
    vec <- as.numeric(
      as.character(regulon_table[which(regulon_table[, 1]==sourceList[ii]), 2]))
    names(vec) <- regulon_table[which(regulon_table[, 1]==sourceList[ii]), 3]
    
    currRegulon[[length(currRegulon)+1]] <- vec
    currRegulon[[length(currRegulon)+1]] <- rep(1, length(vec))
    
    names(currRegulon) <- c("tfmode", "likelihood")
    
    regulon[[length(regulon)+1]] <- currRegulon
    
  }
  
  names(regulon) <- sourceList
  
  return(regulon)
  
}