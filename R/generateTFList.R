#'\code{generateTFList}
#'
#'@details This function generates a list of data frames containing activity 
#'values for each TF. These data-frames can then be used as inputs for the 
#'CARNIVAL analysis (measObj). We can control the amount of TF's we include in 
#'our data-frames based on their absolute activity values through the top 
#'parameter.
#' 
#'@param df dataframe with TF activities
#'@param top number of top regulated TF's (default set to top = 50, if 
#'considering all, then user can set top = "all")
#'@param access_idx index of the sample to consider (default set to 
#'access_idx = 1)
#'
#'@return It will return a list containing the highest active TF's with their 
#'estimated activity levels in a data-frame for each sample where TF activty 
#'estimation analysis was performed.
#'
#'@author Enio Gjerga, 2020 \email{carnival.developers@gmail.com}
#'
#'@examples
#'library(CARNIVAL)
#'library(OmnipathR)
#'library(viper)
#'library(progeny)
#'
#'expr <- as.matrix(read.csv(system.file("extdata", "human_input.csv", 
#'                                       package = "progeny"), 
#'                           row.names = 1))
#'
#'regulon_df <- 
#'  import_TFregulons_Interactions(select_organism = 9606)
#'
#'regulon_df <- 
#'  regulon_df[which((regulon_df$is_stimulation+regulon_df$is_inhibition)==1), ]
#'
#'regulon_table <- matrix(data = , nrow = nrow(regulon_df), ncol = 3)
#'regulon_table[, 1] <- regulon_df$source_genesymbol
#'regulon_table[which(regulon_df$is_stimulation==1), 2] = "1"
#'regulon_table[which(regulon_df$is_inhibition==1), 2] = "-1"
#'regulon_table[, 3] <- regulon_df$target_genesymbol
#'
#'regulons <- createRegulonList(regulon_table = regulon_table)
#'
#'TF_activities = as.data.frame(viper::viper(eset = expr, 
#'                                           regulon = regulons, nes = T, 
#'                                           method = 'none', minsize = 4, 
#'                                           eset.filter = F))
#'
#'tfList <- generateTFList(df = TF_activities, top = "all", 
#'                         access_idx = 1)
#' 
#' @export
#'

generateTFList <- function(df = df, top = 50, access_idx = 1){
  
  if(top=="all"){
    top <- nrow(df)
  }
  
  if(top > nrow(df)){
    warning("Number of to TF's inserted exceeds the number of actual TF's in the
            data frame. All the TF's will be considered.")
    top <- nrow(df)
  }
  
  ctrl <- intersect(x = access_idx, y = 1:ncol(df))
  if(length(ctrl)==0){
    stop("The indeces you inserted do not correspond to 
              the number of columns/samples")
  }
  
  returnList <- list()
  
  for(ii in 1:length(ctrl)){
    
    tfThresh <- sort(x = abs(df[, ctrl[ii]]), decreasing = TRUE)[top]
    temp <- which(abs(df[, ctrl[ii]])>=tfThresh)
    
    currDF <- matrix(data = , nrow = 1, ncol = top)
    colnames(currDF) <- rownames(df)[1:top]
    currDF[1, ] <- df[temp[1:top], ctrl[ii]]
    currDF <- as.data.frame(currDF)
    
    returnList[[length(returnList)+1]] <- currDF
    
  }
  
  names(returnList) <- colnames(df)[ctrl]
  
  return(returnList)
  
}