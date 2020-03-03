#'\code{generateTFList}
#'
#' Creating regulon list similar to vipper from an interaction data-frame
#' 
#' @param df dataframe with TF activities
#' @param top number of top regulated TF's (default set to top = 50)
#' @param access_idx index of the sample to consider (default set to 
#' access_idx = 1)
#' 
#'
#'Enio Gjerga, 2020

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