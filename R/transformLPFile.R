## Vectorizing the bounds
##
## Enio Gjerga, 2020

transformLPFile <- function(lpFileName = "testFile_1_1.lp"){
  
  lpFile <- read_csv(file = lpFileName)
  
  lpFile$`enter Problem` <- as.character(lpFile$`enter Problem`)
  
  idx1 <- which(lpFile$`enter Problem`=="Subject To")
  idx2 <- which(lpFile$`enter Problem`=="Bounds")
  idx3 <- which(lpFile$`enter Problem`=="Binaries")
  idx4 <- which(lpFile$`enter Problem`=="Generals")
  idx5 <- which(lpFile$`enter Problem`=="End")
  
  idx <- which(grepl(pattern = "Obj:", x = lpFile$`enter Problem`))
  
  ##
  newLP <- "min: "
  
  ##
  newLP <- paste0(newLP, gsub(pattern = "Obj:\t ", 
                              replacement = "", 
                              x = lpFile$`enter Problem`[idx], fixed = TRUE), 
                  ";")
  
  ##
  newLP <- append(newLP, paste0(gsub(pattern = "\t", 
                                     replacement = " ", 
                                     x = lpFile$`enter Problem`[(idx1+1):(idx2-1)]), 
                                ";"))
  
  ##
  newLP <- append(newLP, paste0(lpFile$`enter Problem`[(idx2+1):(idx3-1)], ";"))
  
  ##
  tmp <- "bin "
  for(ii in (idx3+1):(idx4-1)){
    tmp <- paste0(tmp, lpFile$`enter Problem`[ii], ", ")
  }
  tmp <- paste0(tmp, ";")
  
  newLP <- append(newLP, tmp)
  
  ##
  tmp <- "int "
  for(ii in (idx3+1):(idx4-1)){
    tmp <- paste0(tmp, lpFile$`enter Problem`[ii], ", ")
  }
  tmp <- paste0(tmp, ";")
  
  newLP <- append(newLP, tmp)
  
  write(newLP, file = "tmp.lp")
  
}