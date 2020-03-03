#'\code{generate_measfile}
#'
#' This function generates the measurement file for each condition which is 
#' required as one of the input file
#'
#' @param measurements A dataframe describing the measurements (e.g. inferred 
#' TF activities). Columns/colnames should correspond to perturbations, 
#' row/rownames to measurements.
#' @param topnumber If given, only the top number of measurements will be
#'  written out.
#' @param write2folder Path to outputfolder
#'
#' @return A n*m dataframe describing the inferred TF activities, where n is 
#' the number of Tfs and m the number of conditions.
#'
#' @import dplyr
#' 
#' @export
#' 
#' Enio Gjerga, 2020

generate_measfile<-function(measurements, 
                            topnumber=NULL, write2folder="./measurements"){
  
  if(dir.exists(write2folder)==FALSE){dir.create(write2folder)}
  
  drugs<-colnames(measurements)
  
  for (i in 1:ncol(measurements)){
    df_drugall<-data.frame('measname'=rownames(measurements),
                           'drug'=measurements[,i])

    df_drugall<-df_drugall[order((abs(df_drugall[,2])), decreasing = TRUE),]
    drug<-drugs[i]
    if(is.null(topnumber)){
      df_drug<-t(df_drugall)
      filepath=paste0( write2folder,"/meas_",drug ,"_all.txt")
    }else{
      df_drug<-t(df_drugall[1:topnumber,])
      filepath=paste0( write2folder,"/meas_",drug ,"_", topnumber,".txt")
    }
    write.table(df_drug, filepath, sep="\t", quote = FALSE, 
                col.names = FALSE, row.names = FALSE)
  }

  print(paste0("Measurement files have been written to ", 
               normalizePath(write2folder)))
  
}

