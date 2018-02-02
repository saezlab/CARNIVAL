Discretize_CARNIVAL <- function(dataMat,method,cutoff) {

# Take data matrix (columns=genes/proteins data,rows=samples) & identify discretisation cutoff into -1 and 1  
# method: 1=absolute value, 2=mean+/-2.5*SD (Gaussian), 3= median+/-2.5*mean_abs_diff    
  
  # Defining cut-off with different methods
  
  if (method == 1) {
    
    CutOff_method <- cutoff 
  
    CutOff_Up <- rep(cutoff,ncol(dataMat)-1)
    CutOff_Down <- rep((-1)*cutoff,ncol(dataMat)-1)
    
  } else if (method==2) {
    
    method_Means <- colMeans(dataMat[,2:ncol(dataMat)])
    method_SD <- apply(dataMat[,2:ncol(dataMat)],2,sd)
    
    CutOff_Up   <- method_Means+(cutoff*method_SD)
    CutOff_Down <- method_Means-(cutoff*method_SD)
    
  } else if (method==3) {
    
    method_Median <- apply(dataMat[,2:ncol(dataMat)],2,median)
    method_Means <- colMeans(dataMat[,2:ncol(dataMat)])
    method_MAD <- NULL
    for (counter in 1:(ncol(dataMat)-1)) {
      method_MAD <- c(method_MAD,sum(abs((dataMat[,counter+1] - method_Means[counter])))/nrow(dataMat))
    }
    
    CutOff_Up   <- method_Median+(cutoff*method_MAD)
    CutOff_Down <- method_Median-(cutoff*method_MAD)
    
  }
  
  dataMat_Cutoff <- dataMat[,2:ncol(dataMat)]
  dataMat_Cutoff[dataMat_Cutoff<CutOff_Up & dataMat_Cutoff>CutOff_Down] <- NaN
  dataMat_Cutoff[dataMat_Cutoff>=CutOff_Up] <- 1
  dataMat_Cutoff[dataMat_Cutoff<=CutOff_Down] <- -1
  dataMat_Cutoff[is.nan(as.matrix(dataMat_Cutoff))] <- 0
  rownames(dataMat_Cutoff) <- dataMat[,1]
  
  return(dataMat_Cutoff)

}