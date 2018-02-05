Discretize_CARNIVAL <- function(dataMat,method,cutoff) {

# Take data matrix (columns=genes/proteins data,rows=samples) & identify discretisation cutoff into -1 and 1  
# method: 1=absolute value, 2=mean+/-cutoff*SD (Gaussian), 3= median+/-cutoff*mean_abs_diff    
  
  # Defining cut-off with different methods
  
  if (method == 1) {
    
    CutOff_method <- cutoff 
  
    CutOff_Up <- rep(cutoff,ncol(dataMat))
    CutOff_Down <- rep((-1)*cutoff,ncol(dataMat))
    
  } else if (method==2) {
    
    method_Means <- colMeans(dataMat)
    method_SD <- apply(dataMat,2,sd)
    
    CutOff_Up   <- method_Means+(cutoff*method_SD)
    CutOff_Down <- method_Means-(cutoff*method_SD)
    
  } else if (method==3) {
    
    method_Median <- apply(dataMat,2,median)
    method_Means <- colMeans(dataMat)
    method_MAD <- NULL
    for (counter in 1:(ncol(dataMat))) {
      method_MAD <- c(method_MAD,sum(abs((dataMat[,counter] - method_Means[counter])))/nrow(dataMat))
    }
    
    CutOff_Up   <- method_Median+(cutoff*method_MAD)
    CutOff_Down <- method_Median-(cutoff*method_MAD)
    
  }
  
  dataMat_Cutoff <- dataMat
  
  for (counter in 1:ncol(dataMat_Cutoff)) {
    dataMat_Cutoff[which(dataMat_Cutoff[,counter]<CutOff_Up[counter] & dataMat_Cutoff[,counter]>CutOff_Down[counter]),counter] <- NaN
    dataMat_Cutoff[which(dataMat_Cutoff[,counter]>=CutOff_Up[counter]),counter] <- 1
    dataMat_Cutoff[which(dataMat_Cutoff[,counter]<=CutOff_Down[counter]),counter] <- -1
    dataMat_Cutoff[which(is.nan(as.matrix(dataMat_Cutoff[,counter]))),counter] <- 0
  }
  rownames(dataMat_Cutoff) <- rownames(dataMat)

  return(dataMat_Cutoff)

}