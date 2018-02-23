InvNormWeight_CARNIVAL <- function(dataMat,method,n=3,k=0.5) {

# Take data matrix (columns=genes/proteins data,rows=samples) & identify discretisation cutoff into -1 and 1  
# method: 1=linear, 2=Hill(default, k=3)
  
  # Defining cut-off with different methods
  
  if (method == 1) {
    
    method_min <- apply(dataMat[,2:ncol(dataMat)],2,min)
    method_max <- apply(dataMat[,2:ncol(dataMat)],2,max)
    dataMat_Weight <- matrix(NA,nrow(dataMat),ncol(dataMat)-1)

    for (counter in 1:(ncol(dataMat)-1)) {
      dataMat_Weight[,counter] <- 1-((dataMat[,counter+1]-method_min[counter])/(method_max[counter]-method_min[counter]))
    }
    colnames(dataMat_Weight) <- colnames(dataMat[,2:ncol(dataMat)])
    rownames(dataMat_Weight) <- dataMat[,1]

  } else if (method==2) {
    
    dataMat_Weight <- 1-((dataMat[,2:ncol(dataMat)]^n)*(1+k^n)/(dataMat[,2:ncol(dataMat)]^n+k^n))
    rownames(dataMat_Weight) <- dataMat[,1]
    
  } 
  
  return(dataMat_Weight)

}