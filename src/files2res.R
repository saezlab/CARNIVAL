files2res <- function(dir_name) {

  res <- list()
  res$weightedSIF <- as.matrix(read.table(paste0("results/",dir_name,"/weightedModel_1.txt"),header = T,sep = "\t",stringsAsFactors = F))
  res$nodeAttributes <- as.matrix(read.table(paste0("results/",dir_name,"/nodesAttributes_1.txt"),header = T,sep = "\t",stringsAsFactors = F))
  res$sifAll <- list()
  res$attributesAll <- list()
  
  AllFiles <- list.files(paste0("results/",dir_name))
  ModelFiles <- which(grepl(pattern = "interactions_1_model",x = AllFiles,fixed = T))
  
  for (counter in 1:length(ModelFiles)) { 
    res$sifAll[[counter]] <- as.matrix(read.table(paste0("results/",dir_name,"/interactions_1_model",counter,".tsv"),header = T,sep = "\t",stringsAsFactors = F))
    res$attributesAll[[counter]] <- as.matrix(read.table(paste0("results/",dir_name,"/nodesActivity_1_model",counter,".txt"),header = T,sep = "\t",stringsAsFactors = F))
  }
  
  return(res)

}