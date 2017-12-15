network <- read.table("Network_Generic_FIN_RemovedTFGenes.sif", sep = "\t", header = FALSE,stringsAsFactors = F)

which(grepl(pattern = "-",x = network[,1],fixed = T)) # indices to replace

network[,1] <- gsub("-","~",network[,1],fixed = T)
network[,3] <- gsub("-","~",network[,3],fixed = T)
write.table(x = network,file = "Network_Generic_FIN_RemovedTFGenes_NoHyphen.sif",sep="\t",quote = F,row.names = F,col.names = F)
