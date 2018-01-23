# Measurement data formatting from csv files

setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/archive/CARNIVAL_Validation/E-MTAB-2091/data") # set working directory (relative)

# E-MTAB-2091_DoRothEA.csv
Dorothea <- read.table("E-MTAB-2091_DoRothEA.csv",header=T,sep=",",stringsAsFactors = F)
par(mfrow=c(8,7))
colname <- colnames(Dorothea[2:length(Dorothea)])
pdf("Dorothea_Density_Plot.pdf")

Dorothea_Density_List <- list()
for (counter in 1:(ncol(Dorothea)-1)) {
  d <- density(as.numeric(Dorothea[,counter+1]))
  plot(d, type="n",main=colname[counter])
  polygon(d, col="red",border="gray") 
  # hist(as.numeric(Dorothea[1,2:nrow(Dorothea)]))
  Dorothea_Density_List[[counter]] <- as.numeric(Dorothea[,counter+1])
}
dev.off()

par(mfrow=c(1,1))
Dorothea_Density_All <- unlist(Dorothea_Density_List)
pdf("Dorothea_All_Histogram.pdf")
hist(Dorothea_Density_All)
dev.off()
d_all <- density(Dorothea_Density_All)
plot(d_all,type="n")
polygon(d_all,col="blue")

# Defining cut-off
# CutOffDorothea <- 1.5 # n=1990 (from n=6708)
CutOffDorothea <- 1 # n=3152 (from n=6708)
sum(Dorothea_Density_All < (-1*CutOffDorothea) | Dorothea_Density_All>CutOffDorothea) 

# Combined data measurement file writing
Dorothea_Cutoff <- Dorothea[,2:ncol(Dorothea)]
Dorothea_Cutoff[Dorothea_Cutoff<CutOffDorothea & Dorothea_Cutoff>(-CutOffDorothea)] <- NaN
Dorothea_Cutoff[Dorothea_Cutoff>=CutOffDorothea] <- 1
Dorothea_Cutoff[Dorothea_Cutoff<=(-CutOffDorothea)] <- -1
Dorothea_Cutoff[is.nan(as.matrix(Dorothea_Cutoff))] <- 0
ColNamesNew <- substring(colnames(Dorothea_Cutoff),4)
colnames(Dorothea_Cutoff) <- ColNamesNew
write.table(x = Dorothea_Cutoff,file = paste0("Dorothea_All_Cutoff_",toString(CutOffDorothea),".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)

# Single data measurement file writing
# betaxolol
Dorothea_Cutoff_betaxolol <- Dorothea_Cutoff[1,]
ColNamesSingle <- ColNamesNew[Dorothea_Cutoff_betaxolol!=0]
Dorothea_Cutoff_betaxolol <- matrix(Dorothea_Cutoff_betaxolol[Dorothea_Cutoff_betaxolol!=0],nrow = 1,ncol = length(Dorothea_Cutoff_betaxolol[Dorothea_Cutoff_betaxolol!=0]))
colnames(Dorothea_Cutoff_betaxolol) <- ColNamesSingle
write.table(x = Dorothea_Cutoff_betaxolol,file = paste0("Dorothea_betaxolol_Cutoff_",toString(CutOffDorothea),".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)

# all compounds
Dorothea_Compounds <- Dorothea[,1]
Dorothea_Compounds_All <- NULL
for (counter in 1:length(Dorothea_Compounds)) {
  Dorothea_Compounds_All[[counter]] <- strsplit(Dorothea_Compounds[counter],split = " ",fixed = T)
}
Dorothea_Compounds_Names <- NULL
for (counter in 1:length(Dorothea_Compounds_All)) {
  if (length(Dorothea_Compounds_All[[counter]][[1]])==1) {
    Dorothea_Compounds_Names[[counter]] <- Dorothea_Compounds_All[[counter]][[1]][1]
  } else {
    if (grepl(pattern = "(",x = Dorothea_Compounds_All[[counter]][[1]][2],fixed = T)) {
      Dorothea_Compounds_Names[[counter]] <- Dorothea_Compounds_All[[counter]][[1]][1]
    } else {
      Dorothea_Compounds_Names[[counter]] <- paste0(Dorothea_Compounds_All[[counter]][[1]][1],"_",Dorothea_Compounds_All[[counter]][[1]][2])
    }
  }
}
Dorothea_Compounds_Names <- unlist(Dorothea_Compounds_Names)
write.table(x = Dorothea_Compounds_Names,file = "All_Compound_Names.tsv",quote = F,sep = "\t",col.names = F,row.names = F)


for (counter in 1:length(Dorothea_Compounds_Names)) {
  Dorothea_Cutoff_current <- Dorothea_Cutoff[counter,]
  ColNamesSingle <- ColNamesNew[Dorothea_Cutoff_current!=0]
  Dorothea_Cutoff_current <- matrix(Dorothea_Cutoff_current[Dorothea_Cutoff_current!=0],nrow = 1,ncol = length(Dorothea_Cutoff_current[Dorothea_Cutoff_current!=0]))
  colnames(Dorothea_Cutoff_current) <- ColNamesSingle
  write.table(x = Dorothea_Cutoff_current,file = paste0("Dorothea_",Dorothea_Compounds_Names[counter],"_Cutoff_",toString(CutOffDorothea),".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)
}

# --- End of script --- #
