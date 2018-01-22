# Measurement data formatting from csv files

setwd("~/Desktop/CARNIVAL_Validation/E-MTAB-2091/data") # set working directory (relative)

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
sum(Dorothea_Density_All < (-1.5) | Dorothea_Density_All>1.5) # n=1990 (from n=6708)

# Combined data measurement file writing
Dorothea_CutOff_Abs_1p5 <- Dorothea[,2:ncol(Dorothea)]
Dorothea_CutOff_Abs_1p5[Dorothea_CutOff_Abs_1p5<1.5 & Dorothea_CutOff_Abs_1p5>(-1.5)] <- NaN
Dorothea_CutOff_Abs_1p5[Dorothea_CutOff_Abs_1p5>=1.5] <- 1
Dorothea_CutOff_Abs_1p5[Dorothea_CutOff_Abs_1p5<=(-1.5)] <- -1
Dorothea_CutOff_Abs_1p5[is.nan(as.matrix(Dorothea_CutOff_Abs_1p5))] <- 0
ColNamesNew <- substring(colnames(Dorothea_CutOff_Abs_1p5),4)
colnames(Dorothea_CutOff_Abs_1p5) <- ColNamesNew
write.table(x = Dorothea_CutOff_Abs_1p5,file = "Dorothea_CutOff_Abs_1p5_All",quote = F,sep = "\t",col.names = T,row.names = F)

# Single data measurement file writing
Dorothea_CutOff_Abs_1p5_betaxolol <- Dorothea_CutOff_Abs_1p5[1,]
ColNamesSingle <- ColNamesNew[Dorothea_CutOff_Abs_1p5_betaxolol!=0]
Dorothea_CutOff_Abs_1p5_betaxolol <- matrix(Dorothea_CutOff_Abs_1p5_betaxolol[Dorothea_CutOff_Abs_1p5_betaxolol!=0],nrow = 1,ncol = length(Dorothea_CutOff_Abs_1p5_betaxolol[Dorothea_CutOff_Abs_1p5_betaxolol!=0]))
colnames(Dorothea_CutOff_Abs_1p5_betaxolol) <- ColNamesSingle
write.table(x = Dorothea_CutOff_Abs_1p5_betaxolol,file = "Dorothea_CutOff_Abs_1p5_betaxolol",quote = F,sep = "\t",col.names = T,row.names = F)


# --- End of script --- #
