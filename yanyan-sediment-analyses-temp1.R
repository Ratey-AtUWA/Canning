yz1 <- read.csv("../Canning/YZseds1.csv", stringsAsFactors = T)
yz1$Sample.ID <- as.character(yz1$Sample.ID)
yz1 <- droplevels(yz1[1:19,])

(varz <- unname(which(sapply(yz1,is.numeric)==TRUE))[-4]) # [4] is Cd

par(mfrow=c(5,5), mar=c(3,3,1,1), mgp=c(1.7,0.3,0), font.lab=2, cex.lab=1.2)
for(i in 1:length(varz)){
  boxplot(log10(yz1[,varz[i]]) ~ yz1$Sieve, xlab="Size fraction", 
          ylab=paste("log10(",colnames(yz1)[varz[i]],"(mg/kg))"),
          col=c("#fec","#aad"), names=paste("<", levels(yz1$Sieve)))
}
