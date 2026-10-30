# read data
bsed <- read.csv("Weng-Mun-OES-MS-bottom-seds.csv")
ssed <- read.csv("Weng-Mun-OES-MS-susp-seds.csv")

# read crustal reference concentrations
PAAS <- read.csv("../PAAS-REE.csv")
row.names(PAAS) <- PAAS$Element

# calculate REE normalised to PAAS crustal reference
for(n in PAAS$Element){
  bsed[,paste0(n,".PAAS")] <- bsed[,n]/PAAS[n,"C"]
}
for(n in PAAS$Element){
  ssed[,paste0(n,".PAAS")] <- ssed[,n]/PAAS[n,"C"]
}

# subset to just normalised REE
bsed.REE <- ssed[,c(1,which(substr(colnames(bsed),4,7)=="PAAS"))] |> na.omit()
ssed.REE <- ssed[,c(1,which(substr(colnames(ssed),4,7)=="PAAS"))] |> na.omit()

pdat <- t(bsed.REE[,-1]) |> as.data.frame()
colnames(pdat) <- bsed.REE[,1]
pdat <- data.frame(REE=substr(row.names(pdat),1,2), pdat)

pdat2 <- t(ssed.REE[,-1]) |> as.data.frame()
colnames(pdat2) <- ssed.REE[,1]
pdat2 <- data.frame(REE=substr(row.names(pdat2),1,2), pdat2)

library(reshape2)
plotdata <- melt(pdat, id="REE")
nc <- length(bsed.REE$Sample)
plotdata$x <- rep(1:14,nc)
colnames(plotdata)[2:3] <- c("Site","REE.PAAS")

plotdata2 <- melt(pdat2, id="REE")
nc <- length(ssed.REE$Sample)
plotdata2$x <- rep(1:14,nc)
colnames(plotdata2)[2:3] <- c("Site","REE.PAAS")

library(viridis)
library(Cairo)
par(mar=c(3,4,1,1), mgp=c(1.7,0.3,0), tcl=0.2, font.lab=2, cex.lab=1.2)
palette(c("black", turbo(nc), "white"))

# CairoPNG("REE-PAAS-plot-WMO.png", width=1080, height=720, dpi=96)
par(mar=c(3,4,1,1), mgp=c(1.7,0.3,0), tcl=0.2, font.lab=2, cex.lab=1.2)
palette(c("black", turbo(nc), "white"))
plot(1:nrow(pdat), pdat$McNess.DS, type="n", xaxt="n", xlab="REE",
     ylab=expression(bold(paste(REE[sample],"/",REE[PAAS]))), ylim=c(-0.1,19))
axis(1, at=1:nrow(pdat), labels=pdat$REE)
abline(h=1, col="grey", lty=3)
symz <- rep(21:25,4)
for(i in 2:ncol(pdat)){
  points(1:nrow(pdat), pdat[,i], type="b", col=i, bg=i, pch=symz[i-1], cex=1.2)
}
legend("topright", bty="n", ncol=3, legend=colnames(pdat)[-1], pch=symz,
       col=c(1:nc)+1, pt.bg=c(1:nc)+1, pt.cex=1.2, 
       y.int=1.1, title="Site", title.font = 2)
# dev.off()

plot(1:nrow(pdat2), pdat$McNess.DS, type="n", xaxt="n", xlab="REE",
     ylab=expression(paste(REE[sample],"/",REE[PAAS])), ylim=c(-0.1,1.5))
axis(1, at=1:nrow(pdat), labels=pdat$REE)
abline(h=1, col="grey", lty=3)
with(plotdata2, plot(x, REE.PAAS, add=TRUE,
                      col=1+c(1:nc)[Site],
                      pch=-1+c(1:nc)[Site],
                      lty=c(1:nc)[Site]))
# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

library(ggplot2)
library(stringr)
library(mgsub)

justREEss <- t(ssed.REE[,2:ncol(ssed.REE)])
colnames(justREEss) <-ssed.REE[,1]
row.names(justREEss) <- substr(row.names(justREEss),1,2)
justREEss.pdat <- melt(justREEss)
colnames(justREEss.pdat) <- c("Element","Location","Conc")
justREEss.pdat$Loc2 <- str_split_i(justREEss.pdat[,2]," ",1)
justREEss.pdat$Loc2 <- str_split_i(justREEss.pdat$Loc2,"-",1)
justREEss.pdat$Loc2 <- as.factor(justREEss.pdat$Loc2)
justREEss.pdat$AtNo <- as.character(justREEss.pdat$Element)
justREEss.pdat$AtNo <- mgsub(justREEss.pdat$AtNo, 
                             pattern=unique(justREEss.pdat$AtNo),
                             replacement=as.character(c(57:60,62:71)))
justREEss.pdat$AtNo <- as.numeric(justREEss.pdat$AtNo)

justREEbs <- t(bsed.REE[,9:ncol(bsed.REE)])
colnames(justREEbs) <-bsed.REE[,1]
row.names(justREEbs) <- substr(row.names(justREEbs),1,2)
justREEbs.pdat <- melt(justREEbs)
colnames(justREEbs.pdat) <- c("Element","Location","Conc")
justREEbs.pdat$Loc2 <- str_split_i(justREEbs.pdat[,2]," ",1)
justREEbs.pdat$Loc2 <- str_split_i(justREEbs.pdat$Loc2,"-",1)
justREEbs.pdat$Loc2 <- as.factor(justREEbs.pdat$Loc2)
justREEbs.pdat$AtNo <- as.character(justREEbs.pdat$Element)
justREEbs.pdat$AtNo <- mgsub(justREEbs.pdat$AtNo, 
                             pattern=unique(justREEbs.pdat$AtNo),
                             replacement=as.character(c(57:60,62:64)))
justREEbs.pdat$AtNo <- as.numeric(justREEbs.pdat$AtNo)

ggplot()+
  geom_point(aes(x=Element,y=Conc, fill=Loc2, shape=Loc2), size=3.5, 
             data=justREEss.pdat) +
  scale_shape_manual(values=rep(21:25, l=nlevels(justREEss.pdat$Loc2))) +
  scale_fill_viridis_d(option="turbo") +theme_bw()

ggplot()+
  geom_point(aes(x=AtNo,y=Conc, fill=Loc2, shape=Loc2), size=3.5, 
             data=justREEbs.pdat) +
  scale_shape_manual(values=rep(21:25, l=nlevels(justREEbs.pdat$Loc2))) +
  scale_fill_viridis_d(begin=0.1, alpha=0.7) +
  scale_y_log10() +
  theme_bw()

ggplot()+
  geom_boxplot(aes(x=Element,y=Conc, fill=Loc2), data=justREEss.pdat) +
  facet_wrap(facets=justREEss.pdat$Loc2) +
  scale_fill_viridis_d(begin=0.1, alpha=0.5, guide="none") +
  scale_y_log10() +
  theme_bw()
ggplot()+
  geom_boxplot(aes(x=Element,y=Conc, fill=Loc2), data=justREEbs.pdat) +
  facet_wrap(facets=justREEbs.pdat$Loc2) +
  scale_fill_viridis_d(begin=0.1, alpha=0.5, guide="none") +
  scale_y_log10() +
  theme_bw()
