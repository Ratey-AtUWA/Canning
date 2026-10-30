library(car)
library(viridis)
library(stringr)
library(TeachingDemos)
library(psych)
library(MASS) # needed for lda function
library(klaR) # needed for stepclass function

sed25 <- read.csv("Sediment2025.csv", stringsAsFactors = TRUE)
names(sed25)[43] <- "Fe.phases"
summary(sed25)
(sed25$LocType <- as.factor(paste0(sed25$Sample,".", substr(sed25$Type,1,3))))

s25_clr <- sed25
s25_clr[,5:42] <- t(apply(sed25[,5:42], MARGIN = 1,
                          FUN = function(x){log(x) - mean(log(x))}))
s25_clr[,43:46] <- t(apply(sed25[,43:46], MARGIN = 1,
                          FUN = function(x){log(x) - mean(log(x))}))
head(s25_clr)
s25_clr$LocType3 <- as.character(s25_clr$LocType)
s25_clr$LocType3[grep("Bot",s25_clr$LocType3)] <- NA
s25_clr$LocType3[grep("Mcness.Sus",s25_clr$LocType3)] <- NA
s25_clr$LocType3[grep("Thomp.Sus",s25_clr$LocType3)] <- NA
s25_clr$LocType3 <- droplevels(as.factor(s25_clr$LocType3))

# ---------- elements and XRD ----------------------------------

pca2 <- prcomp(s25_clr[,5:46], scale. = TRUE)
pca2$rot[,1:8] |> round(3)
{cat("...\n\nComponent Variances - CLR-transformed (open) data\n")
  print(round(pca2$sdev^2,3))}
{cat("...\n\nProportions of variance explained by each component\n")
  round(pca2$sdev^2/sum(pca2$sdev^2),3)}

plot(pca2); abline(h=1, lty=2, col="#8080ff80")
axis(1, at=seq(0.7,11.5,l=10), labels=c(1:10)); mtext("Component",1,1.5,font=2)


# ========= JUST SUSPENDED ELLIPSES AND JUST BOTTOM SED OBSERVATIONS ===========

# Cairo::CairoPNG("PCA-WMO-x4.png", 2400, 2400, dpi=200)
palette(c("black", turbo(14, alpha = 0.7),"white"))
par(mfrow=c(1,1), mar = c(3.5,3.5,3.5,3.5), oma = c(0,0,0,0),
    mgp=c(1.7,0.3,0), tcl = 0.25, font.lab=2, cex.lab=1.2, cex.axis=1.1,
    lend = "square", ljoin = "mitre")
# choose components and set scaling factor (sf)
v1 <- 1; v2 <- 2; sf <- 0.8; szs <- c(1.4,1.3,1.2,1.2,1.2)*2

biplot(pca2, choices = c(v1,v2), col = c("transparent","black"), cex=c(1,1.8),
       pc.biplot = FALSE, arrow.len = 0.08, cex.lab=1.2,
       xlim = c(-0.31,0.31), ylim = c(-0.31,0.31), scale=1,
       xlab = paste0("Scaled PC",v1," Component Loadings"),
       ylab = paste0("Scaled PC",v2," Component Loadings"))
mtext(paste0("Scaled PC",v1," Observation Scores"), 3, 1.4, font = 2)
mtext(paste0("Scaled PC",v2," Observation Scores"), 4, 1.4, font = 2)
mtext("(a)", 3, -1.75, adj=0.05, cex=1.5, family="serif", font = 2)
ellipscols <- c(plasma(4, alpha=0.5, end=0.8),plasma(4, alpha=0.25, end=0.8))
dataEllipse(x=pca2$x[,v1]*sf, y=pca2$x[,v2]*sf*1.5,
                 groups = s25_clr$LocType3, add = TRUE, plot.points = FALSE,
                 levels = c(0.75), center.pch = 3, fill=TRUE,
                 col = ellipscols[1:4],
                 lty = 1, lwd = 3,
                 center.cex = 2.4, group.labels = "")
filt.bot <- grep(".bot",s25_clr$LocType, ignore.case = T)
points(pca2$x[filt.bot,v1]*sf, pca2$x[filt.bot,v2]*sf*1.5,
       pch = c(21:25,21:25,21:25,21:25,21)[droplevels(s25_clr$LocType[filt.bot])],
       lwd = 1, bg = c(1:21)[droplevels(s25_clr$LocType[filt.bot])],
       col = 1,
       cex = rep(szs*1.4,6)[droplevels(s25_clr$LocType[filt.bot])])
box()

# -=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-

s25susp <- droplevels(s25_clr[!is.na(s25_clr$LocType3),])
row.names(s25susp) <- NULL

s25cor <- corTest(s25susp[,5:46])
str(s25cor)
windowsFonts(nar="PT Sans Narrow")
par(mar=c(3,3,1,1), family="nar")
corPlot(s25cor$r, cex=0.4, cex.axis=0.75,las=2)
# -=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-

# LDA from raw variables ####

# 1. all the variables ####

(ldaAll <- lda(LocType3 ~ Al +Ba +Ca +Co +Cr +Cu +Fe +K + Mg +Mn +Na +Ni +P + Pb +
                 Rb +S + Sr +V + Y + Zn +La +Ce +Nd +Sm +Eu +Gd +Tb +Dy +Ho +
                 Er +Yb +Th +As +Se +Mo +Sn +U + Fe.phases+ Quartz +Feldspar +
                 Clays, data=s25susp))
(ldaAll_CV <- lda(LocType3 ~ Al +Ba +Ca +Co +Cr +Cu +Fe +K + Mg +Mn +Na +Ni +P + Pb +
                    Rb +S + Sr +V + Y + Zn +La +Ce +Pr +Nd +Sm +Eu +Gd +Tb +Dy +Ho +
                    Er +Yb +Th +As +Se +Mo +Sn +U + Fe.phases+ Quartz +Feldspar +
                    Clays, data=s25susp, CV=TRUE))

# 2. just ICP-OES elements (omit most REEs and all XRD minerals) ####

(ldaOES <- lda(LocType3 ~ Al +Ba +Ca +Co +Cr +Cu +Fe +K + Mg +Mn +Na +Ni +P + Pb +
                 Rb +S + Sr +V + Y + Zn +La +Ce +Nd +Gd +Th +As +Se +Mo +Sn,
               data=s25susp))
(ldaOES_CV <- lda(LocType3 ~ Al +Ba +Ca +Co +Cr +Cu +Fe +K +Mg +Mn +Na +Ni +P +Pb +
                    Rb +S + Sr +V +Y +Zn +La +Ce +Nd +Gd +Th +As +Se +Mo +Sn, 
                  data=s25susp, CV=TRUE))

s25cor2 <- corTest(s25susp[,c("Al","Ba","Ca","Co","Cr","Cu","Fe","K","Mg","Mn",
                              "Na","Ni","P","Pb","Rb","S","Sr","V","Y","Zn",
                              "La","Ce","Nd","Gd","Th","As","Se","Mo","Sn")])
corPlot(s25cor2$r, cex=0.4, cex.axis=0.75,las=1,diag=F)

# 3. as for 2 but one of each collinear pair removed ####

(ldaNoCorr <- lda(LocType3 ~ Al +Ba +Ca +Co +Cr +Cu +Fe +K + Mg +Mn +Na +Ni +P + Pb +
                 Rb +S + Sr +V + Y + Zn +La +Nd +Th +As +Sn,
               data=s25susp))
ldaPredNoCorr <- predict(ldaNoCorr)
(ldaNoCorr_CV <- lda(LocType3 ~ Al +Ba +Ca +Co +Cr +Cu +Fe +K +Mg +Mn +Na +Ni +P +Pb +
                    Rb +S + Sr +V +Y +Zn +La +Nd +Th +As +Sn, 
                  data=s25susp, CV=TRUE))

s25cor3 <- corTest(s25susp[,c("Al","Ba","Ca","Co","Cr","Cu","Fe","K","Mg","Mn",
                              "Na","Ni","P","Pb","Rb","S","Sr","V","Y","Zn",
                              "La","Nd","Th","As","Sn")])
corPlot(s25cor3$r, cex=0.5, cex.axis=0.75,las=1,diag=F)

par(mfrow=c(2,2), mar=c(3,3,1,1), mgp=c(1.6,0.3,0), family="nar", 
    tcl=0.25, cex=1.2, font.lab=2)

plot(ldaPredNoCorr$x[,1:2],col="#f8f8f8", cex=3,lwd=2,xlim=c(-21,12),
     xlab="LD1", ylab="LD2")
text(ldaNoCorr$scaling[,1:2], labels=row.names(ldaNoCorr$scaling), font=2, col="#00000080")
points(ldaPredNoCorr$x[,1:2],bg=c("#080","#088","#08f","#00f")[s25susp$LocType3], 
       pch=c(21:24)[s25susp$LocType3],cex=1.4)

plot(ldaPredNoCorr$x[,c(1,3)],col="#f8f8f8", cex=3,lwd=2,xlim=c(-21,12),ylim=c(-7,5),
     xlab="LD1", ylab="LD3")
text(ldaNoCorr$scaling[,c(1,3)], labels=row.names(ldaNoCorr$scaling), font=2, col="#00000080")
points(ldaPredNoCorr$x[,1:3],bg=c("#080","#088","#08f","#00f")[s25susp$LocType3], 
       pch=c(21:24)[s25susp$LocType3],cex=1.4)

plot(ldaPredNoCorr$x[,c(2,3)],col="#f8f8f8", cex=3,lwd=2,ylim=c(-7,5),
     xlab="LD2", ylab="LD3")
text(ldaNoCorr$scaling[,c(2,3)], labels=row.names(ldaNoCorr$scaling), font=2, col="#00000080")
points(ldaPredNoCorr$x[,2:3],bg=c("#080","#088","#08f","#00f")[s25susp$LocType3], 
       pch=c(21:24)[s25susp$LocType3], cex=1.4)

plot(0:1,0:1, type="n",axes=F,ann=F)
legend("topleft", bty="n", cex=1.4, pt.cex=1.5, legend=levels(s25susp$LocType3),
       pch=c(21:24), pt.bg=c("#080","#088","#08f","#00f"))

# -=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-

# add PCs to dataframe
s25_clr <- cbind(s25_clr, pca2$x[,1:12])

(lda1 <- lda(LocType3 ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12, 
            data=s25_clr))
lda1.pred <- predict(lda1)

par(mfrow = c(1,1), mar = c(3.5,3.5,1,1), mgp = c(1.5,0.3,0), tcl = 0.25,
    lend = "square", ljoin = "mitre", cex.main = 0.9, font.lab=2)
palette(c("black",c("#104E8B80", "#A0522D80", "#CD000080", "#A020F080", "#36648B80", "#8B5A2B80", 
                    "#7D26CD80", "#CD4F3980"),"white"))
plot(lda1$scaling[,1], lda1$scaling[,2], 
     type="n", xlim = c(-1.8,1.6),  ylim = c(-0.4,1.4), 
     xlab="Linear Discriminant [1]", ylab="Linear Discriminant [2]", 
     main="Variable Coefficients [LD1, LD2]")
abline(v=0,col="grey",lty=2); abline(h=0,col="grey",lty=2)
for(i in 1:NROW(lda1$scaling)){
  arrows(0,0,lda1$scaling[i,1],lda1$scaling[i,2],
         length = 0.08, col = "grey60") }
text(lda1$scaling[,1]*1.1, lda1$scaling[,2]*1.1, 
     labels=row.names(lda1$scaling[,1:2]),
     cex = 0.9, col = 1, pos=c(1,2,3,4), offset = 0.01)
mtext("(a)", 3, -1.5, adj = 0.05, cex = 1.2, font = 2)
clrz <- matrix(rainbow(20,v=0.8,end=0.8),ncol=10,byrow = T)
attributes(clrz) <- NULL
legend("topright", ncol = 2, legend=levels(s25_clr$LocType3), 
       col=c(2:9), pch=c(0,1,2,5,15,16,17,18), pt.lwd = 2,
       bty="n", box.col="grey90", y.intersp = 1, 
       title="\nPotential\nSource",
       box.lwd=2, inset=0.02, pt.cex=1.5, cex=1.1)

# ----------- stepwise LDA on Principal Components -----------

# based on just the suspended sediment samples 
# (i.e. having non-NA s25_clr$LocType3)

# run it multiple times to get greatest accuracy score

steplda2 <- stepclass(s25_clr[!is.na(s25_clr[,48]),49:60], 
                  grouping=s25_clr[!is.na(s25_clr[,48]),48], 
                  method="lda", criterion="AC", improvement=0.005) # AC=accuracy
print(steplda2)
(lda2 <- lda(LocType3 ~ PC2+PC3+PC5+PC8+PC9+PC10+PC12, 
             data=s25_clr[!is.na(s25_clr[,48]), ]))
ldapred <- predict(lda2)
trainChk <- data.frame(Actual=as.character(s25_clr[!is.na(s25_clr[,48]), 48]),
      Predicted=as.character(ldapred$class))
trainChk$is.Correct <- trainChk[,1]==trainChk[,2]
print(trainChk)

# predict bottom sediments from lda model for suspended sediments
ldapred2 <- predict(lda2, newdata=s25_clr[is.na(s25_clr[,48]),49:60])

postLDA2 <- data.frame(BotSed=as.character(s25_clr[is.na(s25_clr[,48]),47]),
                       round(ldapred2$posterior,3))
row.names(postLDA2) <- paste0(row.names(postLDA2),"_",postLDA2[,1])
(postLDA2 <- postLDA2[,2:5])

# bar plot showing posterior probabilities for each bottom sample being
# classified as one of the suspended
palette(c("black", mako(4, begin=0.2, end=0.85, alpha=0.85), "white"))
par(mfrow=c(1,1), mar=c(3,5,1,1))
barplot(t(postLDA2), horiz=T, las=1, col=c(2:5),
        names.arg=substr(row.names(postLDA2),
                         str_locate(row.names(postLDA2),"_")+1,
                         nchar(row.names(postLDA2))-4),
        xlab="Posterior probabilities", xlim=c(0,1), xaxs="i")
mtext("Sample", 2, 4, font=2) ; box()
legend("bottomright", inset=0.02, cex=1.2, bg="#ffffffd0", box.col="#000000b0", 
       legend=gsub(".Sus","",levels(s25_clr$LocType3)),
       pch=22, pt.bg=c(2:5), pt.cex=2.5, 
       title="\nPossible\nSource", title.font=3, title.col=barcolz[1])

# basic 3d scatter
library(scatterplot3d)
palette(c("black", mako(4, begin=0.2, end=0.85, alpha=0.85), "white"))
scatterplot3d(ldapred$x[,2], ldapred$x[,1], ldapred$x[,3], scale.y=0.5, 
              bg = c(2:5)[s25_clr[!is.na(s25_clr[,48]),48]],
              pch=c(21:24)[s25_clr[!is.na(s25_clr[,48]),48]],
              type="h", lty.hplot=3, cex.symbols = 2,
              xlab="LD1", ylab="LD2", zlab="LD3")
legend("topright", ncol = 1, 
       legend=toupper(substr(levels(s25_clr$LocType3),1,
                             nchar(levels(s25_clr$LocType3))-4)), 
       pt.bg=c(2:5), pch=c(21:24), pt.lwd = 1, bg="#ffffffb0",
       box.col="lightgrey", y.intersp = 1.5, pt.cex=2, cex=1.25, 
       title="\nPotential\nsource sediment")

# ggplot + plotly 3d scatter
library(ggplot2)
library(plotly)
data <- data.frame(x=ldapred$x[,1], y=ldapred$x[,2], z=ldapred$x[,3], 
                      LocType3=s25_clr[!is.na(s25_clr[,48]),48])
symz <- c("circle", "square", "diamond", "x")
# Convert the ggplot object to a plotly object for 3D plotting
lda3d <- plot_ly(data, x = ~x, y = ~y, z = ~z, color = ~LocType3, 
                 colors=mako(4, begin=0.2, end=0.75), symbol = ~LocType3, 
             type = "scatter3d", mode = "markers", symbols = symz,
             marker = list(size = 5)) |> 
  layout(title = "LDA following PCA",
         scene = list(xaxis = list(title = "LD 1"),
                      yaxis = list(title = "LD 2"),
                      zaxis = list(title = "LD 3")))
lda3d # Display the 3D scatter plot

#  =============== LDA "biplot" 2D scatter -=-=-=-=-=-=-=-=-

par(mfrow=c(2,2), mar=c(4,4,4,4), mgp=c(1.6,0.3,0), tcl=0.25, font.lab=2, cex.lab=1.25)
# ------------------ plot 1 -=-=-=-=-=-=-=-=-
v1 <- 1; v2 <- 2
plot(lda2$scaling[,v1], lda2$scaling[,v2], 
     type="n", xlim = c(-1.2,1.5),  ylim = c(-1.1,1.2), 
    # main="Variable Coefficients [LD1, LD2]", 
     xlab=paste("Variable Weighting, Linear Discriminant",v1), 
     ylab=paste("Variable Weighting, Linear Discriminant",v2))
abline(v=0,col="grey",lty=3); abline(h=0,col="grey",lty=3)
for(i in 1:NROW(lda2$scaling)){
  arrows(0,0,lda2$scaling[i,v1],lda2$scaling[i,v2],
         length = 0.08, col = "#00000060", lwd=2) }
tf <- 1.04 ; of <- 0.333
axis(3, at=seq(-3,3)*of, labels=seq(-3,3))
mtext("Observation scores LD1", line=1.5, font=2)
axis(4, at=seq(-3,3)*of, labels=seq(-3,3))
mtext("Observation scores LD2", side=4, line=1.5, font=2)
shadowtext(lda2$scaling[,v1]*tf, lda2$scaling[,v2]*tf, 
     labels=row.names(lda2$scaling),
     cex = 1.2, col = 1, bg="#ffffff80", pos=c(3,2,1,1,4), offset = 0.012)
points(ldapred$x[,v1]*of, ldapred$x[,v2]*of, lwd=1, 
       cex=c(1.8,1.5,1.5,1.5)[s25_clr[!is.na(s25_clr[,48]),48]], 
       pch=c(21:24)[s25_clr[!is.na(s25_clr[,48]),48]],
       bg=c(2,3,4,5)[s25_clr[!is.na(s25_clr[,48]),48]])

# ------------------ plot 2 -=-=-=-=-=-=-=-=-
v1 <- 1; v2 <- 3
plot(lda2$scaling[,v1], lda2$scaling[,v2], 
     type="n", xlim = c(-1.2,1.5),  ylim = c(-0.8,0.8), 
     # main="Variable Coefficients [LD1, LD2]", 
     xlab=paste("Variable Weighting, Linear Discriminant",v1), 
     ylab=paste("Variable Weighting, Linear Discriminant",v2))
abline(v=0,col="grey",lty=3); abline(h=0,col="grey",lty=3)
for(i in 1:NROW(lda2$scaling)){
  arrows(0,0,lda2$scaling[i,v1],lda2$scaling[i,v2],
         length = 0.08, col = "#00000060", lwd=2) }
tf <- 1.04 ; of <- 0.333
axis(3, at=seq(-3,3)*of, labels=seq(-3,3))
mtext(paste0("Observation scores LD",v1), line=1.5, font=2)
axis(4, at=seq(-3,3)*of, labels=seq(-3,3))
mtext(paste0("Observation scores LD",v2), side=4, line=1.5, font=2)
shadowtext(lda2$scaling[,v1]*tf, lda2$scaling[,v2]*tf, 
     labels=row.names(lda2$scaling),
     cex = 1.2, col = 1, bg="#ffffff80", pos=c(3,2,1,1,4), offset = 0.012)
points(ldapred$x[,v1]*of, ldapred$x[,v2]*of, lwd=1, 
       cex=c(1.8,1.5,1.5,1.5)[s25_clr[!is.na(s25_clr[,48]),48]], 
       pch=c(21:24)[s25_clr[!is.na(s25_clr[,48]),48]],
       bg=c(2,3,4,5)[s25_clr[!is.na(s25_clr[,48]),48]])

# ------------------ plot 3 -=-=-=-=-=-=-=-=-
v1 <- 2; v2 <- 3
plot(lda2$scaling[,v1], lda2$scaling[,v2], 
     type="n", xlim = c(-1.1,1.2),  ylim = c(-0.8,0.8), 
     # main="Variable Coefficients [LD1, LD2]", 
     xlab=paste("Variable Weighting, Linear Discriminant",v1), 
     ylab=paste("Variable Weighting, Linear Discriminant",v2))
abline(v=0,col="grey",lty=3); abline(h=0,col="grey",lty=3)
for(i in 1:NROW(lda2$scaling)){
  arrows(0,0,lda2$scaling[i,v1],lda2$scaling[i,v2],
         length = 0.08, col = "#00000060", lwd=2) }
tf <- 1.04 ; of <- 0.333
axis(3, at=seq(-3,3)*of, labels=seq(-3,3))
mtext(paste0("Observation scores LD",v1), line=1.5, font=2)
axis(4, at=seq(-3,3)*of, labels=seq(-3,3))
mtext(paste0("Observation scores LD",v2), side=4, line=1.5, font=2)
shadowtext(lda2$scaling[,v1]*tf, lda2$scaling[,v2]*tf, 
     labels=row.names(lda2$scaling),
     cex = 1.2, col = 1, bg="#ffffff80", pos=c(3,2,1,1,4), offset = 0.012)
points(ldapred$x[,v1]*of, ldapred$x[,v2]*of, lwd=1, 
       cex=c(1.8,1.5,1.5,1.5)[s25_clr[!is.na(s25_clr[,48]),48]], 
       pch=c(21:24)[s25_clr[!is.na(s25_clr[,48]),48]],
       bg=c(2,3,4,5)[s25_clr[!is.na(s25_clr[,48]),48]])

# ------------------ legend -=-=-=-=-=-=-=-=-
plot(0:1, 0:1, type="n", ann=F, bty="n", axes=F)
legend("left", ncol = 1, 
       legend=toupper(substr(levels(s25_clr$LocType3),1,
                             nchar(levels(s25_clr$LocType3))-4)), 
       pt.bg=c(2:5), pch=c(21:24), pt.lwd = 1,
       box.col="lightgrey", y.intersp = 1.5, pt.cex=c(2.3,2,2,2), cex=1.5, 
       title="\nPotential\n source sediment ")

# ----------- stepwise LDA on Measured Variables -----------

# based on just the suspended sediment samples 
# (i.e. having non-NA s25_clr$LocType3)

# run it multiple times to get greatest accuracy score
library(klaR)
lda0step <- stepclass(s25_clr[!is.na(s25_clr[,48]),5:46], 
                  grouping=s25_clr[!is.na(s25_clr[,48]),48], 
                  method="lda", criterion="AC", improvement=0.001) # AC=accuracy
(lda0 <- lda(LocType3 ~ Pb + U + Er + Mo + La, 
            data=s25_clr[!is.na(s25_clr[,48]), ])) # accuracy: 0.99993
ldapred0 <- predict(lda0)
trainChk <- data.frame(Actual=as.character(s25_clr[!is.na(s25_clr[,48]), 48]),
                       Predicted=as.character(ldapred0$class))
trainChk$is.Correct <- trainChk[,1]==trainChk[,2]
print(trainChk)

# predict bottom sediments from lda model for suspended sediments
ldapred02 <- predict(lda0, newdata=s25_clr[is.na(s25_clr[,48]),5:46])

postLDA0 <- data.frame(BotSed=as.character(s25_clr[is.na(s25_clr[,48]),47]),
                       round(ldapred02$posterior,3))
row.names(postLDA0) <- paste0(row.names(postLDA0),"_",postLDA0[,1])
(postLDA0 <- postLDA0[,2:5])

# bar plot showing posterior probabilities for each bottom sample being
# classified as one of the suspended
library(viridis); library(stringr)
palette(c("black", mako(4, begin=0.2, end=0.85, alpha=0.85), "white"))
par(mfrow=c(1,1), mar=c(3,5,1,1), mgp=c(1.7,0.2,0), tcl=0.25, font.lab=2)
barplot(t(postLDA0), horiz=T, las=1, col=c(2:5),
        names.arg=substr(row.names(postLDA0),
                         str_locate(row.names(postLDA0),"_")+1,
                         nchar(row.names(postLDA0))-4),
        xlab="Posterior probabilities", xlim=c(0,1), xaxs="i")
mtext("Sample", 2, 3.5, font=2, cex=1.2) ; box()
legend("topright", inset=0.05, cex=1.2, bg="#ffffffd0", box.col="#000000b0", 
       legend=gsub(".Sus","",levels(s25_clr$LocType3)),
       pch=22, pt.bg=c(2:5), pt.cex=2.5, 
       title="\nPossible\nSource", title.font=3, title.col=2)



# basic 3d scatter
library(scatterplot3d)
palette(c("black", mako(4, begin=0.2, end=0.85, alpha=0.85), "white"))
scatterplot3d(ldapred0$x[,2], ldapred0$x[,1], ldapred0$x[,3], scale.y=0.5, 
              bg = c(2:5)[s25_clr[!is.na(s25_clr[,48]),48]],
              pch=c(21:24)[s25_clr[!is.na(s25_clr[,48]),48]],
              type="h", lty.hplot=3, cex.symbols = 2,
              xlab="LD1", ylab="LD2", zlab="LD3")
legend("topright", inset=c(0.05,0.15), ncol = 1, 
       legend=toupper(substr(levels(s25_clr$LocType3),1,
                             nchar(levels(s25_clr$LocType3))-4)), 
       pt.bg=c(2:5), pch=c(21:24), pt.lwd = 1, bg="#ffffffb0",
       box.col="lightgrey", y.intersp = 1.5, pt.cex=2, cex=1.25, 
       title="\nPotential\nsource sediment")

# ggplot + plotly 3d scatter
library(ggplot2)
library(plotly)
data <- data.frame(x=ldapred0$x[,1], y=ldapred0$x[,2], z=ldapred0$x[,3], 
                   LocType3=s25_clr[!is.na(s25_clr[,48]),48])
symz <- c("circle", "square", "diamond", "x")
# Convert the ggplot object to a plotly object for 3D plotting
lda3d <- plot_ly(data, x = ~x, y = ~y, z = ~z, color = ~LocType3, 
                 colors=mako(4, begin=0.2, end=0.75), symbol = ~LocType3, 
                 type = "scatter3d", mode = "markers", symbols = symz,
                 marker = list(size = 5)) |> 
  layout(title = "LDA following PCA",
         scene = list(xaxis = list(title = "LD 1"),
                      yaxis = list(title = "LD 2"),
                      zaxis = list(title = "LD 3")))
lda3d # Display the 3D scatter plot

#  =============== LDA "biplot" 2D scatter -=-=-=-=-=-=-=-=-

library(TeachingDemos)
par(mfrow=c(2,2), mar=c(4,4,4,4), mgp=c(1.6,0.3,0), tcl=0.25, font.lab=2, 
    cex.lab=1.25)
ef <- 1.1
# ------------------ plot 1 -=-=-=-=-=-=-=-=-
v1 <- 1; v2 <- 2
plot(lda0$scaling[,v1]*ef, lda0$scaling[,v2]*ef, 
     type="n", xlim = c(-25,30), ylim = c(-15,15), 
     # main="Variable Coefficients [LD1, LD2]", 
     xlab=paste("Variable Weighting, Linear Discriminant",v1), 
     ylab=paste("Variable Weighting, Linear Discriminant",v2))
abline(v=0,col="grey",lty=3); abline(h=0,col="grey",lty=3)
for(i in 1:NROW(lda0$scaling)){
  arrows(0,0,lda0$scaling[i,v1],lda0$scaling[i,v2],
         length = 0.08, col = "#00000060", lwd=2) }
tf <- 1.04 ; of <- 3
axis(3, at=seq(-10,10,5)*of, labels=seq(-10,10,5))
mtext("Observation scores LD1", line=1.5, font=2)
axis(4, at=seq(-10,10,5)*of, labels=seq(-10,10,5))
mtext("Observation scores LD2", side=4, line=1.5, font=2)
shadowtext(lda0$scaling[,v1]*tf, lda0$scaling[,v2]*tf, 
           labels=row.names(lda0$scaling),
           cex = 1.2, col = 1, bg="#ffffff80", pos=c(3,2,1,1,4), offset = 0.012)
points(ldapred0$x[,v1]*of, ldapred0$x[,v2]*of, lwd=1, 
       cex=c(1.8,1.5,1.5,1.5)[s25_clr[!is.na(s25_clr[,48]),48]], 
       pch=c(21:24)[s25_clr[!is.na(s25_clr[,48]),48]],
       bg=c(2,3,4,5)[s25_clr[!is.na(s25_clr[,48]),48]])

# ------------------ plot 2 -=-=-=-=-=-=-=-=-
v1 <- 1; v2 <- 3
plot(lda0$scaling[,v1]*ef, lda0$scaling[,v2]*ef, 
     type="n", xlim = c(-30,30), # ylim = c(-0.8,0.8), 
     # main="Variable Coefficients [LD1, LD2]", 
     xlab=paste("Variable Weighting, Linear Discriminant",v1), 
     ylab=paste("Variable Weighting, Linear Discriminant",v2))
abline(v=0,col="grey",lty=3); abline(h=0,col="grey",lty=3)
for(i in 1:NROW(lda0$scaling)){
  arrows(0,0,lda0$scaling[i,v1],lda0$scaling[i,v2],
         length = 0.08, col = "#00000060", lwd=2) }
tf <- 1.04 ; of <- 3
axis(3, at=seq(-10,10,5)*of, labels=seq(-10,10,5))
mtext(paste0("Observation scores LD",v1), line=1.5, font=2)
axis(4, at=seq(-10,10,5)*of, labels=seq(-10,10,5))
mtext(paste0("Observation scores LD",v2), side=4, line=1.5, font=2)
shadowtext(lda0$scaling[,v1]*tf, lda0$scaling[,v2]*tf, 
           labels=row.names(lda0$scaling),
           cex = 1.2, col = 1, bg="#ffffff80", pos=c(3,2,1,1,4), offset = 0.012)
points(ldapred0$x[,v1]*of, ldapred0$x[,v2]*of, lwd=1, 
       cex=c(1.8,1.5,1.5,1.5)[s25_clr[!is.na(s25_clr[,48]),48]], 
       pch=c(21:24)[s25_clr[!is.na(s25_clr[,48]),48]],
       bg=c(2,3,4,5)[s25_clr[!is.na(s25_clr[,48]),48]])

# ------------------ plot 3 -=-=-=-=-=-=-=-=-
v1 <- 2; v2 <- 3
plot(lda0$scaling[,v1]*ef, lda0$scaling[,v2]*ef, 
     type="n", xlim = c(-15,15),  ylim = c(-10,7), 
     # main="Variable Coefficients [LD1, LD2]", 
     xlab=paste("Variable Weighting, Linear Discriminant",v1), 
     ylab=paste("Variable Weighting, Linear Discriminant",v2))
abline(v=0,col="grey",lty=3); abline(h=0,col="grey",lty=3)
for(i in 1:NROW(lda0$scaling)){
  arrows(0,0,lda0$scaling[i,v1],lda0$scaling[i,v2],
         length = 0.08, col = "#00000060", lwd=2) }
tf <- 1.04 ; of <- 3
axis(3, at=seq(-10,10,5)*of, labels=seq(-10,10,5))
mtext(paste0("Observation scores LD",v1), line=1.5, font=2)
axis(4, at=seq(-10,10,5)*of, labels=seq(-10,10,5))
mtext(paste0("Observation scores LD",v2), side=4, line=1.5, font=2)
shadowtext(lda0$scaling[,v1]*tf, lda0$scaling[,v2]*tf, 
           labels=row.names(lda0$scaling),
           cex = 1.2, col = 1, bg="#ffffff80", pos=c(3,2,1,1,4), offset = 0.012)
points(ldapred0$x[,v1]*of, ldapred0$x[,v2]*of, lwd=1, 
       cex=c(1.8,1.5,1.5,1.5)[s25_clr[!is.na(s25_clr[,48]),48]], 
       pch=c(21:24)[s25_clr[!is.na(s25_clr[,48]),48]],
       bg=c(2,3,4,5)[s25_clr[!is.na(s25_clr[,48]),48]])

# ------------------ legend -=-=-=-=-=-=-=-=-
plot(0:1, 0:1, type="n", ann=F, bty="n", axes=F)
legend("left", ncol = 1, 
       legend=toupper(substr(levels(s25_clr$LocType3),1,
                             nchar(levels(s25_clr$LocType3))-4)), 
       pt.bg=c(2:5), pch=c(21:24), pt.lwd = 1,
       box.col="lightgrey", y.intersp = 1.5, pt.cex=c(2.3,2,2,2), cex=1.5, 
       title="\nPotential\n source sediment ")

# comparisons

sed25$LocType3 <- s25_clr$LocType3
par(mfrow=c(2,3), mar=c(3,3,1,1), mgp=c(1.6,0.3,0), tcl=0.25, font.lab=2, 
    cex.lab=1.4)
palette(c("black", mako(4, begin=0.2, end=0.85, alpha=0.7), "white"))
with(sed25[!is.na(sed25[,48]),],
     boxplot(Pb ~ LocType3, col=2:5, cex.axis=1.2,
             xlab="Potential source sediment", ylab="Pb (mg/kg)",
             names=toupper(substr(levels(sed25$LocType3),1,
                                  nchar(levels(sed25$LocType3))-4))))
with(sed25[!is.na(sed25[,48]),],
     boxplot(U ~ LocType3, col=2:5, cex.axis=1.2,
             xlab="Potential source sediment", ylab="U (mg/kg)",
             names=toupper(substr(levels(sed25$LocType3),1,
                                  nchar(levels(sed25$LocType3))-4))))
with(sed25[!is.na(sed25[,48]),],
     boxplot(Er ~ LocType3, col=2:5, cex.axis=1.2,
             xlab="Potential source sediment", ylab="Er (mg/kg)",
             names=toupper(substr(levels(sed25$LocType3),1,
                                  nchar(levels(sed25$LocType3))-4))))
with(sed25[!is.na(sed25[,48]),],
     boxplot(Mo ~ LocType3, col=2:5, cex.axis=1.2,
             xlab="Potential source sediment", ylab="Mo (mg/kg)",
             names=toupper(substr(levels(sed25$LocType3),1,
                                  nchar(levels(sed25$LocType3))-4))))
with(sed25[!is.na(sed25[,48]),],
     boxplot(La ~ LocType3, col=2:5, cex.axis=1.2,
             xlab="Potential source sediment", ylab="La (mg/kg)",
             names=toupper(substr(levels(sed25$LocType3),1,
                                  nchar(levels(sed25$LocType3))-4))))
with(sed25[!is.na(sed25[,48]),],
     boxplot(log10(Zn) ~ LocType3, col=2:5, cex.axis=1.2, yaxt="n",
             xlab="Potential source sediment", ylab="Zn (mg/kg)",
             names=toupper(substr(levels(sed25$LocType3),1,
                                  nchar(levels(sed25$LocType3))-4))))
with(sed25[!is.na(sed25[,48]),],
     stripchart(log10(Zn) ~ LocType3, add=TRUE, method="jitter", col="#00000060", 
                cex=1.4, pch=15, vertical=TRUE))
axis(2, at=log10(c(1,2,5,10,20,50,100,200,500,1000,2000,5000)),
     labels = c(1,2,5,10,20,50,100,200,500,1000,2000,5000))
abline(h=log10(410), lty=2, col="firebrick")

# comparison tests
with(sed25[!is.na(sed25[,48]),], shapiro.test(Pb)) # 0.07
with(sed25[!is.na(sed25[,48]),], oneway.test(Pb ~ LocType3)) # 0.0062 **

with(sed25[!is.na(sed25[,48]),], shapiro.test(U)) # 0.39
with(sed25[!is.na(sed25[,48]),], oneway.test(U ~ LocType3)) # 0.00042 ***

with(sed25[!is.na(sed25[,48]),], shapiro.test(Er)) # 0.0005
with(sed25[!is.na(sed25[,48]),], kruskal.test(Er ~ LocType3)) # 0.049 *

with(sed25[!is.na(sed25[,48]),], shapiro.test(Mo)) # 0.061
with(sed25[!is.na(sed25[,48]),], oneway.test(Mo ~ LocType3)) # 0.22 NS

with(sed25[!is.na(sed25[,48]),], shapiro.test(La)) # 0.18
with(sed25[!is.na(sed25[,48]),], oneway.test(La ~ LocType3)) # 0.0042 **

with(sed25[!is.na(sed25[,48]),], shapiro.test(Zn)) # 8.6E-06
with(sed25[!is.na(sed25[,48]),], kruskal.test(Zn ~ LocType3)) # 0.69 NS

par(mfrow=c(2,3), mar=c(3,3,1,1), mgp=c(1.6,0.3,0), tcl=0.25, font.lab=2, 
    cex.lab=1.4)
with(sed25, boxplot(Cr~Type, log="y", col=c("#A0522D40","#4682B440")))
with(sed25, stripchart(Cr~Type, add=T, method="jitter", pch=15, log="y", cex=1.5, 
                       vertical=T, col=c("#A0522D80","#4682B480")))
abline(h=c(80,370), lwd=c(1,2), col=c("#CDAD0080","#B2222280"))
with(sed25, boxplot(Cu~Type, log="y", col=c("#A0522D40","#4682B440")))
with(sed25, stripchart(Cu~Type, add=T, method="jitter", pch=15, log="y", cex=1.5, 
                       vertical=T, col=c("#A0522D80","#4682B480")))
abline(h=c(65,270), lwd=c(1,2), col=c("#CDAD0080","#B2222280"))
with(sed25, boxplot(Pb~Type, log="y", col=c("#A0522D40","#4682B440")))
with(sed25, stripchart(Pb~Type, add=T, method="jitter", pch=15, log="y", cex=1.5, 
                       vertical=T, col=c("#A0522D80","#4682B480")))
abline(h=c(50,220), lwd=c(1,2), col=c("#CDAD0080","#B2222280"))
with(sed25, boxplot(Ni~Type, log="y", col=c("#A0522D40","#4682B440")))
with(sed25, stripchart(Ni~Type, add=T, method="jitter", pch=15, log="y", cex=1.5, 
                       vertical=T, col=c("#A0522D80","#4682B480")))
abline(h=c(21,52), lwd=c(1,2), col=c("#CDAD0080","#B2222280"))
with(sed25, boxplot(Zn~Type, log="y", col=c("#A0522D40","#4682B440")))
with(sed25, stripchart(Zn~Type, add=T, method="jitter", pch=15, log="y", cex=1.5, 
                       vertical=T, col=c("#A0522D80","#4682B480")))
abline(h=c(200,410), lwd=c(1,2), col=c("#CDAD0080","#B2222280"))
plot(0:1, 0:1, type="n", ann=F, bty="n", axes=F)
legend("center", box.col="lightgrey", legend=c("GV-high ","DGV"), lwd=c(2,1), 
       col=c("#B2222280","#CDAD0080"), cex=1.5, 
       title="Guidelines", title.font=2)

