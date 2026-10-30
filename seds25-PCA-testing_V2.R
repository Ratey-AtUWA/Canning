# load packages
library(viridis) # for colour palettes
library(car)     # for data ellipses

# read the data, check and manage
sed25 <- read.csv("Sediment2025.csv", stringsAsFactors = TRUE)
names(sed25)[43] <- "Fe.phases"
summary(sed25)
(sed25$LocType <- as.factor(paste0(sed25$Sample,".", substr(sed25$Type,1,3))))

# copy the data into another object to do transformations on
s25_clr <- sed25

# centered log-ratio (CLR) transformation on just elemental concentrations
s25_clr[,5:42] <- t(apply(sed25[,5:42], MARGIN = 1,
                          FUN = function(x){log(x) - mean(log(x))}))

# separate CLR transformation on XRD mineral proportions
s25_clr[,43:46] <- t(apply(sed25[,43:46], MARGIN = 1,
                          FUN = function(x){log(x) - mean(log(x))}))
# check CLR transformation
head(s25_clr)

# make another Factor for convenience
s25_clr$LocType3 <- as.character(s25_clr$LocType)
s25_clr$LocType3[grep("Bot",s25_clr$LocType3)] <- NA
s25_clr$LocType3[grep("Mcness.Sus",s25_clr$LocType3)] <- NA
s25_clr$LocType3[grep("Thomp.Sus",s25_clr$LocType3)] <- NA
s25_clr$LocType3 <- droplevels(as.factor(s25_clr$LocType3))

# ---------- elements and XRD ----------------------------------

# run PCA on just the aqua regia and simplified XRD columns
pca2 <- prcomp(s25_clr[,5:46], scale. = TRUE)

# display PCA variable weightings for first 8 principal components
pca2$rot[,1:8] |> round(3)

# display component variances (eigenvalues) 
{cat("...\n\nComponent Variances - CLR-transformed (open) data\n")
  print(round(pca2$sdev^2,3))}

# display proportions of total variance explained by each principal component
{cat("...\n\nProportions of variance explained by each component\n")
  round(pca2$sdev^2/sum(pca2$sdev^2),3)}

# display cumulative proportions of total variance explained by each principal component
{cat("...\n\nCumulative proportions of variance explained by each component\n")
  cumsum(round(pca2$sdev^2/sum(pca2$sdev^2),3))}

# plot the scree plot with eigenvalue = 1 indicated
plot(pca2); abline(h=1, lty=2, col="#8080ff80")
axis(1, at=seq(0.7,11.5,l=10), labels=c(1:10)); mtext("Component",1,1.5,font=2)


# ========= JUST SUSPENDED ELLIPSES AND JUST BOTTOM SED OBSERVATIONS ===========
library(car)
library(viridis)

# this is a large block of code to be run all at once, starting below and ending
# at the comment line `# dev.off()`.
# 💡 If you want to save to a high-quality image, uncomment these lines:
# `# Cairo::CairoPNG("PCA-x4.png", 2400, 2400, dpi=200)`
# and
# `# dev.off()`,
# then run them together with all the code between them!

# Cairo::CairoPNG("PCA-WMO-x4.png", 2400, 2400, dpi=200)

# code repeats 4 times for different combinations of principal components

# set colour palette and plot parameters
palette(c("black", turbo(14, alpha = 0.7),"white"))
par(mfrow=c(1,1), mar = c(3.5,3.5,3.5,3.5), oma = c(0,0,0,0),
    mgp=c(1.7,0.3,0), tcl = 0.25, font.lab=2, cex.lab=1.8, cex.axis=1.6,
    lend = "square", ljoin = "mitre")
layout(matrix(c(1,1,2,2,5,
                1,1,2,2,5,
                3,3,4,4,5,
                3,3,4,4,5), nrow=4, byrow=T))

# choose components and set scaling factor (sf) for first plot
v1 <- 1; v2 <- 2; sf <- 0.8; szs <- c(1.4,1.3,1.2,1.2,1.2)*2

# plot basic biplot but make observations invisible ("transparent")
biplot(pca2, choices = c(v1,v2), col = c("transparent","black"), cex=c(1,1.8),
       pc.biplot = FALSE, arrow.len = 0.08, cex.lab=1.8,
       xlim = c(-0.31,0.31), ylim = c(-0.31,0.31), scale=1,
       xlab = paste0("Scaled PC",v1," Component Loadings"),
       ylab = paste0("Scaled PC",v2," Component Loadings"))
# add top and right axes and plot label
mtext(paste0("Scaled PC",v1," Observation Scores"), 3, 1.6, font = 2)
mtext(paste0("Scaled PC",v2," Observation Scores"), 4, 1.6, font = 2)
mtext("(a)", 3, -1.75, adj=0.05, cex=1.5, family="serif", font = 2)

# add data ellipses for suspended sediments defined by factor `LocType3`
ellipscols <- c(plasma(4, alpha=0.5, end=0.8),plasma(4, alpha=0.25, end=0.8))
dataEllipse(x=pca2$x[,v1]*sf, y=pca2$x[,v2]*sf*1.5,
                 groups = s25_clr$LocType3, add = TRUE, plot.points = FALSE,
                 levels = c(0.75), center.pch = 3, fill=TRUE,
                 col = ellipscols[1:4],
                 lty = 1, lwd = 3,
                 center.cex = 2.4, group.labels = "")

# plot observation scores for just the bottom sediments
filt.bot <- grep(".bot",s25_clr$LocType, ignore.case = T) # gives vector of row numbers
points(pca2$x[filt.bot,v1]*sf, pca2$x[filt.bot,v2]*sf*1.5,
       pch = c(21:25,21:25,21:25,21:25,21)[droplevels(s25_clr$LocType[filt.bot])],
       lwd = 1, bg = c(1:21)[droplevels(s25_clr$LocType[filt.bot])],
       col = 1,
       cex = rep(szs*1.4,6)[droplevels(s25_clr$LocType[filt.bot])])
box()

# PLOT 2: choose components and set scaling factor (sf)
v1 <- 1; v2 <- 3; sf <- 0.7

biplot(pca2, choices = c(v1,v2), col = c("transparent","black"), cex=c(1,1.8),
       pc.biplot = FALSE, arrow.len = 0.08, cex.lab=1.8,
       xlim = c(-0.28,0.28), ylim = c(-0.25,0.25), scale=1,
       xlab = paste0("Scaled PC",v1," Component Loadings"),
       ylab = paste0("Scaled PC",v2," Component Loadings"))
mtext(paste0("Scaled PC",v1," Observation Scores"), 3, 1.6, font = 2)
mtext(paste0("Scaled PC",v2," Observation Scores"), 4, 1.6, font = 2)
mtext("(b)", 3, -1.75, adj=0.05, cex=1.5, family="serif", font = 2)
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
       cex = rep(szs*1.2,6)[droplevels(s25_clr$LocType[filt.bot])])
box()

# PLOT 3: choose components and set scaling factor (sf)
v1 <- 2; v2 <- 3; sf <- 0.9; szs <- c(1.4,1.3,1.2,1.2,1.2)*2

biplot(pca2, choices = c(v1,v2), col = c("transparent","black"), cex=c(1,1.8),
       pc.biplot = FALSE, arrow.len = 0.08, cex.lab=1.8,
       xlim = c(-0.32,0.32), ylim = c(-0.45,0.45), scale=1,
       xlab = paste0("Scaled PC",v1," Component Loadings"),
       ylab = paste0("Scaled PC",v2," Component Loadings"))
mtext(paste0("Scaled PC",v1," Observation Scores"), 3, 1.6, font = 2)
mtext(paste0("Scaled PC",v2," Observation Scores"), 4, 1.6, font = 2)
mtext("(c)", 3, -1.75, adj=0.05, cex=1.5, family="serif", font = 2)
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
       cex = rep(szs*1.2,6)[droplevels(s25_clr$LocType[filt.bot])])
box()

# PLOT 4: choose components and set scaling factor (sf)
v1 <- 1; v2 <- 4; sf <- 0.8; szs <- c(1.4,1.3,1.2,1.2,1.2)*2

biplot(pca2, choices = c(v1,v2), col = c("transparent","black"), cex=c(1,1.8),
       pc.biplot = FALSE, arrow.len = 0.08, cex.lab=1.8,
       xlim = c(-0.3,0.3), ylim = c(-0.2,0.2), scale=1,
       xlab = paste0("Scaled PC",v1," Component Loadings"),
       ylab = paste0("Scaled PC",v2," Component Loadings"))
mtext(paste0("Scaled PC",v1," Observation Scores"), 3, 1.6, font = 2)
mtext(paste0("Scaled PC",v2," Observation Scores"), 4, 1.6, font = 2)
mtext("(d)", 3, -1.75, adj=0.05, cex=1.5, family="serif", font = 2)
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
       cex = rep(szs*1.2,6)[droplevels(s25_clr$LocType[filt.bot])])
box()

plot(0:1,0:1,axes=F,ann=F,bty="n",type="n")
legend("left", bty = "n",
       legend = c(levels(droplevels(s25_clr$LocType[filt.bot])),
                  levels(s25_clr$LocType3)),
       pch = c(21:25,21:25,21:25,rep(21,4)),
       col = c(rep(1,15),ellipscols[1:4]),
       pt.bg = c(1:15,ellipscols[5:8]),
       pt.cex = c(rep(szs,3),rep(6.5,4)), pt.lwd=c(rep(1,15),rep(3,4)),
       cex = 1.8, y.intersp = 1.75)
# dev.off()
layout(1) # reset complex layout back to default

# -=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# PCA with elements AND XRD minerals ####

pca1 <- prcomp(s25_clr[,5:46], scale. = TRUE)
pca1$rot[,1:8] |> round(3)
{cat("...\n\nComponent Variances - CLR-transformed (open) data\n")
  print(round(pca1$sdev^2,3))}
{cat("...\n\nProportions of variance explained by each component\n")
  round(pca1$sdev^2/sum(pca1$sdev^2),3)}

plot(pca1); abline(h=1, lty=2, col="#0000ff40")

palette(c("black",viridis::turbo(20, alpha = 0.7),"white"))
par(mfrow=c(1,1), mar = c(3.5,3.5,3.5,3.5), oma = c(0,0,0,0),
    mgp=c(1.7,0.3,0), tcl = 0.25, font.lab=2, cex.lab=1.5,
    lend = "square", ljoin = "mitre")
layout(matrix(c(1,1,1,1,2), nrow=1))

# choose components and set scaling factor (sf)
v1 <- 1; v2 <- 2; sf <- 0.95; szs <- c(1.4,1.3,1.2,1.2,1.2)*1.5

biplot(pca1, choices = c(v1,v2), col = c("grey92","black"), cex=c(1,1.5),
       pc.biplot = FALSE, arrow.len = 0.08,
       # xlim = c(-0.5,0.4), ylim = c(-0.25,0.6),
       xlab = paste0("Scaled PC",v1," Component Loadings"),
       ylab = paste0("Scaled PC",v2," Component Loadings"))
mtext(paste0("Scaled PC",v1," Observation Scores"), 3, 1.6, font = 2)
mtext(paste0("Scaled PC",v2," Observation Scores"), 4, 1.6, font = 2)
points(pca1$x[,v1]*sf, pca1$x[,v2]*sf*1.5,
       pch = c(21:25,21:25,21:25,21:25,21)[s25_clr$LocType],
       lwd = 1, bg = c(1:21)[s25_clr$LocType],
       col = 1,
       cex = rep(szs*1.5,6)[s25_clr$LocType])
# dataEllipse(x=pca1$x[,v1]*sf, y=pca1$x[,v2]*sf*1.5,
#             groups = s25_clr$LocType, add = TRUE, plot.points = FALSE,
#             levels = c(0.9), center.pch = 3, col = c(2,3,8,9,11),
#             lty = 2, lwd = 1, center.cex = 2.4, group.labels = "")
box()
plot(0:1,0:1,axes=F,ann=F,bty="n",type="n")
legend("right", bty = "n", inset = 0.03,
       legend = levels(s25_clr$LocType),
       pch = c(21:25,21:25,21:25,21:25,21), pt.lwd = 1,
       col = 1, pt.bg = c(1:21),
       pt.cex = rep(szs*1.5,6),
       cex = 1.25, y.intersp = 1.5)
layout(1)
