### load the R packages we need for plotting maps (install first if needed) ####

# install to your device if you don't have them already (ONLY DO ONCE)
if(!require(sf)) install.packages("sf")
if(!require(maptiles)) install.packages("maptiles")
if(!require(prettymapr)) install.packages("prettymapr")
if(!require(TeachingDemos)) install.packages("TeachingDemos")
if(!require(viridis)) install.packages("viridis")

# you will need to load the installed packages each time you start RStudio
library(sf)            # essential for maps and spatial analysis in R
library(maptiles)      # for downloading map tile images
library(prettymapr)    # for making north arrow and scale bar
  source("../scalebar_use_sf_prettymapr.R")
library(TeachingDemos) # for shadowtext() function (outlined text)
library(viridis)       # for accessible colour palettes
library(Cairo)         # exporting high quality images
secretss <- read.csv("../StadiaMaps-API.csv")[1,1]

palette(c("black", viridis(8), "royalblue", 
          "white", "transparent", "firebrick", "grey44")) # custom colours

secrettf <- readLines("../thunderforest-API-key.txt")

# ~~~~~ read some external data for adding info to maps ####

git<-"https://github.com/Ratey-AtUWA/Learn-R-web/raw/refs/heads/main/"

# canning_drains <- st_read(paste0(git, "Town-of-canning-Drains.kml"))
# (canning_drains <- st_transform(canning_drains, 
#                                  crs=st_crs(32750))) # convert to UTM Zone 50S

coast <- st_read("../Ashfield-REE/Coastline_LGATE_070.shp")
drains <- st_read("../shapefiles/DoW_Stormwater_Drains.shp")

lu <- st_read("../shapefiles/zones-reserves/Region_Scheme_Zones_Reserves_DPLH_023.shp")
lu$descriptio <- as.factor(lu$descriptio)
lu_water <- subset(lu, lu$descriptio=="Waterways")

WCdrn <- subset(drains, subset=drains$LGA=="Water Corporation")
WCearth <- subset(WCdrn, subset=WCdrn$PTYPE=="E- normal open earth chan")
SthRivDrns <- subset(WCdrn, subset=WCdrn$COUNCILID=="FORRESTDALE M.D." |
                       WCdrn$COUNCILID=="BAILEYS ROAD B.D." |
                       WCdrn$COUNCILID=="KEANE ROAD B.D.")
# WCforrestdale <- subset(WCdrn, subset=WCdrn$COUNCILID=="FORRESTDALE M.D.")
SthRivDrns <- subset(WCdrn, subset=WCdrn$COUNCILID=="FORRESTDALE M.D." |
                       WCdrn$COUNCILID=="BAILEYS ROAD B.D." |
                       WCdrn$COUNCILID=="KEANE ROAD B.D.")

pools <- read.csv("pools.csv")
canningE <- st_read("../shapefiles/canning/CanningRiverLinear.shp")
canning <- subset(canningE, subset=canningE$NAME!="CANNING RIVER EAST")

canning$focus <- cut(as.numeric(canning$STKEHDRI), 
                     breaks = c(0,246200,999999), 
                     labels=F)
(canLg <- matrix(c(115.83583,-32.4777, 
                   115.83583,-31.99836,
                   116.46801,-31.99836,
                   116.46801,-32.4777,
                   115.83583,-32.4777),ncol=2,byrow=T))
canmat <- matrix(c(115.8402,-32.2424, 
                   115.8402,-31.9923,
                   116.1279,-31.9923,
                   116.1279,-32.2424,
                   115.8402,-32.2424),ncol=2,byrow=T)
kentmat <- matrix(c(115.9203,-32.0613, 
                    115.9203,-32.019,
                    115.978,-32.019,
                    115.978,-32.0613,
                    115.9203,-32.0613),ncol=2,byrow=T)
reachmat <- matrix(c(115.9203,-32.155, 
                    115.9203,-32.015,
                    116.128,-32.015,
                    116.128,-32.155,
                    115.9203,-32.155),ncol=2,byrow=T)
canpoly <- st_polygon(list(canmat)) |> st_sfc(crs=st_crs(4283))
canLgPoly <- st_polygon(list(canLg)) |> st_sfc(crs=st_crs(4283))
kentpoly <- st_polygon(list(kentmat)) |> st_sfc(crs=st_crs(4283))
reachpoly <- st_polygon(list(reachmat)) |> st_sfc(crs=st_crs(4283))
canningW <- st_intersection(canning, canpoly)
kentpool <- st_intersection(canning, kentpoly)
reach <- st_intersection(canning, reachpoly)
cat("Reach length of the Kent St pool is",round(as.numeric(sum(st_length(kentpool))/1000),2),"km\n")
cat("Reach length (Kent St Weir to Canning Dam) is",round(as.numeric(sum(st_length(reach))/1000),2),"km\n")

cannAll <- st_read("../shapefiles/canning/CanningTributaries.shp")

#### ~~~~~~~~~~ Make pools map ~~~~~~~~~~ ####

locality <- st_as_sf(data.frame(x=c(115.8402,116.1567), y=c(-32.2424,-31.9923)),
                     coords=c("x","y"), crs=st_crs(4283)) # NOTE GDA94
locality # check it worked: should include 'Geodetic CRS:  GDA94'

# use the get_tiles() function with options to download map tiles
canningMap <- get_tiles(locality, provider="Stadia.StamenTerrain", 
                        crop=TRUE,
                        apikey=secretss, zoom=13, forceDownload = T)
# canningMap <- get_tiles(locality, provider="Thunderforest.Outdoors", crop=T, apikey=secrettf, zoom=13, forceDownload = T)

# then PLOT THE POOLS MAP, first setting graphics parameters with par() ####

# Cairo(2826, 2700, "Cann3PoolsMap2026.png", dpi=300)
par(mfrow=c(1,1), mar=c(3,3,1,1), mgp=c(1.5,0.3,0), font.lab=2,
    lend="square", ljoin="mitre")
palette(c("black","dodgerblue", "blueviolet", "steelblue4", "lightskyblue4",
          "cyan","#99B3CC","#ffffffa0","#ffffffd0","white",
          "gray48","transparent","#70503040","khaki","red2"))
# first an empty plot frame based on the map extent
plot(st_coordinates(locality), type="n", asp=1.2, xaxs="i", yaxs="i",
     xlab="Longitude (\u00b0E)", ylab="Latitude (\u00b0S)")
plot_tiles(canningMap, add=TRUE)

plot(cannAll[3], add=T, lwd=4, col=10)
plot(cannAll[3], add=T, lwd=2, col=4)
plot(WCearth[2], add=T, col=10, lwd=3)
plot(WCearth[2], add=T, col=15, lwd=2, lty="32")

points(st_coordinates(pools3), pch=21, bg="cyan", cex=1.4)
shadowtext(st_coordinates(pools3), labels=pools3$site_name, pos=c(4,2,4),col="black",bg="white")
box()

# plot(canning[2], add=T, col=7, lwd=3, lty="13")
# plot(kentpool[1], add=T, col=10,lwd=10)
# plot(canningW[2], add=T, col=10, lwd=8)
# plot(canningW[2], add=T, col=2, lwd=4)
# plot(lu_water[1], add=T, col="#99B3CC", border="#99B3CC", lwd=0.25)
# plot(kentpool[1], add=T, col=3,lwd=6,lend="round")
# with(pools, points(Long, Lat, pch=22, bg=3, col=10, cex=1.4, lwd=2))
# with(pools, shadowtext(Long, Lat, labels=Pool, 
#                        pos=c(3,4,2,2,2,2,4,2,1,3),
#                        cex=0.8, col=3, bg=10, r=0.2))
# shadowtext(116.0169, -32.0919, labels="–", cex=1.5, 
#            col=1, bg=10, r=0.06) # Seaforth Weir
# shadowtext(116.0169, -32.0935, col=1, bg=10, cex=0.85, 
#            labels="Seaforth\nWeir", pos=4, r=0.06)
# shadowtext(c(115.920787,116.1279),c(-32.021289,-32.151955), 
#      labels=c("Kent\nStreet\nWeir","Canning\nDam"), 
#      pos=c(3,1), col=1, bg=10, font=2)
# points(c(115.920787,116.1279),c(-32.021289,-32.151955), 
#        pch="\u258C", cex=1.2, lwd=2, bg=1)
shadowtext(115.982,-32.14, labels="Wungong Brook",
           font=3, col=4, bg=10, r=0.07,srt=75)
shadowtext(116.04,-32.188, labels="Wungong Brook",
           pos=1, font=3, col=4, bg=10, r=0.07)

addnortharrow(pos="topright", padin=c(0.73,0.61), text.col=10,
              border=12, cols=c(12,12),lwd=2)
addscalebar(pos="topright", plotepsg=4283, htin=0.15, label.cex = 1.2,lwd=2, 
            padin=c(0.13,0.36), widthhint = 0.2, label.col=10,linecol=9)
addnortharrow(pos="topright", padin=c(0.75,0.6))
addscalebar(pos="topright", plotepsg=4283, htin=0.15, label.cex = 1.2, 
            padin=c(0.15,0.35), widthhint = 0.2)
legend("topright", cex=0.8, bg=8, box.col=12, x.int=0.5, y.int=0.5,
       legend="Map plotted using sf in R; CRS is GDA94 (EPSG:4283). Map tiles are \u2018Stamen Terrain\u2019 from Stadia  ")
# legend("bottomleft", bg=9, box.col=10, , inset=c(0.01,0.01),
#        legend=c("Canning River","Kent Street Weir Pool","Tributaries",
#                 "Water Corporation open drains", "Limits of Study Area",
#                 "Identified pools","Proposed sampling locations"), 
#        col=c(2, 3, 4, 15, 1, 10,1), pch=c(NA,NA,NA,NA,22,22,1), 
#        y.intersp=1.05, pt.bg=c(NA,NA,NA,NA,1,3,NA), pt.lwd=2, 
#        pt.cex=c(NA,NA,NA,NA,1.4,1.4,1.4), 
#        lwd=c(4,6,2,2,NA,NA,NA), lty=c("F1","F1","F1","32",NA,NA,NA))
legend("bottomleft", bg=9, box.col=10, , inset=c(0.01,0.01),
       legend=c("Canning River","Tributaries",
       "Water Corporation open drains"), 
       col=c(2, 4, 15), pch=c(NA,NA,NA), 
       y.intersp=1.05, pt.bg=c(NA,NA,NA), pt.lwd=2, 
       pt.cex=c(NA,NA,NA), 
       lwd=c(4,2,2), lty=c("F1","F1","32"))
box(lwd=1.5)
# possible sampling locations ???? ####
# 
# (samplocs <- read.csv("samples.csv"))
# with(samplocs, points(Lon,Lat,pch=1, col=14, cex=1.4, lwd=5))
# with(samplocs, points(Lon,Lat,pch=1, col=1, cex=1.4, lwd=2))
# with(samplocs, 
#      shadowtext(Lon,Lat,labels=loc, cex=0.7,bg="#F0E68C20", col=1,r=0.2,
#                 pos=c(2,2,4,4,4,2,4,1,2,4,2,2,2,4)))
# 
# Meeting places
# points(c(115.9561,116.0027),c(-32.034349,-32.07085),pch=19,cex=1.4,col=10)
# lines(c(116.0027,115.974),c(-32.07085,-32.0778),lwd=3,col=10)
# lines(c(116.0027,115.974),c(-32.07085,-32.0778))
# lines(c(115.9561,115.9561),c(-32.034349,-32.02201),lwd=3,col=10)
# lines(c(115.9561,115.9561),c(-32.034349,-32.02201),lwd=1)
# points(c(115.9561,116.0027),c(-32.034349,-32.07085),pch=19)
# shadowtext(115.974,-32.0778,labels="Centennial Pioneer\nPark Amphitheatre",
#      pos=2,cex=0.8,font=3, offset=0.1, col=1, bg=10)
# shadowtext(115.9561,-32.02201,labels="SERCUL",
#      pos=3,cex=0.8,font=3, offset=0.15, col=1, bg=10)
dev.off()

a# points(c(116.0011,115.969,115.9752,115.988,116.0217,
#          115.957, 115.9467,116.0203,116.0189,116.0676,
#          116.0179,116.0993),
#        c(-32.0663,-32.1057,-32.0419,-32.038,-32.0301,
#          -32.0328,-32.030371,-32.0777,-32.0785,-32.13456,
#          -32.1165,-32.1278),
#        pch=24, bg=6, col=3, lwd=2)
# shadowtext(c(116.0011,115.969,115.9752,115.988,116.0217,
#        115.957, 115.9467,116.0203,116.0189,116.0676,
#        116.0179,116.0993),
#      c(-32.0663,-32.1057,-32.0419,-32.038,-32.0301,
#        -32.0328,-32.030371,-32.0777,-32.0785,-32.13456,
#        -32.1165,-32.1278),
#      labels=c("Ellis Brook","Wungong Brook Drain","Bickley Brook",
#               "Bickley Brook","Bickley Brook","SERCUL Drain",
#               "Lacey St MD","Ferres trib","Ferres trib","Churchman Brk",
#               "Rundle St MD","McNess Dr Bridge"), pos=c(2,4), cex=0.7,bg=2,r=0.3)
# 
# Ellis Brook from Centennial Pioneer Park (-32.0663,116.0011)? Are pools near Albany highway supposed to remove sediment?
#   Drain into Wungong Brook: Holmes St. near Matison St., Southern River? -32.105658, 115.969006
# Bickley Brook: end of Bridal Cres (-32.0419, 115.9752) or Lakewook Cove (-32.038, 115.988) or White Rd (-32.0301, 116.0217)
# 'SERCUL' Drain NW of Bickley Brook (-32.032780, 115.956975)
# Lacey st MD: access SW end of Woodside Loop? sample at (-32.030371, 115.946737)?
#   'Ferres trib: Mills Rd E (-32.0777, 116.0203) or Cockram Rd (-32.0785, 116.0189)
# Churchman Brook: access from Soldiers Rd (-32.134561, 116.06764)
# Rundle St MD: sample at ~-32.1165, 116.0179, access from Saddlers Rt (-32.1167, 116.0182)
# McNess Dr: there is trib comin in from Araluen, but erodible soil around bridge pool itself

# -~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-
#### add water corporation drains ####
wcd <- st_read("../shapefiles/watercorpDrains/water_corp_drains.shp")
nicholson <- st_intersection(wcd,kentpoly)
plot(WCearth[2],add=T,col="#800000b0",lwd=2)
plot(nicholson[4], add=T, col="orangered",lwd=1)

# -~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-
#### check landuse legend colours ####
uses <- read.csv("land-use-categs.csv")
(uses$Bord <- uses$Colour)
stringr::str_sub(uses$Bord,8,9) <- "d0"
par(mfrow=c(1,1), mar=c(3,3,1,1), mgp=c(1.5,0.3,0), font.lab=2,
    lend="square")
plot(st_coordinates(locality), type="n", asp=1.2, xaxs="i", yaxs="i",
     xlab="Longitude (\u00b0E)", ylab="Latitude (\u00b0S)")
shadowtext(c(116.006,116.015,115.975, 116.108, 116.09, 116.04, 115.937),
           c(-32.038,-32.065,-32.09,-32.097, -32.14, -32.188, -32.1581),
           labels=c("Bickley Brook","Ellis\nBrook","Southern\nRiver",
                    "Stinton\nCreek","Churchman\nBrook","Wungong River",
                    "Forrestdale\nLake"),
           pos=c(1,4,4,1,1,1,1), font=3, col="steelblue4", bg="white", r=0.07)
addnortharrow(pos="topright", padin=c(0.75,0.6))
addscalebar(pos="topright",plotepsg=4283, htin=0.15, label.cex = 1.2, 
            padin=c(0.15,0.35), widthhint = 0.2)
plot(lga[1],add=T, col="#00000000", border="#ff200040", lwd=4)
with(centroids, text(X,Y,labels=name, cex=1.6, font=4, col="#ff200040"))
legend("topright",legend="Map plotted using sf in R; CRS is GDA94 (EPSG:4283)  ", 
       cex=0.75, bg="#ffffffa0", box.col="#00000000", x.int=0.5, y.int=0.5)
legend("top", bg="#ffffffd0", box.col="white", inset=0.02,
       legend=levels(lu$descriptio),
       cex=0.9, y.int=0.9, pch=22, pt.cex=2, 
       pt.bg=uses$Colour, 
       col=uses$Bord)
legend("bottomleft", bg="#ffffffd0", box.col="pink", inset=c(0.005,0.15),
       legend=c("Canning River","Kent Street Weir Pool","Tributaries",
                "Water Corporation open drains", "Limits of Study Area"), 
       col=c("dodgerblue", "blueviolet", "steelblue4", "lightskyblue4", "black"), 
       y.intersp=1.3, pt.bg="gold", pt.lwd=2, pt.cex=1.4, 
       lwd=c(4,6,2,2,NA), lty=c("F1","F1","F1","12",NA), pch=c(NA,NA,NA,NA,22))
legend("bottom", bg="#ffffffd0", box.col="pink", inset=0.002,
       legend=levels(lu$descriptio)[-c(4,7,12:22,32,33)], ncol=3,
       cex=0.95, y.int=0.9, pch=22, pt.cex=2, 
       pt.bg=uses$Colour[-c(4,7,12:22,32,33)], 
       col=uses$Bord[-c(4,7,12:22,32,33)])
