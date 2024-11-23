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
library(TeachingDemos) # for shadowtext() function (outlined text)
library(viridis)       # for accessible colour palettes
secretss <- read.csv("../StadiaMaps-API.csv")[1,1]

palette(c("black", viridis(8), "royalblue", 
          "white", "transparent", "firebrick", "grey44")) # custom colours

secret <- readLines("../thunderforest-API-key.txt")

# ~~~~~ read some external data for adding info to maps ####

git<-"https://github.com/Ratey-AtUWA/Learn-R-web/raw/refs/heads/main/"

# canning_drains <- st_read(paste0(git, "Town-of-canning-Drains.kml"))
# (canning_drains <- st_transform(canning_drains, 
#                                  crs=st_crs(32750))) # convert to UTM Zone 50S

coast <- st_read("../Ashfield-REE/Coastline_LGATE_070.shp")
drains <- st_read("../shapefiles/DoW_Stormwater_Drains.shp")
lu <- st_read("../shapefiles/zones-reserves/Region_Scheme_Zones_Reserves_DPLH_023.shp")
lu$descriptio <- as.factor(lu$descriptio)

WCdrn <- subset(drains, subset=drains$LGA=="Water Corporation")
WCearth <- subset(WCdrn, subset=WCdrn$PTYPE=="E- normal open earth chan")
SthRivDrns <- subset(WCdrn, subset=WCdrn$COUNCILID=="FORRESTDALE M.D." |
                       WCdrn$COUNCILID=="BAILEYS ROAD B.D." |
                       WCdrn$COUNCILID=="KEANE ROAD B.D.")
# WCforrestdale <- subset(WCdrn, subset=WCdrn$COUNCILID=="FORRESTDALE M.D.")
SthRivDrns <- subset(WCdrn, subset=WCdrn$COUNCILID=="FORRESTDALE M.D." |
                       WCdrn$COUNCILID=="BAILEYS ROAD B.D." |
                       WCdrn$COUNCILID=="KEANE ROAD B.D.")
# baileys <- subset(WCdrn, subset=WCdrn$COUNCILID=="BAILEYS ROAD B.D.")
# denny <- subset(WCdrn, subset=WCdrn$COUNCILID=="DENNY AVENUE B.D.")
# lacey <- subset(WCdrn, subset=WCdrn$COUNCILID=="LACEY STREET M.D.")
# keane <- subset(WCdrn, subset=WCdrn$COUNCILID=="KEANE ROAD B.D."|
#                   WCdrn$COUNCILID=="KEANE STREET B.D.")
# plot(WCforrestdale[2],add=T,col="gold2",lwd=2)
# plot(baileys[2],add=T,col="orangered",lwd=2)
# plot(denny[2],add=T,col="darkorchid",lwd=2)
# plot(lacey[2],add=T,col="gold2",lwd=2)
# plot(keane[2],add=T,col="orangered",lwd=2)
# plot(SthRivDrns[2],add=T,col="#a8b8d8", lty=3, lwd=2)

# -~-~-~-~-~-~-~- NEXT LINE VERY SLOW!! -~-~-~-~-~-~-~-
# shl <- st_read("../shapefiles/SurfaceHydrology/SurfaceHydrologyLinesNational.gdb")
# shlPer <- subset(shl, shl$PERENNIALITY=="Perennial"|shl$PERENNIALITY=="Perennial")
# canningE <- shl[grep("CANNING RIVER", shl$NAME),]
canningE <- st_read("../shapefiles/canning/CanningRiverLinear.shp")
canning <- subset(canningE, subset=canningE$NAME!="CANNING RIVER EAST")
# st_write(canning,"CanningRiverLinear.shp")
canning$focus <- cut(as.numeric(canning$STKEHDRID), 
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
kentmat <- matrix(c(115.9203,-32.0375, 
                    115.9203,-32.019,
                    115.9364,-32.019,
                    115.9364,-32.0375,
                   115.9203,-32.0375),ncol=2,byrow=T)
canpoly <- st_polygon(list(canmat)) |> st_sfc(crs=st_crs(4283))
canLgPoly <- st_polygon(list(canLg)) |> st_sfc(crs=st_crs(4283))
kentpoly <- st_polygon(list(kentmat)) |> st_sfc(crs=st_crs(4283))
canningW <- st_intersection(canning, canpoly)
kentpool <- st_intersection(canning, kentpoly)
# -~-~-~-~-~-~-~- NEXT LINE VERY SLOW!! -~-~-~-~-~-~-~-
# cannAll <- st_intersection(shl, canLgPoly)
# st_write(cannAll, dsn="CanningTributaries.shp")
cannAll <- st_read("../shapefiles/canning/CanningTributaries.shp")

#### ~~~~~~~~~~ LOCALITY MAP ~~~~~~~~~~ ####

locality <- st_as_sf(data.frame(x=c(115.8402,116.1567), y=c(-32.2424,-31.9923)),
                     coords=c("x","y"), crs=st_crs(4283)) # NOTE GDA94
locality # check it worked: should include 'Geodetic CRS:  GDA94'

# use the get_tiles() function with options to download map tiles
canningMap <- get_tiles(locality, provider="Stadia.Stamen.Terrain", crop=TRUE,
                        apikey=secretss, zoom=14, forceDownload = T)

uses <- read.csv("land-use-categs.csv")
uses$Bord <- uses$Colour
stringr::str_sub(uses$Bord,8,9) <- "d0"

# then plot the map, first setting graphics parameters with par()
par(mfrow=c(1,1), mar=c(3,3,1,1), mgp=c(1.5,0.3,0), font.lab=2,
    lend="square")
palette(c("black","dodgerblue", "blueviolet", "steelblue4", "lightskyblue4",
          "gold","#99B3CC","#ffffffa0","#ffffffd0","white",
          "gray48","transparent"))
# first an empty plot frame based on the map extent
plot(st_coordinates(locality), type="n", asp=1.2, xaxs="i", yaxs="i",
     xlab="Longitude (\u00b0E)", ylab="Latitude (\u00b0S)")
plot_tiles(canningMap, add=TRUE)

plot(lu[2], add=T, pal=uses$Colour, border=uses$Bord)

plot(cannAll[3], add=T, lwd=4, col=10)
plot(cannAll[3], add=T, lwd=2, col=4)
plot(WCearth[2],add=T,col=10,lwd=3)
plot(WCearth[2],add=T,col=5,lwd=1, lty="23")
plot(canning[2], add=T, col=7, lwd=3, lty="13")
plot(canningW[2], add=T, col=10, lwd=8)
plot(canningW[2], add=T, col=2, lwd=4)
plot(kentpool[1],add=T, col=10,lwd=10,lend="round")
plot(kentpool[1],add=T, col=3,lwd=6,lend="round")
shadowtext(c(115.920787,116.1279),c(-32.021289,-32.151955), 
     labels=c("Kent\nStreet\nWeir","Canning\nDam"), 
     pos=c(3,1), col=1, bg=10, font=2)
points(c(115.920787,116.1279),c(-32.021289,-32.151955), pch=22,
       cex=1.4, lwd=2, bg=6)
shadowtext(c(116.006,116.015,115.975, 116.108, 116.09, 116.04, 115.922),
     c(-32.038,-32.065,-32.09,-32.097, -32.14, -32.188, -32.148),
     labels=c("Bickley Brook","Ellis\nBrook","Southern\nRiver",
              "Stinton\nCreek","Churchman\nBrook","Wungong Brook",
              "Forrestdale\nLake"),
     pos=c(1,4,4,1,1,1,1), font=3, col=4, bg=10, r=0.07)
shadowtext(c(115.98,116.065), c(-32.02,-32.11), col=11, bg=10, cex=0.85, 
           labels=c("Yule Brook","Stoney Brook"), font=3, srt=35, r=0.06)
addnortharrow(pos="topright", padin=c(0.72,0.62), text.col=10,
              border=12, cols=c(12,12),lwd=2)
addscalebar(pos="topright", plotepsg=4283, htin=0.15, label.cex = 1.2,lwd=2, 
            padin=c(0.13,0.36), widthhint = 0.2, label.col=10,linecol=9)
addnortharrow(pos="topright", padin=c(0.75,0.6))
addscalebar(pos="topright", plotepsg=4283, htin=0.15, label.cex = 1.2, 
            padin=c(0.15,0.35), widthhint = 0.2)
legend("topright",legend="Map plotted using GDA94 (EPSG:4283)  ", cex=0.8,
       bg=8, box.col=12, x.int=0.5, y.int=0.5)
legend("bottomleft", bg=9, box.col=10, , inset=c(0.005,0.15),
       legend=c("Canning River","Kent Street Weir Pool","Tributaries",
                "Water Corporation open drains", "Limits of Study Area"), 
       col=c(2, 3, 4, 5, 1), 
       y.intersp=1.15, pt.bg=6, pt.lwd=2, pt.cex=1.4, 
       lwd=c(4,6,2,1,NA), lty=c("F1","F1","F1","23",NA), pch=c(NA,NA,NA,NA,22))
legend("bottom", bg=9, box.col=10, inset=0.002,
       legend=levels(lu$descriptio)[-c(4,7,12:22,32,33)], ncol=3,
       cex=0.95, y.int=0.9, pch=22, pt.cex=2, 
       pt.bg=uses$Colour[-c(4,7,12:22,32,33)], 
       col=uses$Bord[-c(4,7,12:22,32,33)])
box() # redraw plot frame

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
            padin=c(0.15,0.35), widthhint = 0.21)
legend("topright",legend="Map plotted using GDA94 (EPSG:4283)  ", cex=0.8,
       bg="#ffffffa0", box.col="#00000000", x.int=0.5, y.int=0.5)
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
