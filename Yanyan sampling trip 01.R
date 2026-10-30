library(sf)            # essential for maps and spatial analysis in R
library(maptiles)      # for downloading map tile images
library(prettymapr)    # for making north arrow and scale bar
source("../scalebar_use_sf_prettymapr.R")
library(TeachingDemos) # for shadowtext() function (outlined text)
library(viridis)       # for accessible colour palettes
library(stars)
library(terra)
library(viridis)
library(ggplot2)
library(ggspatial)
secretss <- read.csv("../StadiaMaps-API.csv")[1,1]
secretTF <- readLines("../thunderforest-API-key.txt")

# (AraluenExt <- st_as_sf(data.frame(X=c(116.095758, 116.104083), Y=c(-32.129588, -32.123833)),
#                        coords = c("X","Y"), crs=st_crs(4326)))
# AraluenExt <- st_transform(AraluenExt, crs=32750)
AraluenRast <- get_tiles(AraluenExt, provider="Thunderforest.Outdoors",
                         crop=T, zoom=17, apikey=secretTF, forceDownload=TRUE)
# plot_tiles(AraluenRast, axes=T, mar=c(3,3,1,1))

pools <- st_read("CanningPools.shp") |> st_transform(crs=7850)
canningE <- st_read("../shapefiles/canning/CanningRiverLinear.shp") |> 
  st_transform(crs=7850)
canning <- subset(canningE, subset=canningE$NAME!="CANNING RIVER EAST")
stinton <- st_linestring(as.matrix(read.csv("stinton.csv"))) |> st_sfc()
st_crs(stinton) <- st_crs(4326)
stinton <- st_transform(stinton, crs=7850)

ll <- c(-32.124834, 116.101278);st_transform(
  st_as_sf(data.frame(X=ll[2], Y=ll[1]), coords=c("X","Y"),crs=st_crs(4326)),
crs=7850)

AraluenNM <- terra::rast("Araluen_EPSG7850_20250709.jpg")
terra::crs(AraluenNM) <- "epsg:7850"

par(mar=c(3,3,1,1), mgp=c(1.7,0.3,0), tcl=-0.2, font.lab=2, lend="square")
plot(matrix(st_bbox(AraluenNM), nrow=2, byrow=T), asp=1, type="n", 
     xaxs="i", yaxs="i",                      # no space around data range
     xlab="Easting (m)", ylab="Northing (m)") 
terra::plot(AraluenNM, add=TRUE) # add the georeferenced image, and
plot(stinton, add=T, col="#4060f080", lwd=4)
plot(canning, add=T, col="#4060f080", lwd=4)
plot(samples1, add=TRUE, pch=c(21,22,24)[samples1$Type], 
     bg=c("#48F","#f84","gold")[samples1$Type], col="#fff", cex=1.5)
legend("bottomright", legend=levels(samples1$Type), pch=c(21,22,24),
       pt.bg=c("#48F","#f84","gold"))
box()                   # re-draw the box in case of overlap
points(c(415051.4, 415224.6), c(6445053, 6445374), 
       pch=21, col="white", bg="#00c", cex=1.5)
text(compss[,1], compss[,2],labels=1:5)
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

#### ESTIMATE BARE SOIL AREA McNESS BRIDGE ####

# 1. From phone OsmAnd app track
McNessGPX <- st_read("McNess-bare-area.gpx",layer="tracks")
bareTemp <- rbind(st_coordinates(McNessGPX)[,1:2],
                  st_coordinates(McNessGPX)[1,1:2])
baresoil <- st_polygon(list(bareTemp)) |> st_sfc()
st_crs(baresoil) <- st_crs(4326)
baresoil<- st_transform(baresoil, crs=7850)
st_area(baresoil)

# 2. From polygon drawn on NearMap
baresoil2 <- st_read("McNess-bare-area.geojson")
baresoil2 <- st_transform(baresoil2, crs=7850)
st_area(baresoil2)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

samples1 <- read.csv("YZsedsAll.csv")
samples1 <- st_as_sf(samples1, coords=c("Longitude","Latitude"), crs=4326,
                     remove=FALSE)
samples1 <- st_transform(samples1, crs=7850)
# samples1$Type <- factor(samples1$Type, 
#                         levels=c("Pool sediment","Source material","Sediment Sampler"))
(samples1$Type <- factor(samples1$Type, 
                        levels=c("riparian","pool","suspended")))

AraluenExt <- st_as_sf(data.frame(matrix(st_bbox(AraluenNM), nrow=2, byrow=T)),
                       coords=c("X1","X2"), crs=7850)
sitesYY <- st_as_sf(data.frame(Easting=c(415051.4, 415224.6), 
                               Northing=c(6445053, 6445374),
                               SiteID=c("McNess","Stinton")),
                    coords=c("Easting","Northing"), crs=7850, remove = F)
sedtubes <- st_as_sf(data.frame(Easting=c(415068, 415205, 415231, 415007),
                                Northing=c(6445008,6445330,6445388,6445204)),
                     coords=c("Easting","Northing"), crs=7850, remove=FALSE)
set_theme(new=theme_bw())

# map tile version
ggplot() +
  geom_sf(data=AraluenExt, col="#00000000") +
  annotation_spatial(data=AraluenRast) +
  #geom_sf(data=sitesYY, shape=23, col="#000", fill="#FEE", size=3.5) +
  geom_sf_text(aes(label=c("McNess Bridge","Stinton Creek")), 
               data=sitesYY, hjust=0, nudge_x=10, fontface=4) +
  geom_sf(aes(fill=Type, shape=Type, size=Type), data=samples1) +
  geom_sf_label(aes(label=c("Samples material\neroded from bare\nsoil at McNess",
                            "Samples any\nsuspended sediment\nfrom Stinton Ck",
                            "Samples catchment\nabove Stinton\nand road source(s)",
                            "Samples input\ninto upstream\nMcNess pool",
                            "Samples input\nfrom Stinton plus\nroadside erosion")), 
                data=samples1[samples1$Type=="Sediment Sampler",], 
                size=3.5, fill="#ffffffb0", border.colour="#777", 
                hjust=c(0,0.9,0,1,1), nudge_x=c(10,-10,10,-10,-10), 
                nudge_y=c(-15,35,-15,0,0)) +
  geom_sf_text(aes(label=c(1:5)), 
                data=samples1[samples1$Type=="Sediment Sampler",], 
                size=3.5) +
  geom_label(aes(x=415500, y=6445000), fill="#ffffffb0", border.colour="#777",
             label="Sediment sampler\nlocations 4 and 5\nsample similar material") +
  scale_fill_manual(values=c("dodgerblue","coral","#ffffff80")) +
  scale_shape_manual(values=c(25,21,22)) + 
  scale_size_manual(values=c(3.5,3.5,7)) +
  geom_segment(aes(x=414780,y=6445157, xend=414710, yend=6445157), 
               arrow = arrow(length = unit(0.3,"cm")), linewidth=1) +
  geom_point(aes(x=414780,y=6445157), shape=25, fill="dodgerblue", size=3.5) +
  geom_text(aes(x=414775, y=6445137), label="Thompson\n(412645E 6445157N)") +
  annotation_north_arrow(location="tr", 
                         style=north_arrow_fancy_orienteering(text_face=2)) +
  annotation_scale(text_cex=1.2, height = unit(0.35,"cm")) +
  scale_x_continuous(expand=c(0,0), limits=st_coordinates(AraluenExt)[,1]) +
  scale_y_continuous(expand=c(0,0), limits=st_coordinates(AraluenExt)[,2]) +
  labs(x="Easting (m)", y="Northing (m)", 
       caption = "MGA Zone 50 / GDA2020 (EPSG:7850; raster is Thunderforest ‘Outdoors’)") +
  theme(axis.title=element_text(size=12,face="bold"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14, face="bold"),
        plot.caption = element_text(colour="#777", face="italic"),
        legend.position = "inside",
        legend.position.inside = c(0.88,0.1)) +
  coord_sf(datum=7850)

# aerial photo version
ggplot() +
  geom_sf(data=AraluenExt, col="#00000000") +
  annotation_spatial(data=AraluenNM) +
  geom_sf(data=canning, col="#4060f080", linewidth=4) +
  geom_sf(data=stinton, col="#4060f080", linewidth=2) +
  # geom_sf(data=sitesYY, shape=23, col="white", fill="#200", size=4) +
  geom_sf_text(aes(label=SiteID), data=sitesYY, col="white", fontface=2, size=5, 
              hjust=0, nudge_x=10) +
  geom_sf(aes(fill=Type, shape=Type), data=samples1, col="white", size=4) +
  scale_fill_manual(values=c("royalblue","orangered","gold3")) +
  scale_shape_manual(values=c(22,21,24)) + 
  geom_segment(aes(x=414780,y=6445157, xend=414710, yend=6445157), 
               arrow = arrow(length = unit(0.3,"cm")), linewidth=1.2, color="#FFF") +
  geom_point(aes(x=414780,y=6445157), shape=21, color="#FFF", fill="orangered", 
             size=4) +
  geom_text(aes(x=414775, y=6445137), label="Thompson\n(412645E 6445157N)", 
            color="#FFF", fontface=1, size=4.5, hjust=0) +
  annotation_north_arrow(location="tr", 
      style=north_arrow_fancy_orienteering(line_col="#FFF", 
                            text_col="#FFF", text_face=2)) +
  annotation_scale(line_col="#FFF", text_col="#FFF", text_cex=1.2, 
                   height = unit(0.35,"cm")) +
  geom_text(aes(x=415392.6, y=6445312),label="Canning River", 
            color="#BDF", angle=-15, fontface=2) +
  geom_text(aes(x=415190, y=6445585),label="Stinton Creek", 
            color="#BDF", angle=-90, fontface=2) +
  scale_x_continuous(expand=c(0,0), limits=st_coordinates(AraluenExt)[,1]) +
  scale_y_continuous(expand=c(0,0), limits=st_coordinates(AraluenExt)[,2]) +
  labs(x="Easting (m)", y="Northing (m)", 
       caption = "MGA Zone 50 / GDA2020 (EPSG:7850)") +
  theme(axis.title=element_text(size=12,face="bold"),
        legend.background = element_rect(fill="#566262"),
        legend.key = element_rect(fill="#566262"),
        legend.text = element_text(colour="#FFF", size=12),
        legend.title = element_text(colour="#FFF", size=12, face="bold")) +
  coord_sf(datum=7850)
