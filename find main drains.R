# source("canning pools map.R") # just in case, to load packages!

wcd <- st_read("../shapefiles/watercorpDrains/water_corp_drains.shp")
(sadmat <- matrix(c(116.0081,116.0081,116.0243,116.0243,116.0081,
                    -32.13871,-32.11291,-32.11291,-32.13871,-32.13871),
                  ncol=2, byrow = F))
sadpoly <- st_polygon(list(sadmat)) |> st_sfc(crs=st_crs(4283))
saddrn <- st_intersection(wcd,sadpoly)
saddrn <- subset(saddrn, saddrn$DRN_CLASSI=="Open Drain")
(rundlrect <- st_as_sf(data.frame(X=sadmat[,1],Y=sadmat[,2]), 
                      coords=c("X","Y"),
                      crs=st_crs(4283)))
rundlmap <- get_tiles(rundlrect, provider="Stadia.Stamen.Terrain",
                      crop=TRUE,
                      apikey=secretss, zoom=15, forceDownload = T)
# rundlmap <- get_tiles(rundlrect, provider="CartoDB.Voyager", crop=TRUE, zoom=15, forceDownload = T)
plot(st_coordinates(rundlrect), type="n", asp=1.2, xaxs="i", yaxs="i",
                    xlab="Longitude (\u00b0E)", ylab="Latitude (\u00b0S)")
plot_tiles(rundlmap,add=T)
plot(saddrn[4], add=T, col="magenta",lwd=2)

(nicmat <- matrix(c(115.9419,-32.03041,
                    115.9419,-32.02231,
                    115.9509,-32.02231,
                    115.9509,-32.03041,
                    115.9419,-32.03041),ncol=2,byrow=T))
(nicpoly <- st_polygon(list(nicmat)) |> st_sfc(crs=st_crs(4283)))
nicholson <- st_intersection(wcd,nicpoly)
nicholson <- subset(nicholson, nicholson$DRN_CLASSI=="Open Drain")

(loc <- locator(2))
McNmat <- matrix(c(loc$x[1],loc$x[1],loc$x[2],loc$x[2],loc$x[1],
                   loc$y[1],loc$y[2],loc$y[2],loc$y[1],loc$y[1]),
                 ncol=2, byrow=F)
(McNpoly <- st_polygon(list(McNmat)) |> st_sfc(crs=st_crs(4283)))
McNtrib <- st_intersection(cannAll,McNpoly)

(McNrect <- st_as_sf(data.frame(X=McNmat[,1],Y=McNmat[,2]), 
                       coords=c("X","Y"),
                       crs=st_crs(4283)))
McNmap <- get_tiles(McNrect, provider="Stadia.Stamen.Terrain",
                      crop=TRUE,
                      apikey=secretss, zoom=15, forceDownload = T)
# McNmap <- get_tiles(McNrect, provider="CartoDB.Voyager", crop=TRUE, zoom=15, forceDownload = T)
plot(st_coordinates(McNrect), type="n", asp=1.2, xaxs="i", yaxs="i",
     xlab="Longitude (\u00b0E)", ylab="Latitude (\u00b0S)")
plot_tiles(McNmap,add=T)
plot(McNtrib[4], add=T, col="magenta",lwd=2)

(loc <- locator(2))
Yulemat <- matrix(c(loc$x[1],loc$x[1],loc$x[2],loc$x[2],loc$x[1],
                   loc$y[1],loc$y[2],loc$y[2],loc$y[1],loc$y[1]),
                 ncol=2, byrow=F)
(Yulepoly <- st_polygon(list(Yulemat)) |> st_sfc(crs=st_crs(4283)))
Yuledrain <- st_intersection(wcd,Yulepoly)
Yuledrain <- subset(Yuledrain, Yuledrain$DRN_CLASSI=="Open Drain")

(Yulerect <- st_as_sf(data.frame(X=Yulemat[,1],Y=Yulemat[,2]), 
                     coords=c("X","Y"),
                     crs=st_crs(4283)))
Yulemap <- get_tiles(Yulerect, provider="Stadia.Stamen.Terrain",
                    crop=TRUE,
                    apikey=secretss, zoom=15, forceDownload = T)
# Yulemap <- get_tiles(Yulerect, provider="CartoDB.Voyager", crop=TRUE, zoom=15, forceDownload = T)
plot(st_coordinates(Yulerect), type="n", asp=1.2, xaxs="i", yaxs="i",
     xlab="Longitude (\u00b0E)", ylab="Latitude (\u00b0S)")
plot_tiles(Yulemap,add=T)
plot(Yuledrain[4], add=T, col="magenta",lwd=2)
