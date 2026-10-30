sedtubes <- st_as_sf(data.frame(Easting=c( 415050, 415215, 415231, 415007),
                                Northing=c(6445008,6445340,6445388,6445204)),
                                coords=c("Easting","Northing"), crs=7850, remove=F)
firenames <- st_as_sf(data.frame(X=c(415165,415337),Y=c(6445119,6445170),
                                 Name=c("Firebreak 1\n(McNess Upstream)",
                                        "Firebreak 2\n(Canning Upstream)")), 
                      coords=c("X","Y"), crs=7850, remove=F)
firebreak1 <- st_read("firebreak1.kml") |> st_transform(7850)
firebreak2 <- st_read("firebreak2.kml") |> st_transform(7850)
firebreaks <- st_union(firebreak1, firebreak2)
McNessBaresoil <- st_read("McNess-bare-GEarth.kml") |> st_transform(7850)

Cairo::Cairo(width=1523, height=1292, file="sediment-sampler-locations.jpg", 
             type="jpeg", dpi=150)
ggplot() +
  geom_sf(data=AraluenExt, col="#00000000") +
  annotation_spatial(data=AraluenNM) +
  geom_sf(data=canning, col="#4060f080", linewidth=4) +
  geom_sf(data=stinton, col="#4060f080", linewidth=2) +
  geom_sf(fill="#fd0", shape=22, data=sedtubes, col="white", size=4) +
  geom_sf_label(aes(label=c("McNess downstream",
                            "Canning Upstream",
                            "Stinton Creek",
                            "McNess upstream")), 
                data=sedtubes, size=5, fill="#ffffffd0", border.colour="#aaa", 
                hjust=c(0,1,0,1), nudge_x=c(10,-10,10,-10)) +
  geom_sf(data=firebreaks, col="#f0a000a0",linewidth=2) +
  geom_sf(data=McNessBaresoil, col="#fa5", fill="#f0a000a0") +
  geom_sf_label(aes(label=c("Firebreak 1\n(McNess Upstream)",
                            "Firebreak 2\n(Canning Upstream)")), 
             data=firenames, fill="#f0a000a0", border.colour="#fa4", 
             size=4, hjust=0, nudge_x=10) +
  geom_label(aes(x=415000, y=6445050), label="McNess Bridge\nbare soil area",
            border.color="#FA5", fill="#f0a000a0", size=4,, hjust=1) +
  annotation_north_arrow(location="tr", 
                         style=north_arrow_fancy_orienteering(line_col="#FFF", 
                                           text_col="#FFF", text_face=2)) +
  annotation_scale(line_col="#FFF", text_col="#FFF", text_cex=1.2, 
                   height = unit(0.35,"cm")) +
  geom_text(aes(x=415392.6, y=6445312),label="Canning River", 
            color="#DEF", angle=-15, fontface=2) +
  geom_text(aes(x=415190, y=6445585),label="Stinton Creek", 
            color="#DEF", angle=-90, fontface=2) +
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
dev.off()


Cairo::Cairo(width=1523, height=1292, file="sediment-samplers-sources.jpg", 
             type="jpeg", dpi=150)
ggplot() +
  geom_sf(data=AraluenExt, col="#00000000") +
  annotation_spatial(data=AraluenRast) +
  geom_sf(fill="#f80", shape=22, data=sedtubes, col="#000", size=4) +
  geom_sf_label(aes(label=c("McNess downstream",
                            "Canning Upstream",
                            "Stinton Creek",
                            "McNess upstream")), 
                data=sedtubes, size=5, fill="#ffffffd0", border.colour="#aaa", 
                hjust=c(0,1,0,1), nudge_x=c(10,-10,10,-10)) +
  geom_sf(data=firebreaks, col="#f0a000a0",linewidth=2) +
  geom_sf(data=McNessBaresoil, col="#fa5", fill="#f0a000a0") +
  geom_sf_label(aes(label=c("Firebreak 1\n(McNess Upstream)",
                            "Firebreak 2\n(Canning Upstream)")), 
                data=firenames, fill="#f0a000a0", border.colour="#fa4", 
                size=4, hjust=0, nudge_x=10) +
  geom_label(aes(x=415000, y=6445050), label="McNess Bridge\nbare soil area",
             border.color="#FA5", fill="#f0a000a0", size=4,, hjust=1) +
  annotation_north_arrow(location="tr", 
                         style=north_arrow_fancy_orienteering(text_face=2)) +
  annotation_scale(text_cex=1.2, height = unit(0.35,"cm")) +
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
dev.off()

