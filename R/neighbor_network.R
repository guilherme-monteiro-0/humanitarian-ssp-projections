list.of.packages <- c("data.table","reshape2", "sp","rgdal","rgeos","maptools", "sf", "leaflet", "geosphere")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

# Set the path to the shapefile
shapefile_path <- "./WB_countries_Admin0_10m/WB_countries_Admin0_10m.shp"

# Read the shapefile as a SpatialPolygonsDataFrame
world_spdf <- readOGR(dsn = shapefile_path)
keep = c("ISO_A3", "NAME_EN")
world_spdf@data = world_spdf@data[,keep]
world_spdf@data$ISO_A3[which(world_spdf@data$NAME_EN=="Kosovo")] = "XXK"

# Create a polygon representing the entire world
world_polygon <- SpatialPolygons(list(Polygons(list(Polygon(cbind(c(-180, -180, 180, 180), c(-90, 90, 90, -90)))), "world")))

# Set the proj4 string of the world polygon to match the shapefile's proj4 string
proj4_string <- proj4string(world_spdf)
proj4string(world_polygon) <- proj4_string

# Reproject the world polygon to match the shapefile's coordinate reference system
world_polygon <- spTransform(world_polygon, CRS(proj4_string))

# Use gDifference from the rgeos package to cut the world polygon by the countries
water_bodies <- gDifference(world_polygon, world_spdf)

# Separate different non-contiguous bodies of water into different polygons
water_polygons <- disaggregate(water_bodies)
water_polygons = water_polygons[c(1:2,4:7)] # Drop small sliver

# Convert the resulting SpatialPolygons object to a SpatialPolygonsDataFrame
water_polygons_spdf <- SpatialPolygonsDataFrame(water_polygons, data = data.frame(row.names = 1:length(water_polygons)), match.ID = FALSE)
water_polygons_spdf@data$ISO_A3 = c(
  "WB1",
  "WB2",
  "DT1",
  "DT2",
  "DT3",
  "DT4"
)
water_polygons_spdf@data$NAME_EN = c(
  "World oceans and seas",
  "Caspian sea",
  "Sudanese disputed territory",
  "Aksai Chin 1",
  "Aksai Chin 2",
  "Arunachal Pradesh"
  )

land_and_sea = rbind(world_spdf, water_polygons_spdf)
leaflet(data=land_and_sea) %>% addTiles() %>% addPolygons(color="red", popup=land_and_sea@data$NAME_EN)

centroid_list = list()

touching_list = list()
combinations = combn(c(1:nrow(land_and_sea)), 2)

pb = txtProgressBar(max=ncol(combinations), style=3)
for(i in 1:ncol(combinations)){
  from = combinations[1,i]
  to = combinations[2,i]
  from_iso3 = land_and_sea@data[from,"ISO_A3"]
  to_iso3 = land_and_sea@data[to,"ISO_A3"]
  from_name = land_and_sea@data[from,"NAME_EN"]
  to_name = land_and_sea@data[to,"NAME_EN"]
  from_poly = land_and_sea[from,]
  to_poly = land_and_sea[to,]
  if(as.character(from) %in% names(centroid_list)){
    from_centroid = centroid_list[[as.character(from)]]
  }else{
    from_centroid = gCentroid(from_poly)
    centroid_list[[as.character(from)]] = from_centroid
  }
  if(as.character(to) %in% names(centroid_list)){
    to_centroid = centroid_list[[as.character(to)]]
  }else{
    to_centroid = gCentroid(to_poly)
    centroid_list[[as.character(to)]] = to_centroid
  }
  setTxtProgressBar(pb, i)
  intersecting = gIntersects(from_poly, to_poly)
  mean_distance_km = distm(from_centroid, to_centroid)[1,1] / 1000
  if(intersecting){
    border_length_km = tryCatch({
      intersection <- rgeos::gIntersection(from_poly, to_poly, byid = TRUE)
      if(class(intersection) == "SpatialCollections"){
        lines = intersection@lineobj
      }
      if(class(intersection) == "SpatialLines"){
        lines = intersection
      }
      if(class(intersection) == "SpatialPoints"){
        0
      }else{
        sum(sp::SpatialLinesLengths(lines))
      }
    }, error = function(e){ return(NA) })
  }else{
    border_length_km <- 0
  }
  tmp = data.frame(
    from_iso3,
    from_name,
    to_iso3,
    to_name,
    intersecting,
    border_length_km,
    mean_distance_km
  )
  touching_list[[i]] = tmp
}
close(pb)

world_network <- rbindlist(touching_list)
save(world_network, file="./intermediate_data/world_network.RData")
fwrite(world_network, "./intermediate_data/world_network.csv")

library(igraph)
exclude = c(
  "-99"
  ,"WB1"
  # ,"WB2"
  # ,"DT1"
  # ,"DT2"
  # ,"DT3"
  # ,"DT4"
  )
world_network = subset(world_network, !(from_iso3  %in% exclude) & !(to_iso3  %in% exclude))
nodes = unique(c(unique(world_network$from_iso3), unique(world_network$to_iso3)))
links = world_network[,c("from_iso3", "to_iso3", "mean_distance_km", "border_length_km", "intersecting")]
net <- graph_from_data_frame(d=links, vertices=nodes, directed=F) 
net <- simplify(net, remove.multiple = F, remove.loops = T)
link_weights <- (1 / E(net)$mean_distance_km) * 100
edge_weights = sqrt(E(net)$border_length_km) / 10
edge_weights[which(E(net)$intersecting & edge_weights < 2)] = 2
edge_weights[which(edge_weights > 5)] = 5
l <- layout_with_fr(net, weights = link_weights)
plot(net, layout=l, vertex.shape="none", vertex.label=nodes, vertex.label.font=2, 
     vertex.label.cex=.6, edge.color="gray70", edge.width = edge_weights)

