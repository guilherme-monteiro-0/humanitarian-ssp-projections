list.of.packages <- c("data.table","reshape2", "sp","rgdal","rgeos","maptools", "sf", "leaflet", "geosphere", "s2")
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
keep = c("WB_A3", "WB_NAME")
world_spdf@data = world_spdf@data[,keep]
names(world_spdf@data) = c("ISO_A3", "NAME_EN")
world_spdf@data$ISO_A3[which(world_spdf@data$NAME_EN=="Kosovo")] = "XXK"
world_spdf@data$ISO_A3[which(world_spdf@data$NAME_EN=="Guantanamo Bay (US)")] = "USA1"
world_spdf@data$ISO_A3[which(world_spdf@data$NAME_EN=="Navassa Island (US)")] = "USA2"
world_spdf@data$ISO_A3[which(world_spdf@data$NAME_EN=="Clipperton Island (Fr.)")] = "FRA1"
# Some issues still remain with overseas territories, but disaggregating here leads to ~4000 polys

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
world_oceans_and_seas = water_polygons[1] %>% st_as_sf()
water_polygons = water_polygons[c(2,4:7)] # Drop small sliver
ocean_grid = st_make_grid(world_oceans_and_seas, n = 20) %>% st_as_sf()
ocean_list = list()
pb = txtProgressBar(max=nrow(ocean_grid), style=3)
for(i in 1:nrow(ocean_grid)){
  setTxtProgressBar(pb, i)
  ocean_diff = st_difference(ocean_grid[i,], world_oceans_and_seas, dimension = "polygon")
  ocean_list[[i]] = ocean_diff
}
close(pb)
split_ocean <- do.call(rbind, ocean_list)
split_ocean_poly = split_ocean %>% as_Spatial()
split_ocean_poly = disaggregate(split_ocean_poly)
split_ocean_spdf = SpatialPolygonsDataFrame(split_ocean_poly, data = data.frame(row.names = 1:length(split_ocean_poly)), match.ID = FALSE)
split_ocean_spdf@data$ISO_A3 = paste0("WB",sprintf("%03d", 1:nrow(split_ocean_spdf)))
split_ocean_spdf@data$NAME_EN = paste0("Water body ",c(1:nrow(split_ocean_spdf)))

# Convert the resulting SpatialPolygons object to a SpatialPolygonsDataFrame
water_polygons_spdf <- SpatialPolygonsDataFrame(water_polygons, data = data.frame(row.names = 1:length(water_polygons)), match.ID = FALSE)
water_polygons_spdf@data$ISO_A3 = c(
  paste0("WB",sprintf("%03d", nrow(split_ocean_spdf) + 1)),
  "DT1",
  "DT2",
  "DT3",
  "DT4"
)
water_polygons_spdf@data$NAME_EN = c(
  "Caspian sea",
  "Sudanese disputed territory",
  "Aksai Chin 1",
  "Aksai Chin 2",
  "Arunachal Pradesh"
  )

water_polygons_spdf = rbind(split_ocean_spdf, water_polygons_spdf)

land_and_sea = rbind(world_spdf, water_polygons_spdf)
leaflet(data=land_and_sea) %>% addTiles() %>% addPolygons(color="red", popup=land_and_sea@data$NAME_EN)
save(land_and_sea, file="./intermediate_data/land_and_sea.RData")
rm(list=ls())
gc()
####
load(file="./intermediate_data/land_and_sea.RData")

centroid_list = list()

intersection_list = list()
intersection_index = 1
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
    tmp = data.frame(
      from_iso3,
      from_name,
      to_iso3,
      to_name,
      mean_distance_km
    )
    intersection_list[[intersection_index]] = tmp
    intersection_index = intersection_index + 1
  }
}
close(pb)

world_network <- rbindlist(intersection_list)
save(world_network, file="./intermediate_data/world_network.RData")
fwrite(world_network, "./intermediate_data/world_network.csv")

library(igraph)
nodes = unique(c(unique(world_network$from_iso3), unique(world_network$to_iso3)))
exclude = nodes[which(startsWith(nodes, "WB"))]
world_network = subset(world_network, !(from_iso3  %in% exclude) & !(to_iso3  %in% exclude))
nodes = unique(c(unique(world_network$from_iso3), unique(world_network$to_iso3)))
links = world_network[,c("from_iso3", "to_iso3", "mean_distance_km")]
net <- graph_from_data_frame(d=links, vertices=nodes, directed=F) 
net <- simplify(net, remove.multiple = F, remove.loops = T)
link_weights <- (1 / E(net)$mean_distance_km) * 100
l <- layout_with_fr(net, weights = link_weights)
plot(net, layout=l, vertex.shape="none", vertex.label=nodes, vertex.label.font=2, 
     vertex.label.cex=.6, edge.color="gray70", edge.width = 2)

load(file="./intermediate_data/world_network.RData")

nodes = unique(c(unique(world_network$from_name), unique(world_network$to_name)))
links = world_network[,c("from_name", "to_name", "mean_distance_km")]
net <- graph_from_data_frame(d=links, vertices=nodes, directed=F) 
link_weights <- E(net)$mean_distance_km

shortest_paths(
  net,
  from=which(nodes=="Libya"),
  to=which(nodes=="Italy"),
  weights=link_weights
)$vpath

shortest_paths(
  net,
  from=which(nodes=="South Africa"),
  to=which(nodes=="United Kingdom"),
  weights=link_weights
)$vpath
