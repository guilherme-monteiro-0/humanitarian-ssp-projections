list.of.packages <- c("data.table","reshape2", "sp","rgdal","rgeos","maptools", "sf", "leaflet")
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
  setTxtProgressBar(pb, i)
  touching = gTouches(land_and_sea[from,], land_and_sea[to,])
  if(touching){
    border_length = tryCatch({
      lines <- rgeos::gIntersection(land_and_sea[from,], land_and_sea[to,], byid = TRUE)
      sp::SpatialLinesLengths(lines)
    }, error = function(e){ return(0) })
  }else{
    border_length <- 0
  }
  tmp = data.frame(
    from_iso3,
    from_name,
    to_iso3,
    to_name,
    touching,
    border_length
  )
  touching_list[[i]] = tmp
}
close(pb)

world_network <- rbindlist(touching_list)
save(world_network, land_and_sea, file="./intermediate_data/world_network.RData")
world_network = subset(world_network, touching)
fwrite(world_network, "./intermediate_data/world_network.csv")
