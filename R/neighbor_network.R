list.of.packages <- c("data.table","reshape2", "sp","rgdal","rgeos","maptools", "sf", "leaflet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

# Set the path to the shapefile
shapefile_path <- "ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"

# Read the shapefile as a SpatialPolygonsDataFrame
world_spdf <- readOGR(dsn = shapefile_path)
keep = c("ISO_A3", "NAME_EN")
world_spdf@data = world_spdf@data[,keep]

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
water_polygons = water_polygons[1:2] # Drop small Egypt slivers

# Convert the resulting SpatialPolygons object to a SpatialPolygonsDataFrame
water_polygons_spdf <- SpatialPolygonsDataFrame(water_polygons, data = data.frame(row.names = 1:length(water_polygons)), match.ID = FALSE)
water_polygons_spdf@data$ISO_A3 = paste0("WB",c(1:nrow(water_polygons_spdf@data)))
water_polygons_spdf@data$NAME_EN = c("World oceans and seas", "Caspian sea")

land_and_sea = rbind(world_spdf, water_polygons_spdf)

touching_list = list()
combinations = combn(c(1:nrow(land_and_sea)), 2)

pb = txtProgressBar(max=ncol(combinations), style=3)
for(i in 1:ncol(combinations)){
  from = combinations[1,i]
  to = combinations[2,i]
  setTxtProgressBar(pb, i)
  touching = gTouches(land_and_sea[from,], land_and_sea[to,])
  if(touching){
    border_length = tryCatch({
      lines <- rgeos::gIntersection(land_and_sea[from,], land_and_sea[to,], byid = TRUE)
      return(sp::SpatialLinesLengths(lines))
    }, error = function(e){ return(0) })
  }else{
    border_length <- 0
  }
  tmp = data.frame(
    from = from,
    to = to,
    touching,
    border_length
  )
  touching_list[[i]] = tmp
}
close(pb)

world_network <- rbindlist(touching_list)
save(world_network, land_and_sea, file="./intermediate_data/world_network.RData")
