list.of.packages <- c("data.table","reshape2", "sp","rgdal","rgeos","maptools", "sf", "leaflet", "geosphere", "s2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

load(file="./intermediate_data/land_and_sea.RData")
load(file="./intermediate_data/world_network.RData")

nodes = unique(c(unique(world_network$from_iso3), unique(world_network$to_iso3)))
links = world_network[,c("from_iso3", "to_iso3", "mean_distance_km")]
net <- graph_from_data_frame(d=links, vertices=nodes, directed=F) 
link_weights <- E(net)$mean_distance_km

first_order = "ZAR"
second_order = attributes(neighbors(net, which(nodes==first_order)))$names
second_order = setdiff(second_order, first_order)

third_order = c()
for(iso in second_order){
  third_order = c(third_order, attributes(neighbors(net, which(nodes==iso)))$names)
}
third_order = setdiff(third_order, first_order)
third_order = setdiff(third_order, second_order)

fourth_order = c()
for(iso in third_order){
  fourth_order = c(fourth_order, attributes(neighbors(net, which(nodes==iso)))$names)
}
fourth_order = setdiff(fourth_order, third_order)
fourth_order = setdiff(fourth_order, second_order)
fourth_order = setdiff(fourth_order, first_order)
fourth_order = subset(fourth_order, fourth_order!="FRA")

fifth_order = c()
for(iso in fourth_order){
  fifth_order = c(fifth_order, attributes(neighbors(net, which(nodes==iso)))$names)
}
fifth_order = setdiff(fifth_order, fourth_order)
fifth_order = setdiff(fifth_order, third_order)
fifth_order = setdiff(fifth_order, second_order)
fifth_order = setdiff(fifth_order, first_order)
fifth_order = subset(fifth_order, fifth_order!="FRA")


splash_df = data.frame(
  ISO_A3 = c(first_order, second_order, third_order, fourth_order, fifth_order),
  color = c(
    "red",
    rep("orange", length(second_order)),
    rep("yellow", length(third_order)),
    rep("green", length(fourth_order)),
    rep("blue", length(fifth_order))
  )
)

splash_shape = merge(land_and_sea, splash_df, by="ISO_A3")
splash_shape = subset(splash_shape, !is.na(color))
leaflet(data=splash_shape) %>% addTiles() %>% addPolygons(color=splash_shape@data$color, popup=splash_shape@data$NAME_EN)

