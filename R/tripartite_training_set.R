list.of.packages <- c("data.table", "reshape2", "igraph", "dplyr", "foreach", "doSNOW","snow", "doParallel",
                      "sp","rgdal","rgeos","maptools", "sf", "leaflet", "geosphere", "s2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

load(file="./intermediate_data/land_and_sea.RData")

centroid_list = list()
for(iso3 in land_and_sea$ISO_A3){
  poly = land_and_sea[which(land_and_sea$ISO_A3==iso3),]
  centroid = gCentroid(poly)
  centroid_df = data.frame(lon=centroid$x, lat=centroid$y, iso3=iso3)
  centroid_list[[iso3]] = centroid_df
}
centroids = rbindlist(centroid_list)
centroids$iso3[which(centroids$iso3=="ZAR")] = "COD"
centroids$iso3[which(centroids$iso3=="ROM")] = "ROU"
centroids$iso3[which(centroids$iso3=="TMP")] = "TLS"

displacement = fread("~/git/saint/outputs/regression_displacement_worldclim_forecast.csv")
displacement$displaced_persons[which(displacement$year>=2023)] = displacement$y_hat[which(displacement$year>=2023)]
displacement = subset(displacement, scenario=="ssp1")
keep = c(
  "displaced_persons",
  "iso3",
  "year"
)
displacement = displacement[,keep, with=F]
training_set = merge(displacement, centroids, by="iso3")

load("intermediate_data/iiasa.RData")
setnames(iiasa,c("Scenario","Region"),c("scenario", "iso3"))
iiasa$scenario = tolower(iiasa$scenario)
iiasa = subset(iiasa, scenario=="ssp1")
iiasa = iiasa[,c("iso3", "year", "pop")]
training_set = merge(training_set, iiasa, by=c("iso3", "year"))

climate = fread("~/git/saint/outputs/regression_climate_worldclim_forecast.csv")
climate$climate_disasters[which(climate$year>=2014)] = climate$y_hat[which(climate$year>=2014)]
climate = subset(climate, scenario=="ssp1")
keep = c(
  "climate_disasters",
  "iso3",
  "year"
)
climate = climate[,keep,with=F]
training_set = merge(training_set, climate, by=c("iso3", "year"))

conflict = fread("~/git/saint/outputs/binary_conflict_clim_bigram_forecast.csv")
conflict$conflict[which(conflict$year>=2014)] = conflict$y_hat[which(conflict$year>=2014)]
conflict = subset(conflict, scenario=="ssp1")
keep = c("iso3", "year", "conflict")
conflict = conflict[,keep, with=F]
training_set = merge(training_set, conflict, by=c("iso3", "year"))

load("./fts/plans.RData")
fts_plans = subset(fts_plans,!is.na(location_iso3))
fts_aggregate = fts_plans[,.(humanitarian_needs=sum(original_requirements,na.rm=T)),by=.(year,location_iso3)]
setnames(fts_aggregate,"location_iso3", "iso3")
training_set = merge(training_set, fts_aggregate, by=c("iso3", "year"))
# training_set$humanitarian_needs[which(is.na(training_set$humanitarian_needs))] = 0
# training_set = subset(training_set, year>=1999)

training_set = training_set[,c(
  "humanitarian_needs",
  "pop",
  "displaced_persons",
  "climate_disasters",
  "conflict",
  "iso3",
  "lat",
  "lon",
  "year"
)]

# Bigrams
load(file="./intermediate_data/world_network.RData")
world_network$from_iso3[which(world_network$from_iso3=="ZAR")] = "COD"
world_network$to_iso3[which(world_network$to_iso3=="ZAR")] = "COD"
world_network$from_iso3[which(world_network$from_iso3=="ROM")] = "ROU"
world_network$to_iso3[which(world_network$to_iso3=="ROM")] = "ROU"
world_network$from_iso3[which(world_network$from_iso3=="TMP")] = "TLS"
world_network$to_iso3[which(world_network$to_iso3=="TMP")] = "TLS"
world_network$from_iso3[which(world_network$from_iso3=="ADO")] = "AND"
world_network$to_iso3[which(world_network$to_iso3=="ADO")] = "AND"

nodes = unique(c(unique(world_network$from_iso3), unique(world_network$to_iso3)))
links = world_network[,c("from_iso3", "to_iso3", "mean_distance_km")]
net <- graph_from_data_frame(d=links, vertices=nodes, directed=F) 
link_weights <- E(net)$mean_distance_km

missing_isos = setdiff(unique(training_set$iso3), nodes)
training_set = subset(training_set, !iso3 %in% missing_isos)

country_nodes = nodes[which(!startsWith(nodes, "WB"))]
country_nodes = country_nodes[which(country_nodes %in% unique(training_set$iso3))]
all_combinations = combn(country_nodes, 2)
country_bigrams_list = list()
country_bigram_index = 1
pb = txtProgressBar(max=ncol(all_combinations), style=3)
for(i in 1:ncol(all_combinations)){
  setTxtProgressBar(pb, i)
  from = all_combinations[1,i]
  to = all_combinations[2,i]
  from.neighborhood = attributes(neighborhood(net, nodes=which(nodes==from), order=1)[[1]])$names
  adjacent = to %in% from.neighborhood
  if(adjacent){
    country_bigrams_list[[country_bigram_index]] = data.frame(from, to)
    country_bigram_index = country_bigram_index + 1
  }
}
close(pb)
country_bigrams = rbindlist(country_bigrams_list)
country_bigrams$iso3 = paste0(country_bigrams$from, "-", country_bigrams$to)

bigram_grid = expand.grid(iso3=unique(country_bigrams$iso3), year=unique(training_set$year))
country_bigrams_training_set = merge(country_bigrams, bigram_grid, all=T)
country_bigrams_training_set = merge(
  country_bigrams_training_set,
  training_set,
  by.x=c("from", "year"),
  by.y=c("iso3", "year"),
  all.x=T
)
setnames(
  country_bigrams_training_set,
  c(
    "humanitarian_needs", "displaced_persons", 
    "climate_disasters",
    "pop",
    "conflict", "lat", "lon"
  ),
  c(
    "humanitarian_needs.from", "displaced_persons.from",
    "climate_disasters.from",
    "pop.from",
    "conflict.from", "lat.from", "lon.from"
  )
)
country_bigrams_training_set = merge(
  country_bigrams_training_set,
  training_set,
  by.x=c("to", "year"),
  by.y=c("iso3", "year"),
  all.x=T
)
setnames(
  country_bigrams_training_set,
  c(
    "humanitarian_needs", "displaced_persons", 
    "climate_disasters",
    "pop",
    "conflict", "lat", "lon"
  ),
  c(
    "humanitarian_needs.to", "displaced_persons.to", 
    "climate_disasters.to",
    "pop.to",
    "conflict.to", "lat.to", "lon.to"
  )
)
country_bigrams_training_set = country_bigrams_training_set[complete.cases(country_bigrams_training_set),]
country_bigrams_training_set$pop = rowSums(
  country_bigrams_training_set[,c("pop.from", "pop.to")],
  na.rm=T
)
country_bigrams_training_set$humanitarian_needs = rowSums(
  country_bigrams_training_set[,c("humanitarian_needs.from", "humanitarian_needs.to")],
  na.rm=T
)
country_bigrams_training_set$displaced_persons = rowSums(
  country_bigrams_training_set[,c("displaced_persons.from", "displaced_persons.to")],
  na.rm=T
)
country_bigrams_training_set$climate_disasters = rowSums(
  country_bigrams_training_set[,c("climate_disasters.from", "climate_disasters.to")],
  na.rm=T
)
country_bigrams_training_set$conflict = pmax(
  country_bigrams_training_set$conflict.from, country_bigrams_training_set$conflict.to,
  na.rm=T
)
country_bigrams_training_set$lat = rowMeans(
  country_bigrams_training_set[,c("lat.from", "lat.to")],
  na.rm=T
)
country_bigrams_training_set$lon = rowMeans(
  country_bigrams_training_set[,c("lon.from", "lon.to")],
  na.rm=T
)


country_bigrams_training_set[,c(
  "pop.from", "pop.to",
  "humanitarian_needs.from", "humanitarian_needs.to",
  "displaced_persons.from", "displaced_persons.to",
  "climate_disasters.from", "climate_disasters.to",
  "conflict.from", "conflict.to",
  "lat.from", "lat.to",
  "lon.from", "lon.to"
)] = NULL
training_set = rbindlist(list(training_set, country_bigrams_training_set), fill=T)
training_set[,c("from", "to")] = NULL

training_set = training_set[order(training_set$iso3, training_set$year),]
# training_set$pop = training_set$pop * 1e6

# training_set$humanitarian_needs = training_set$humanitarian_needs / (training_set$pop * 1e6)
# training_set$displaced_persons = training_set$displaced_persons / (training_set$pop * 1e6)
# training_set$climate_affected_persons = training_set$climate_affected_persons / (training_set$pop * 1e6)
# training_set$pop = NULL

training_set = training_set[,c(
  "humanitarian_needs",
  # "pop",
  "displaced_persons",
  "climate_disasters",
  "conflict",
  "iso3",
  # "lat",
  # "lon",
  "year"
)]

fwrite(training_set, "./intermediate_data/tripartite_bigram.csv")
