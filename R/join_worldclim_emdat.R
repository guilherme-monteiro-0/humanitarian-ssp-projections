list.of.packages <- c("data.table", "reshape2", "sp", "geosphere")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

worldclim = fread("intermediate_data/conflict_clim_bigram.csv")
worldclim$conflict = NULL

emdat = fread("EM-DAT/emdat_110723.csv")
setnames(
  emdat,
  c("ISO","Year","Total Deaths","No Injured","No Affected","No Homeless","Total Affected"),
  c("iso3","year","total_deaths","no_injured","no_affected","no_homeless","total_affected")
)
emdat = emdat[,c("iso3","year","total_deaths","no_injured","no_affected","no_homeless","total_affected")]
emdat$max_affected = 
  pmax(
    emdat$total_deaths,
    emdat$no_injured,
    emdat$no_affected,
    emdat$no_homeless,
    emdat$total_affected,
    na.rm=T
  )
emdat = emdat[,c("iso3","year","max_affected")]
emdat[is.na(emdat)] = 0
emdat = data.table(emdat)[,.(max_affected=sum(max_affected)),by=.(iso3,year)]

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

missing_isos = setdiff(unique(emdat$iso3), nodes)
emdat = subset(emdat, !iso3 %in% missing_isos)

country_nodes = nodes[which(!startsWith(nodes, "WB"))]
country_nodes = country_nodes[which(country_nodes %in% unique(emdat$iso3))]
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

bigram_grid = expand.grid(iso3=unique(country_bigrams$iso3), year=unique(emdat$year))
country_bigrams_emdat = merge(country_bigrams, bigram_grid, all=T)
country_bigrams_emdat = merge(
  country_bigrams_emdat,
  emdat,
  by.x=c("from", "year"),
  by.y=c("iso3", "year"),
  all.x=T
)
setnames(
  country_bigrams_emdat,
  c(
    "max_affected"
  ),
  c(
    "max_affected.from"
  )
)
country_bigrams_emdat = merge(
  country_bigrams_emdat,
  emdat,
  by.x=c("to", "year"),
  by.y=c("iso3", "year"),
  all.x=T
)
setnames(
  country_bigrams_emdat,
  c(
    "max_affected"
  ),
  c(
    "max_affected.to"
  )
)
country_bigrams_emdat = country_bigrams_emdat[complete.cases(country_bigrams_emdat),]
country_bigrams_emdat$max_affected = rowSums(
  country_bigrams_emdat[,c("max_affected.from", "max_affected.to")],
  na.rm=T
)

country_bigrams_emdat[,c(
  "max_affected.from", "max_affected.to", "from", "to"
)] = NULL
emdat = rbindlist(list(emdat, country_bigrams_emdat), fill=T)

climate_worldclim = merge(emdat, worldclim, all.y=T)
climate_worldclim[is.na(climate_worldclim)] = 0
climate_worldclim = climate_worldclim[,c(
  "max_affected"
  ,paste("prec",c(1:12),sep="_")
  ,paste("tmax",c(1:12),sep="_")
  ,"iso3"
  ,"year"
)]
fwrite(climate_worldclim, "intermediate_data/climate_worldclim.csv")



worldclim_forecasting = fread("intermediate_data/large/conflict_clim_forecasting.csv")
worldclim_forecasting$conflict = NULL

climate_worldclim_forecasting = merge(emdat, worldclim_forecasting, all.y=T)
climate_worldclim_forecasting[is.na(climate_worldclim_forecasting)] = 0
climate_worldclim_forecasting = climate_worldclim_forecasting[,c(
  "max_affected"
  ,"scenario"
  ,paste("prec",c(1:12),sep="_")
  ,paste("tmax",c(1:12),sep="_")
  ,"iso3"
  ,"year"
)]
fwrite(climate_worldclim_forecasting, "intermediate_data/large/climate_worldclim_forecasting.csv")
