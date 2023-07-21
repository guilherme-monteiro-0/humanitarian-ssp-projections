list.of.packages <- c("data.table", "reshape2", "igraph", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

load("intermediate_data/iiasa.RData")

unhcr_pop = fread("./UNHCR/population.csv", skip=14, na.strings=c("","-"))
keep = c(
  "Year",
  "Country of origin (ISO)",
  "Refugees under UNHCR's mandate",
  "Asylum-seekers",
  "IDPs of concern to UNHCR",
  "Stateless persons"
)
unhcr_pop = unhcr_pop[,keep, with=F]
names(unhcr_pop) = c("year", "Region", "refugees", "asylum", "idps", "stateless")
unhcr_pop_l = melt(unhcr_pop, id.vars=c("year", "Region"))
unhcr_pop_l$Region[which(unhcr_pop_l$Region=="ESH")] = "MAR" # No Western Sahara in World network
unhcr_pop_agg = data.table(unhcr_pop_l)[,.(displaced_persons=sum(value,na.rm=T)), by=.(year, Region)]

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

missing_isos = setdiff(unique(unhcr_pop_agg$Region), nodes)
unhcr_pop_agg = subset(unhcr_pop_agg, !Region %in% missing_isos)
pop_grid = expand.grid(Region=unique(unhcr_pop_agg$Region), year=unique(unhcr_pop_agg$year))
unhcr_pop_agg = merge(unhcr_pop_agg, pop_grid, all=T)
unhcr_pop_agg$displaced_persons[which(is.na(unhcr_pop_agg$displaced_persons))] = 0

unhcr_pop_agg = unhcr_pop_agg[order(unhcr_pop_agg$Region, unhcr_pop_agg$year),]

unhcr_pop_agg <- unhcr_pop_agg %>%                           
  group_by(Region) %>%
  dplyr::mutate(
    displaced_persons_t1 = lag(displaced_persons, n = 1, default = 0),
    displaced_persons_t2 = lag(displaced_persons, n = 2, default = 0),
    displaced_persons_t3 = lag(displaced_persons, n = 3, default = 0),
  )
lagged_vars = c(
  "displaced_persons_t1",
  "displaced_persons_t2",
  "displaced_persons_t3"
)

# Calculate mean lagged displacement for orders of neighbors
# Possible future idea: Also create country "bigrams" to increase data
orders = c(1:3)
lags = c(1:3)
spatial_lagged_vars = c()
for(order in orders){
  for(lag in lags){
    var_name = paste0("mean_displaced_persons_t",lag,"_o",order)
    unhcr_pop_agg[,var_name] = 0
    spatial_lagged_vars = c(spatial_lagged_vars, var_name)
  }
}
pb = txtProgressBar(max=nrow(unhcr_pop_agg), style=3)
for(i in 1:nrow(unhcr_pop_agg)){
  setTxtProgressBar(pb, i)
  row = unhcr_pop_agg[i,]
  row_year = row$year
  neighborhood_o1_iso3s = attributes(neighborhood(net, nodes=which(nodes=="COD"), order=1)[[1]])$names
  neighborhood_o2_iso3s = attributes(neighborhood(net, nodes=which(nodes=="COD"), order=2)[[1]])$names
  neighborhood_o3_iso3s = attributes(neighborhood(net, nodes=which(nodes=="COD"), order=3)[[1]])$names
  neighborhood_o3_subset = unhcr_pop_agg[which((unhcr_pop_agg$Region %in% neighborhood_o3_iso3s) & unhcr_pop_agg$year == row_year),]
  neighborhood_o2_subset = neighborhood_o3_subset[which((neighborhood_o3_subset$Region %in% neighborhood_o2_iso3s)),]
  neighborhood_o1_subset = neighborhood_o2_subset[which((neighborhood_o2_subset$Region %in% neighborhood_o1_iso3s)),]
  for(order in orders){
    for(lag in lags){
      var_name = paste0("mean_displaced_persons_t",lag,"_o",order)
      source_name = paste0("displaced_persons_t",lag)
      neighborhood_name = paste0("neighborhood_o",order,"_subset")
      neighborhood_subset = get(neighborhood_name)
      unhcr_pop_agg[i,var_name] = sum(neighborhood_subset[,source_name], na.rm=T) / nrow(neighborhood_subset)
    }
  }
}
close(pb)

training = merge(unhcr_pop_agg, iiasa, by=c("year", "Region"), all=T)
training = subset(training, Scenario=="SSP1" & year <= 2022)
training = training[,c(
  "displaced_persons",
  lagged_vars,
  spatial_lagged_vars,
  "Region",
  "year",
  "pop",
  "gdp",
  "urban"
)]
training$displaced_persons[which(is.na(training$displaced_persons))] = 0
fwrite(training, "intermediate_data/iiasa_unhcr_displaced.csv")