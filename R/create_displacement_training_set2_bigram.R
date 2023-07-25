list.of.packages <- c("data.table", "reshape2", "igraph", "dplyr", "foreach", "doSNOW","snow", "doParallel")
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

country_nodes = nodes[which(!startsWith(nodes, "WB"))]
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
country_bigrams$Region = paste0(country_bigrams$from, "-", country_bigrams$to)

missing_isos = setdiff(unique(unhcr_pop_agg$Region), nodes)
unhcr_pop_agg = subset(unhcr_pop_agg, !Region %in% missing_isos)
pop_grid = expand.grid(Region=unique(unhcr_pop_agg$Region), year=unique(unhcr_pop_agg$year))
unhcr_pop_agg = merge(unhcr_pop_agg, pop_grid, all=T)
unhcr_pop_agg$displaced_persons[which(is.na(unhcr_pop_agg$displaced_persons))] = 0
bigram_grid = expand.grid(Region=unique(country_bigrams$Region), year=unique(unhcr_pop_agg$year))
country_bigrams_unhcr = merge(country_bigrams, bigram_grid, all=T)
country_bigrams_unhcr = merge(
  country_bigrams_unhcr,
  unhcr_pop_agg,
  by.x=c("from", "year"),
  by.y=c("Region", "year"),
  all.x=T
)
setnames(country_bigrams_unhcr, "displaced_persons", "displaced_persons.from")
country_bigrams_unhcr = merge(
  country_bigrams_unhcr,
  unhcr_pop_agg,
  by.x=c("to", "year"),
  by.y=c("Region", "year"),
  all.x=T
)
setnames(country_bigrams_unhcr, "displaced_persons", "displaced_persons.to")
country_bigrams_unhcr$displaced_persons = rowSums(
  country_bigrams_unhcr[,c("displaced_persons.from", "displaced_persons.to")],
  na.rm=T
)
country_bigrams_unhcr[,c("from","to","displaced_persons.from","displaced_persons.to")] = NULL
unhcr_pop_agg = rbind(unhcr_pop_agg, country_bigrams_unhcr)

missing_isos = setdiff(unique(iiasa$Region), nodes)
iiasa = subset(iiasa, !Region %in% missing_isos)
iiasa_grid = expand.grid(Region=unique(iiasa$Region), year=unique(iiasa$year), Scenario=unique(iiasa$Scenario))
iiasa = merge(iiasa, iiasa_grid, by=c("Region", "year", "Scenario"), all=T)
iiasa[is.na(iiasa)] = 0
iiasa = iiasa[order(iiasa$Scenario, iiasa$Region, iiasa$year),]
bigram_grid = expand.grid(Scenario=unique(iiasa$Scenario), Region=unique(country_bigrams$Region), year=unique(iiasa$year))
country_bigrams_iiasa = merge(country_bigrams, bigram_grid, all=T)
country_bigrams_iiasa = merge(
  country_bigrams_iiasa,
  iiasa,
  by.x=c("from", "year", "Scenario"),
  by.y=c("Region", "year", "Scenario"),
  all.x=T
)
setnames(
  country_bigrams_iiasa,
  c("pop", "gdp", "urban"),
  c("pop.from", "gdp.from", "urban.from")
)
country_bigrams_iiasa = merge(
  country_bigrams_iiasa,
  iiasa,
  by.x=c("to", "year", "Scenario"),
  by.y=c("Region", "year", "Scenario"),
  all.x=T
)
setnames(
  country_bigrams_iiasa,
  c("pop", "gdp", "urban"),
  c("pop.to", "gdp.to", "urban.to")
)
country_bigrams_iiasa$pop = rowSums(
  country_bigrams_iiasa[,c("pop.from", "pop.to")],
  na.rm=T
)
country_bigrams_iiasa$gdp = rowSums(
  country_bigrams_iiasa[,c("gdp.from", "gdp.to")],
  na.rm=T
)
country_bigrams_iiasa$urban = 
  ((country_bigrams_iiasa$urban.from * country_bigrams_iiasa$pop.from) +
  (country_bigrams_iiasa$urban.to * country_bigrams_iiasa$pop.to)) /
  country_bigrams_iiasa$pop 
country_bigrams_iiasa[,c(
  "pop.from",
  "pop.to",
  "gdp.from",
  "gdp.to",
  "urban.from",
  "urban.to"
  )] = NULL
iiasa = rbindlist(list(iiasa, country_bigrams_iiasa), fill=T)

iiasa <- iiasa %>%                           
  group_by(Scenario, Region) %>%
  dplyr::mutate(
    pop_t1 = lag(pop, n = 1, default = 0),
    pop_t2 = lag(pop, n = 2, default = 0),
    pop_t3 = lag(pop, n = 3, default = 0),
    gdp_t1 = lag(gdp, n = 1, default = 0),
    gdp_t2 = lag(gdp, n = 2, default = 0),
    gdp_t3 = lag(gdp, n = 3, default = 0),
    urban_t1 = lag(urban, n = 1, default = 0),
    urban_t2 = lag(urban, n = 2, default = 0),
    urban_t3 = lag(urban, n = 3, default = 0),
  )
lagged_vars = c(
  "pop_t1",
  "pop_t2",
  "pop_t3",
  "gdp_t1",
  "gdp_t2",
  "gdp_t3",
  "urban_t1",
  "urban_t2",
  "urban_t3"
)

iiasa = subset(iiasa, Scenario=="SSP1")
iiasa$Scenario = NULL
iiasa$Region = as.character(iiasa$Region)

# Calculate mean lagged vars for orders of neighbors
orders = c(1:3)
lags = c(1:3)
ivs = c("pop", "gdp", "urban")
iv_funcs = list(
  "pop"=function(x, w){return(sum(x,na.rm=T))},
  "gdp"=function(x, w){return(sum(x,na.rm=T))},
  "urban"=function(x, w){return(weighted.mean(x, w,na.rm=T))}
)
spatial_lagged_vars = c()
for(order in orders){
  for(lag in lags){
    for(iv in ivs){
      var_name = paste0(iv,"_t",lag,"_o",order)
      spatial_lagged_vars = c(spatial_lagged_vars, var_name)
    }
  }
}

# Detect the number of cores to use and set up cluster
nCores <- detectCores() - 2
parallelCluster <- makeCluster(nCores,type = "SOCK",methods = FALSE) # Make a parallel cluster
setDefaultCluster(parallelCluster)
registerDoSNOW(parallelCluster)

# Tie R exit to the shutdown of cluster nodes
on.exit({
  try({
    cat("Attempting to stop cluster\n")
    stopImplicitCluster()        # package: `doParallel`
    stopCluster(parallelCluster) # package: `parallel`
  })
})

pb = txtProgressBar(max=nrow(iiasa), style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

iiasa_spatial = foreach(i=1:nrow(iiasa), .combine = rbind, .options.snow = opts) %dopar% {
  row = iiasa[i,]
  row_year = row$year
  row_iso = row$Region
  row_from = row$from
  if(is.na(row_from)){
    neighborhood_o1_iso3s = attributes(igraph::neighborhood(net, nodes=which(nodes==row_iso), order=1)[[1]])$names
    neighborhood_o2_iso3s = attributes(igraph::neighborhood(net, nodes=which(nodes==row_iso), order=2)[[1]])$names
    neighborhood_o3_iso3s = attributes(igraph::neighborhood(net, nodes=which(nodes==row_iso), order=3)[[1]])$names
  }else{
    row_to = row$to
    neighborhood_o1_iso3s_from = attributes(igraph::neighborhood(net, nodes=which(nodes==row_from), order=1)[[1]])$names
    neighborhood_o2_iso3s_from = attributes(igraph::neighborhood(net, nodes=which(nodes==row_from), order=2)[[1]])$names
    neighborhood_o3_iso3s_from = attributes(igraph::neighborhood(net, nodes=which(nodes==row_from), order=3)[[1]])$names
    neighborhood_o1_iso3s_to = attributes(igraph::neighborhood(net, nodes=which(nodes==row_to), order=1)[[1]])$names
    neighborhood_o2_iso3s_to = attributes(igraph::neighborhood(net, nodes=which(nodes==row_to), order=2)[[1]])$names
    neighborhood_o3_iso3s_to = attributes(igraph::neighborhood(net, nodes=which(nodes==row_to), order=3)[[1]])$names
    neighborhood_o1_iso3s = intersect(neighborhood_o1_iso3s_from, neighborhood_o1_iso3s_to)
    neighborhood_o2_iso3s = intersect(neighborhood_o2_iso3s_from, neighborhood_o2_iso3s_to)
    neighborhood_o3_iso3s = intersect(neighborhood_o3_iso3s_from, neighborhood_o3_iso3s_to)
  }
  neighborhood_o3_subset = iiasa[which((iiasa$Region %in% neighborhood_o3_iso3s) & iiasa$year == row_year),]
  neighborhood_o2_subset = neighborhood_o3_subset[which((neighborhood_o3_subset$Region %in% neighborhood_o2_iso3s)),]
  neighborhood_o1_subset = neighborhood_o2_subset[which((neighborhood_o2_subset$Region %in% neighborhood_o1_iso3s)),]
  for(order in orders){
    for(lag in lags){
      for(iv in ivs){
        iv_func = iv_funcs[[iv]]
        var_name = paste0(iv,"_t",lag,"_o",order)
        source_name = paste0(iv,"_t",lag)
        weight_source_name = paste0("pop_t",lag)
        neighborhood_name = paste0("neighborhood_o",order,"_subset")
        neighborhood_subset = get(neighborhood_name)
        weights = neighborhood_subset[,weight_source_name]
        if(sum(weights)==0){
          weights[,weight_source_name] = 1
        }
        row[,var_name] = iv_func(neighborhood_subset[,source_name], weights)
      }
    }
  }
  return(row)
}
close(pb)
stopCluster(parallelCluster)

iiasa_spatial[,c("from", "to")] = NULL
training = merge(iiasa_spatial, unhcr_pop_agg, by=c("year", "Region"), all.x=T)
training = subset(training, year <= 2022)
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
fwrite(training, "intermediate_data/iiasa_unhcr_displaced2_bigram.csv")
