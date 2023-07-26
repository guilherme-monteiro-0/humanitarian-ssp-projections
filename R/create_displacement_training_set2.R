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

missing_isos = setdiff(unique(unhcr_pop_agg$Region), nodes)
unhcr_pop_agg = subset(unhcr_pop_agg, !Region %in% missing_isos)
pop_grid = expand.grid(Region=unique(unhcr_pop_agg$Region), year=unique(unhcr_pop_agg$year))
unhcr_pop_agg = merge(unhcr_pop_agg, pop_grid, all=T)
unhcr_pop_agg$displaced_persons[which(is.na(unhcr_pop_agg$displaced_persons))] = 0

missing_isos = setdiff(unique(iiasa$Region), nodes)
iiasa = subset(iiasa, !Region %in% missing_isos)
iiasa_grid = expand.grid(Region=unique(iiasa$Region), year=unique(iiasa$year), Scenario=unique(iiasa$Scenario))
iiasa = merge(iiasa, iiasa_grid, by=c("Region", "year", "Scenario"), all=T)
iiasa[is.na(iiasa)] = 0
iiasa = iiasa[order(iiasa$Scenario, iiasa$Region, iiasa$year),]

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
      iiasa[,var_name] = 0
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
  neighborhood_o1_iso3s = attributes(igraph::neighborhood(net, nodes=which(nodes==row_iso), order=1)[[1]])$names
  neighborhood_o2_iso3s = attributes(igraph::neighborhood(net, nodes=which(nodes==row_iso), order=2)[[1]])$names
  neighborhood_o3_iso3s = attributes(igraph::neighborhood(net, nodes=which(nodes==row_iso), order=3)[[1]])$names
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
          weights = rep(1, nrow(neighborhood_subset))
        }
        row[,var_name] = iv_func(neighborhood_subset[,source_name], weights)
      }
    }
  }
  return(row)
}
close(pb)

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
fwrite(training, "intermediate_data/iiasa_unhcr_displaced2.csv")
