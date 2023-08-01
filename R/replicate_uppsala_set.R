list.of.packages <- c("data.table", "reshape2", "igraph", "dplyr", "foreach", "doSNOW","snow", "doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

load("./uppsala_replication/PredictionSSP_1.RData")
conflict = PredictionSSP_1
rm(PredictionSSP_1)
gc()
conflict_iso3 = fread("./supporting_data/uppsala_iso3.csv")
conflict = merge(conflict, conflict_iso3)

keep = c(
  "conflict",
  "nb_conflict",
  "iso3",
  "year",
  "PopSSP1",
  "PopSSP2",
  "PopSSP3",
  "YMHEPSSP1",
  "YMHEPSSP2",
  "YMHEPSSP3",
  "GDPcapSSP1",
  "GDPcapSSP2",
  "GDPcapSSP3"
)
conflict = conflict[,keep]
conflict_l = melt(conflict,id.vars=c("conflict","nb_conflict","iso3","year"))
conflict_l$variable = as.character(conflict_l$variable)
conflict_l$Scenario = substr(conflict_l$variable, nchar(conflict_l$variable)-3, nchar(conflict_l$variable))
conflict_l$variable = substr(conflict_l$variable, 1, nchar(conflict_l$variable)-4)
conflict_w = dcast(conflict_l, conflict+nb_conflict+iso3+year+Scenario~variable)
conflict_w = subset(conflict_w, year<=2013 & Scenario=="SSP1")
conflict_w$Scenario = NULL
conflict_w = conflict_w[,c(
  "conflict",
  "nb_conflict",
  "GDPcap",
  "Pop",
  "YMHEP",
  "iso3",
  "year"
)]
# Drop neighbor conflict
conflict_w$nb_conflict = NULL
# Enforce binary
conflict_w$conflict[which(conflict_w$conflict==2)] = 1
# Lags
conflict_w = conflict_w[order(conflict_w$iso3, conflict_w$year),]
conflict_w <- conflict_w %>%                           
  group_by(iso3) %>%
  dplyr::mutate(
    Pop_t1 = lag(Pop, n = 1, default = 0),
    Pop_t2 = lag(Pop, n = 2, default = 0),
    Pop_t3 = lag(Pop, n = 3, default = 0),
    GDPcap_t1 = lag(GDPcap, n = 1, default = 0),
    GDPcap_t2 = lag(GDPcap, n = 2, default = 0),
    GDPcap_t3 = lag(GDPcap, n = 3, default = 0),
    YMHEP_t1 = lag(YMHEP, n = 1, default = 0),
    YMHEP_t2 = lag(YMHEP, n = 2, default = 0),
    YMHEP_t3 = lag(YMHEP, n = 3, default = 0),
  )
setnames(
  conflict_w,
  c("Pop", "GDPcap", "YMHEP"),
  c("Pop_t0", "GDPcap_t0", "YMHEP_t0")
)
lagged_vars = c(
  "Pop_t1",
  "Pop_t2",
  "Pop_t3",
  "GDPcap_t1",
  "GDPcap_t2",
  "GDPcap_t3",
  "YMHEP_t1",
  "YMHEP_t2",
  "YMHEP_t3"
)

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

missing_isos = setdiff(unique(conflict_w$iso3), nodes)
conflict_w = subset(conflict_w, !iso3 %in% missing_isos)

# Calculate mean lagged vars for orders of neighbors
orders = c(1:3)
lags = c(0:3)
ivs = c("Pop", "GDPcap", "YMHEP")
iv_funcs = list(
  "Pop"=function(x, w){return(sum(x,na.rm=T))},
  "GDPcap"=function(x, w){return(weighted.mean(x, w,na.rm=T))},
  "YMHEP"=function(x, w){return(weighted.mean(x, w,na.rm=T))}
)
spatial_lagged_vars = c()
for(order in orders){
  for(lag in lags){
    for(iv in ivs){
      var_name = paste0(iv,"_t",lag,"_o",order)
      conflict_w[,var_name] = 0
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

pb = txtProgressBar(max=nrow(conflict_w), style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

conflict_w_spatial = foreach(i=1:nrow(conflict_w), .combine = rbind, .options.snow = opts) %dopar% {
  row = conflict_w[i,]
  row_year = row$year
  row_iso = row$iso3
  neighborhood_o1_iso3s = attributes(igraph::neighborhood(net, nodes=which(nodes==row_iso), order=1)[[1]])$names
  neighborhood_o2_iso3s = attributes(igraph::neighborhood(net, nodes=which(nodes==row_iso), order=2)[[1]])$names
  neighborhood_o3_iso3s = attributes(igraph::neighborhood(net, nodes=which(nodes==row_iso), order=3)[[1]])$names
  neighborhood_o3_subset = conflict_w[which((conflict_w$iso3 %in% neighborhood_o3_iso3s) & conflict_w$year == row_year),]
  neighborhood_o2_subset = neighborhood_o3_subset[which((neighborhood_o3_subset$iso3 %in% neighborhood_o2_iso3s)),]
  neighborhood_o1_subset = neighborhood_o2_subset[which((neighborhood_o2_subset$iso3 %in% neighborhood_o1_iso3s)),]
  for(order in orders){
    for(lag in lags){
      for(iv in ivs){
        iv_func = iv_funcs[[iv]]
        var_name = paste0(iv,"_t",lag,"_o",order)
        source_name = paste0(iv,"_t",lag)
        weight_source_name = paste0("Pop_t",lag)
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
stopCluster(parallelCluster)

fwrite(conflict_w_spatial, "intermediate_data/simple_uppsala_replication.csv")
