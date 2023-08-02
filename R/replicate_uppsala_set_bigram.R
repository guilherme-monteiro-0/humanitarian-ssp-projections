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

missing_isos = setdiff(unique(conflict_w$iso3), nodes)
conflict_w = subset(conflict_w, !iso3 %in% missing_isos)

country_nodes = nodes[which(!startsWith(nodes, "WB"))]
country_nodes = country_nodes[which(country_nodes %in% unique(conflict_w$iso3))]
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

bigram_grid = expand.grid(iso3=unique(country_bigrams$iso3), year=unique(conflict_w$year))
country_bigrams_conflict_w = merge(country_bigrams, bigram_grid, all=T)
country_bigrams_conflict_w = merge(
  country_bigrams_conflict_w,
  conflict_w,
  by.x=c("from", "year"),
  by.y=c("iso3", "year"),
  all.x=T
)
setnames(
  country_bigrams_conflict_w,
  c("conflict", "Pop", "GDPcap", "YMHEP"),
  c("conflict.from", "Pop.from", "GDPcap.from", "YMHEP.from")
)
country_bigrams_conflict_w = merge(
  country_bigrams_conflict_w,
  conflict_w,
  by.x=c("to", "year"),
  by.y=c("iso3", "year"),
  all.x=T
)
setnames(
  country_bigrams_conflict_w,
  c("conflict", "Pop", "GDPcap", "YMHEP"),
  c("conflict.to", "Pop.to", "GDPcap.to", "YMHEP.to")
)
country_bigrams_conflict_w$conflict = (rowSums(
  country_bigrams_conflict_w[,c("conflict.from", "conflict.to")],
  na.rm=T
) > 0) * 1
country_bigrams_conflict_w$Pop = rowSums(
  country_bigrams_conflict_w[,c("Pop.from", "Pop.to")],
  na.rm=T
)
country_bigrams_conflict_w$GDP.from = country_bigrams_conflict_w$GDPcap.from * country_bigrams_conflict_w$Pop.from
country_bigrams_conflict_w$GDP.to = country_bigrams_conflict_w$GDPcap.to * country_bigrams_conflict_w$Pop.to
country_bigrams_conflict_w$GDP = rowSums(
  country_bigrams_conflict_w[,c("GDP.from", "GDP.to")],
  na.rm=T
)
country_bigrams_conflict_w$GDPcap = country_bigrams_conflict_w$GDP / country_bigrams_conflict_w$Pop
country_bigrams_conflict_w$YMHE.from = country_bigrams_conflict_w$YMHEP.from * country_bigrams_conflict_w$Pop.from
country_bigrams_conflict_w$YMHE.to = country_bigrams_conflict_w$YMHEP.to * country_bigrams_conflict_w$Pop.to
country_bigrams_conflict_w$YMHE = rowSums(
  country_bigrams_conflict_w[,c("YMHE.from", "YMHE.to")],
  na.rm=T
)
country_bigrams_conflict_w$YMHEP = country_bigrams_conflict_w$YMHE / country_bigrams_conflict_w$Pop
country_bigrams_conflict_w[,c(
  "conflict.from",
  "conflict.to",
  "Pop.from",
  "Pop.to",
  "GDP",
  "GDP.from",
  "GDP.to",
  "GDPcap.from",
  "GDPcap.to",
  "YMHEP.from",
  "YMHEP.to",
  "YMHE",
  "YMHE.from",
  "YMHE.to"
)] = NULL
conflict_w = rbindlist(list(conflict_w, country_bigrams_conflict_w), fill=T)
conflict_w$GDPcap[which(is.infinite(conflict_w$GDPcap))] = 0
conflict_w$YMHEP[which(is.infinite(conflict_w$YMHEP))] = 0
conflict_w$GDPcap[which(is.nan(conflict_w$GDPcap))] = 0
conflict_w$YMHEP[which(is.nan(conflict_w$YMHEP))] = 0

# Lags
conflict_w = conflict_w[order(conflict_w$iso3, conflict_w$year),]
conflict_w <- conflict_w %>%                           
  group_by(iso3) %>%
  dplyr::mutate(
    # conflict_t1 = lag(conflict, n = 1, default = 0),
    # conflict_t2 = lag(conflict, n = 2, default = 0),
    # conflict_t3 = lag(conflict, n = 3, default = 0),
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

fwrite(conflict_w_spatial, "intermediate_data/simple_uppsala_replication_bigram.csv")
