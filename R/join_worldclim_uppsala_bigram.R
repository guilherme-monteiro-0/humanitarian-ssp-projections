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
  "iso3",
  "year",
  "PopSSP1",
  "PopSSP2",
  "PopSSP3",
  "PopSSP5",
  "YMHEPSSP1",
  "YMHEPSSP2",
  "YMHEPSSP3",
  "YMHEPSSP5",
  "GDPcapSSP1",
  "GDPcapSSP2",
  "GDPcapSSP3",
  "GDPcapSSP5"
)
conflict = conflict[,keep]
conflict_l = melt(conflict,id.vars=c("conflict","iso3","year"))
conflict_l$variable = as.character(conflict_l$variable)
conflict_l$scenario = tolower(substr(conflict_l$variable, nchar(conflict_l$variable)-3, nchar(conflict_l$variable)))
conflict_l$variable = substr(conflict_l$variable, 1, nchar(conflict_l$variable)-4)
conflict_w = dcast(conflict_l, conflict+iso3+year+scenario~variable)
conflict_w = subset(conflict_w, year<=2013 & scenario=="ssp1")
conflict_w$scenario = NULL
conflict_w = conflict_w[,c(
  "conflict",
  "GDPcap",
  "Pop",
  "YMHEP",
  "iso3",
  "year"
)]
conflict_w$conflict[which(conflict_w$conflict==2)] = 1

worldclim = fread("./WorldClim/ACCESS-CM2/processed/historical.csv")
worldclim = subset(worldclim, year<=2013)
setnames(worldclim,"ISO_A3", "iso3")
conflict = merge(conflict_w, worldclim)

conflict <- conflict %>%                           
  group_by(iso3) %>%
  dplyr::mutate(
    gdp = GDPcap * Pop
  )

conflict = conflict[,c(
  "conflict"
  ,"gdp"
  ,paste("prec",c(1:12),sep="_")
  ,paste("tmin",c(1:12),sep="_")
  ,paste("tmax",c(1:12),sep="_")
  ,"iso3"
  ,"year"
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

missing_isos = setdiff(unique(conflict$iso3), nodes)
conflict = subset(conflict, !iso3 %in% missing_isos)

country_nodes = nodes[which(!startsWith(nodes, "WB"))]
country_nodes = country_nodes[which(country_nodes %in% unique(conflict$iso3))]
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

bigram_grid = expand.grid(iso3=unique(country_bigrams$iso3), year=unique(conflict$year))
country_bigrams_conflict = merge(country_bigrams, bigram_grid, all=T)
country_bigrams_conflict = merge(
  country_bigrams_conflict,
  conflict,
  by.x=c("from", "year"),
  by.y=c("iso3", "year"),
  all.x=T
)
setnames(
  country_bigrams_conflict,
  c(
    "conflict", "gdp", "prec_1",
    "prec_2", "prec_3", "prec_4", "prec_5", "prec_6", "prec_7", "prec_8",
    "prec_9", "prec_10", "prec_11", "prec_12", "tmin_1", "tmin_2", "tmin_3",
    "tmin_4", "tmin_5", "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
    "tmin_11", "tmin_12", "tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
    "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10", "tmax_11", "tmax_12"
  ),
  c(
    "conflict.from", "gdp.from", "prec_1.from",
    "prec_2.from", "prec_3.from", "prec_4.from", "prec_5.from", "prec_6.from", "prec_7.from", "prec_8.from",
    "prec_9.from", "prec_10.from", "prec_11.from", "prec_12.from", "tmin_1.from", "tmin_2.from", "tmin_3.from",
    "tmin_4.from", "tmin_5.from", "tmin_6.from", "tmin_7.from", "tmin_8.from", "tmin_9.from", "tmin_10.from",
    "tmin_11.from", "tmin_12.from", "tmax_1.from", "tmax_2.from", "tmax_3.from", "tmax_4.from", "tmax_5.from",
    "tmax_6.from", "tmax_7.from", "tmax_8.from", "tmax_9.from", "tmax_10.from", "tmax_11.from", "tmax_12.from"
  )
)
country_bigrams_conflict = merge(
  country_bigrams_conflict,
  conflict,
  by.x=c("to", "year"),
  by.y=c("iso3", "year"),
  all.x=T
)
setnames(
  country_bigrams_conflict,
  c(
    "conflict", "gdp", "prec_1",
    "prec_2", "prec_3", "prec_4", "prec_5", "prec_6", "prec_7", "prec_8",
    "prec_9", "prec_10", "prec_11", "prec_12", "tmin_1", "tmin_2", "tmin_3",
    "tmin_4", "tmin_5", "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
    "tmin_11", "tmin_12", "tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
    "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10", "tmax_11", "tmax_12"
  ),
  c(
    "conflict.to", "gdp.to", "prec_1.to",
    "prec_2.to", "prec_3.to", "prec_4.to", "prec_5.to", "prec_6.to", "prec_7.to", "prec_8.to",
    "prec_9.to", "prec_10.to", "prec_11.to", "prec_12.to", "tmin_1.to", "tmin_2.to", "tmin_3.to",
    "tmin_4.to", "tmin_5.to", "tmin_6.to", "tmin_7.to", "tmin_8.to", "tmin_9.to", "tmin_10.to",
    "tmin_11.to", "tmin_12.to", "tmax_1.to", "tmax_2.to", "tmax_3.to", "tmax_4.to", "tmax_5.to",
    "tmax_6.to", "tmax_7.to", "tmax_8.to", "tmax_9.to", "tmax_10.to", "tmax_11.to", "tmax_12.to"
  )
)
country_bigrams_conflict = country_bigrams_conflict[complete.cases(country_bigrams_conflict),]
country_bigrams_conflict$conflict = pmax(
  country_bigrams_conflict$conflict.from, country_bigrams_conflict$conflict.to,
  na.rm=T
)
country_bigrams_conflict$gdp = rowSums(
  country_bigrams_conflict[,c("gdp.from", "gdp.to")],
  na.rm=T
)
country_bigrams_conflict$prec_1 = rowMeans(
  country_bigrams_conflict[,c("prec_1.from", "prec_1.to")],
  na.rm=T
)
country_bigrams_conflict$prec_2 = rowMeans(
  country_bigrams_conflict[,c("prec_2.from", "prec_2.to")],
  na.rm=T
)
country_bigrams_conflict$prec_3 = rowMeans(
  country_bigrams_conflict[,c("prec_3.from", "prec_3.to")],
  na.rm=T
)
country_bigrams_conflict$prec_4 = rowMeans(
  country_bigrams_conflict[,c("prec_4.from", "prec_4.to")],
  na.rm=T
)
country_bigrams_conflict$prec_5 = rowMeans(
  country_bigrams_conflict[,c("prec_5.from", "prec_5.to")],
  na.rm=T
)
country_bigrams_conflict$prec_6 = rowMeans(
  country_bigrams_conflict[,c("prec_6.from", "prec_6.to")],
  na.rm=T
)
country_bigrams_conflict$prec_7 = rowMeans(
  country_bigrams_conflict[,c("prec_7.from", "prec_7.to")],
  na.rm=T
)
country_bigrams_conflict$prec_8 = rowMeans(
  country_bigrams_conflict[,c("prec_8.from", "prec_8.to")],
  na.rm=T
)
country_bigrams_conflict$prec_9 = rowMeans(
  country_bigrams_conflict[,c("prec_9.from", "prec_9.to")],
  na.rm=T
)
country_bigrams_conflict$prec_10 = rowMeans(
  country_bigrams_conflict[,c("prec_10.from", "prec_10.to")],
  na.rm=T
)
country_bigrams_conflict$prec_11 = rowMeans(
  country_bigrams_conflict[,c("prec_11.from", "prec_11.to")],
  na.rm=T
)
country_bigrams_conflict$prec_12 = rowMeans(
  country_bigrams_conflict[,c("prec_12.from", "prec_12.to")],
  na.rm=T
)
country_bigrams_conflict$tmax_1 = pmax(
  country_bigrams_conflict$tmax_1.from, country_bigrams_conflict$tmax_1.to,
  na.rm=T
)
country_bigrams_conflict$tmax_2 = pmax(
  country_bigrams_conflict$tmax_2.from, country_bigrams_conflict$tmax_2.to,
  na.rm=T
)
country_bigrams_conflict$tmax_3 = pmax(
  country_bigrams_conflict$tmax_3.from, country_bigrams_conflict$tmax_3.to,
  na.rm=T
)
country_bigrams_conflict$tmax_4 = pmax(
  country_bigrams_conflict$tmax_4.from, country_bigrams_conflict$tmax_4.to,
  na.rm=T
)
country_bigrams_conflict$tmax_5 = pmax(
  country_bigrams_conflict$tmax_5.from, country_bigrams_conflict$tmax_5.to,
  na.rm=T
)
country_bigrams_conflict$tmax_6 = pmax(
  country_bigrams_conflict$tmax_6.from, country_bigrams_conflict$tmax_6.to,
  na.rm=T
)
country_bigrams_conflict$tmax_7 = pmax(
  country_bigrams_conflict$tmax_7.from, country_bigrams_conflict$tmax_7.to,
  na.rm=T
)
country_bigrams_conflict$tmax_8 = pmax(
  country_bigrams_conflict$tmax_8.from, country_bigrams_conflict$tmax_8.to,
  na.rm=T
)
country_bigrams_conflict$tmax_9 = pmax(
  country_bigrams_conflict$tmax_9.from, country_bigrams_conflict$tmax_9.to,
  na.rm=T
)
country_bigrams_conflict$tmax_10 = pmax(
  country_bigrams_conflict$tmax_10.from, country_bigrams_conflict$tmax_10.to,
  na.rm=T
)
country_bigrams_conflict$tmax_11 = pmax(
  country_bigrams_conflict$tmax_11.from, country_bigrams_conflict$tmax_11.to,
  na.rm=T
)
country_bigrams_conflict$tmax_12 = pmax(
  country_bigrams_conflict$tmax_12.from, country_bigrams_conflict$tmax_12.to,
  na.rm=T
)
country_bigrams_conflict$tmin_1 = pmin(
  country_bigrams_conflict$tmin_1.from, country_bigrams_conflict$tmin_1.to,
  na.rm=T
)
country_bigrams_conflict$tmin_2 = pmin(
  country_bigrams_conflict$tmin_2.from, country_bigrams_conflict$tmin_2.to,
  na.rm=T
)
country_bigrams_conflict$tmin_3 = pmin(
  country_bigrams_conflict$tmin_3.from, country_bigrams_conflict$tmin_3.to,
  na.rm=T
)
country_bigrams_conflict$tmin_4 = pmin(
  country_bigrams_conflict$tmin_4.from, country_bigrams_conflict$tmin_4.to,
  na.rm=T
)
country_bigrams_conflict$tmin_5 = pmin(
  country_bigrams_conflict$tmin_5.from, country_bigrams_conflict$tmin_5.to,
  na.rm=T
)
country_bigrams_conflict$tmin_6 = pmin(
  country_bigrams_conflict$tmin_6.from, country_bigrams_conflict$tmin_6.to,
  na.rm=T
)
country_bigrams_conflict$tmin_7 = pmin(
  country_bigrams_conflict$tmin_7.from, country_bigrams_conflict$tmin_7.to,
  na.rm=T
)
country_bigrams_conflict$tmin_8 = pmin(
  country_bigrams_conflict$tmin_8.from, country_bigrams_conflict$tmin_8.to,
  na.rm=T
)
country_bigrams_conflict$tmin_9 = pmin(
  country_bigrams_conflict$tmin_9.from, country_bigrams_conflict$tmin_9.to,
  na.rm=T
)
country_bigrams_conflict$tmin_10 = pmin(
  country_bigrams_conflict$tmin_10.from, country_bigrams_conflict$tmin_10.to,
  na.rm=T
)
country_bigrams_conflict$tmin_11 = pmin(
  country_bigrams_conflict$tmin_11.from, country_bigrams_conflict$tmin_11.to,
  na.rm=T
)
country_bigrams_conflict$tmin_12 = pmin(
  country_bigrams_conflict$tmin_12.from, country_bigrams_conflict$tmin_12.to,
  na.rm=T
)
country_bigrams_conflict[,c(
  "conflict.from", "gdp.from", "prec_1.from",
  "prec_2.from", "prec_3.from", "prec_4.from", "prec_5.from", "prec_6.from", "prec_7.from", "prec_8.from",
  "prec_9.from", "prec_10.from", "prec_11.from", "prec_12.from", "tmin_1.from", "tmin_2.from", "tmin_3.from",
  "tmin_4.from", "tmin_5.from", "tmin_6.from", "tmin_7.from", "tmin_8.from", "tmin_9.from", "tmin_10.from",
  "tmin_11.from", "tmin_12.from", "tmax_1.from", "tmax_2.from", "tmax_3.from", "tmax_4.from", "tmax_5.from",
  "tmax_6.from", "tmax_7.from", "tmax_8.from", "tmax_9.from", "tmax_10.from", "tmax_11.from", "tmax_12.from",
  "conflict.to", "gdp.to", "prec_1.to",
  "prec_2.to", "prec_3.to", "prec_4.to", "prec_5.to", "prec_6.to", "prec_7.to", "prec_8.to",
  "prec_9.to", "prec_10.to", "prec_11.to", "prec_12.to", "tmin_1.to", "tmin_2.to", "tmin_3.to",
  "tmin_4.to", "tmin_5.to", "tmin_6.to", "tmin_7.to", "tmin_8.to", "tmin_9.to", "tmin_10.to",
  "tmin_11.to", "tmin_12.to", "tmax_1.to", "tmax_2.to", "tmax_3.to", "tmax_4.to", "tmax_5.to",
  "tmax_6.to", "tmax_7.to", "tmax_8.to", "tmax_9.to", "tmax_10.to", "tmax_11.to", "tmax_12.to"
)] = NULL
conflict = rbindlist(list(conflict, country_bigrams_conflict), fill=T)

conflict <- conflict %>%                           
  group_by(iso3) %>%
  dplyr::mutate(
    gdpgrowth = (gdp - dplyr::lag(gdp, n = 1, default = NA))  / dplyr::lag(gdp, n = 1, default = NA)
  )
conflict = subset(conflict, !is.na(gdpgrowth))

conflict = conflict[,c(
  "conflict"
  ,"gdpgrowth"
  ,paste("prec",c(1:12),sep="_")
  # ,paste("tmin",c(1:12),sep="_")
  ,paste("tmax",c(1:12),sep="_")
  ,"iso3"
  ,"year"
)]
fwrite(conflict,"intermediate_data/conflict_clim_bigram.csv")
