list.of.packages <- c("data.table", "reshape2", "igraph", "dplyr", "foreach", "doSNOW","snow", "doParallel", "WDI", "zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

unhcr_pop = fread("./UNHCR/population.csv", skip=14, na.strings=c("","-"))
keep = c(
  "Year",
  "Country of origin (ISO)",
  "Refugees under UNHCR's mandate",
  "Asylum-seekers",
  "IDPs of concern to UNHCR"
)
unhcr_pop = unhcr_pop[,keep, with=F]
names(unhcr_pop) = c("year", "iso3", "refugees", "asylum", "idps")
unhcr_pop_l = melt(unhcr_pop, id.vars=c("year", "iso3"))
unhcr_pop_l$iso3[which(unhcr_pop_l$iso3=="ESH")] = "MAR" # No Western Sahara in World network
unhcr_pop_agg = data.table(unhcr_pop_l)[,.(displaced_persons=sum(value,na.rm=T)), by=.(year, iso3)]
unhcr_pop_agg = subset(unhcr_pop_agg, year > 1992)
pop_grid = expand.grid(iso3=unique(unhcr_pop_agg$iso3), year=unique(unhcr_pop_agg$year))
unhcr_pop_agg = merge(unhcr_pop_agg, pop_grid, all=T)
unhcr_pop_agg$displaced_persons[which(is.na(unhcr_pop_agg$displaced_persons))] = 0

worldclim = fread("./WorldClim/ACCESS-CM2/processed/historical.csv")
setnames(worldclim,"ISO_A3", "iso3")
unhcr_pop_agg = merge(unhcr_pop_agg, worldclim)

# gdp_ppp_2017 = WDI(
#   indicator = "NY.GDP.MKTP.PP.KD",
#   start=1992,
#   end=2023
# )
# gdp_ppp_2017 = gdp_ppp_2017[,c("iso3c", "year", "NY.GDP.MKTP.PP.KD")]
# names(gdp_ppp_2017) = c("iso3","year","gdp")
# gdp_ppp_2017 = subset(gdp_ppp_2017, iso3!="")
# gdp_ppp_2017 = gdp_ppp_2017[order(gdp_ppp_2017$iso3, gdp_ppp_2017$year),]
# gdp_ppp_2017 = data.table(gdp_ppp_2017)[,.(gdp=na.approx(gdp)), by=.(iso3, year)]
# gdp_ppp_2017 = gdp_ppp_2017[,.(year=year, gdp=gdp, gdp_t1=dplyr::lag(gdp,1)), by=.(iso3)]
# gdp_ppp_2017$gdp_growth = (gdp_ppp_2017$gdp - gdp_ppp_2017$gdp_t1) / gdp_ppp_2017$gdp_t1
# gdp = gdp_ppp_2017[,c("iso3", "year", "gdp_growth", "gdp")]
# gdp = subset(gdp, !is.na(gdp_growth))
# 
# unhcr_pop_agg = merge(unhcr_pop_agg, gdp)

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
country_nodes = country_nodes[which(country_nodes %in% unique(unhcr_pop_agg$iso3))]

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

bigram_grid = expand.grid(iso3=unique(country_bigrams$iso3), year=unique(unhcr_pop_agg$year))
country_bigrams_unhcr = merge(country_bigrams, bigram_grid, all=T)
country_bigrams_unhcr = merge(
  country_bigrams_unhcr,
  unhcr_pop_agg,
  by.x=c("from", "year"),
  by.y=c("iso3", "year"),
  all.x=T
)
setnames(
  country_bigrams_unhcr,
  c(
    "displaced_persons", 
    # "gdp", "gdp_growth", 
    "prec_1",
    "prec_2", "prec_3", "prec_4", "prec_5", "prec_6", "prec_7", "prec_8",
    "prec_9", "prec_10", "prec_11", "prec_12", "tmin_1", "tmin_2", "tmin_3",
    "tmin_4", "tmin_5", "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
    "tmin_11", "tmin_12", "tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
    "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10", "tmax_11", "tmax_12"
  ),
  c(
    "displaced_persons.from", 
    # "gdp.from", "gdp_growth.from", 
    "prec_1.from",
    "prec_2.from", "prec_3.from", "prec_4.from", "prec_5.from", "prec_6.from", "prec_7.from", "prec_8.from",
    "prec_9.from", "prec_10.from", "prec_11.from", "prec_12.from", "tmin_1.from", "tmin_2.from", "tmin_3.from",
    "tmin_4.from", "tmin_5.from", "tmin_6.from", "tmin_7.from", "tmin_8.from", "tmin_9.from", "tmin_10.from",
    "tmin_11.from", "tmin_12.from", "tmax_1.from", "tmax_2.from", "tmax_3.from", "tmax_4.from", "tmax_5.from",
    "tmax_6.from", "tmax_7.from", "tmax_8.from", "tmax_9.from", "tmax_10.from", "tmax_11.from", "tmax_12.from"
  )
)
country_bigrams_unhcr = merge(
  country_bigrams_unhcr,
  unhcr_pop_agg,
  by.x=c("to", "year"),
  by.y=c("iso3", "year"),
  all.x=T
)
setnames(
  country_bigrams_unhcr,
  c(
    "displaced_persons", 
    # "gdp", "gdp_growth", 
    "prec_1",
    "prec_2", "prec_3", "prec_4", "prec_5", "prec_6", "prec_7", "prec_8",
    "prec_9", "prec_10", "prec_11", "prec_12", "tmin_1", "tmin_2", "tmin_3",
    "tmin_4", "tmin_5", "tmin_6", "tmin_7", "tmin_8", "tmin_9", "tmin_10",
    "tmin_11", "tmin_12", "tmax_1", "tmax_2", "tmax_3", "tmax_4", "tmax_5",
    "tmax_6", "tmax_7", "tmax_8", "tmax_9", "tmax_10", "tmax_11", "tmax_12"
  ),
  c(
    "displaced_persons.to", 
    # "gdp.to", "gdp_growth.to", 
    "prec_1.to",
    "prec_2.to", "prec_3.to", "prec_4.to", "prec_5.to", "prec_6.to", "prec_7.to", "prec_8.to",
    "prec_9.to", "prec_10.to", "prec_11.to", "prec_12.to", "tmin_1.to", "tmin_2.to", "tmin_3.to",
    "tmin_4.to", "tmin_5.to", "tmin_6.to", "tmin_7.to", "tmin_8.to", "tmin_9.to", "tmin_10.to",
    "tmin_11.to", "tmin_12.to", "tmax_1.to", "tmax_2.to", "tmax_3.to", "tmax_4.to", "tmax_5.to",
    "tmax_6.to", "tmax_7.to", "tmax_8.to", "tmax_9.to", "tmax_10.to", "tmax_11.to", "tmax_12.to"
  )
)
country_bigrams_unhcr$displaced_persons = rowSums(
  country_bigrams_unhcr[,c("displaced_persons.from", "displaced_persons.to")],
  na.rm=T
)
# country_bigrams_unhcr$gdp_growth = 
#   (
# (country_bigrams_unhcr$gdp_growth.from * country_bigrams_unhcr$gdp.from) +
# (country_bigrams_unhcr$gdp_growth.to * country_bigrams_unhcr$gdp.to)
#   ) /
# (country_bigrams_unhcr$gdp.from + country_bigrams_unhcr$gdp.to)
country_bigrams_unhcr$prec_1 = rowMeans(
  country_bigrams_unhcr[,c("prec_1.from", "prec_1.to")],
  na.rm=T
)
country_bigrams_unhcr$prec_2 = rowMeans(
  country_bigrams_unhcr[,c("prec_2.from", "prec_2.to")],
  na.rm=T
)
country_bigrams_unhcr$prec_3 = rowMeans(
  country_bigrams_unhcr[,c("prec_3.from", "prec_3.to")],
  na.rm=T
)
country_bigrams_unhcr$prec_4 = rowMeans(
  country_bigrams_unhcr[,c("prec_4.from", "prec_4.to")],
  na.rm=T
)
country_bigrams_unhcr$prec_5 = rowMeans(
  country_bigrams_unhcr[,c("prec_5.from", "prec_5.to")],
  na.rm=T
)
country_bigrams_unhcr$prec_6 = rowMeans(
  country_bigrams_unhcr[,c("prec_6.from", "prec_6.to")],
  na.rm=T
)
country_bigrams_unhcr$prec_7 = rowMeans(
  country_bigrams_unhcr[,c("prec_7.from", "prec_7.to")],
  na.rm=T
)
country_bigrams_unhcr$prec_8 = rowMeans(
  country_bigrams_unhcr[,c("prec_8.from", "prec_8.to")],
  na.rm=T
)
country_bigrams_unhcr$prec_9 = rowMeans(
  country_bigrams_unhcr[,c("prec_9.from", "prec_9.to")],
  na.rm=T
)
country_bigrams_unhcr$prec_10 = rowMeans(
  country_bigrams_unhcr[,c("prec_10.from", "prec_10.to")],
  na.rm=T
)
country_bigrams_unhcr$prec_11 = rowMeans(
  country_bigrams_unhcr[,c("prec_11.from", "prec_11.to")],
  na.rm=T
)
country_bigrams_unhcr$prec_12 = rowMeans(
  country_bigrams_unhcr[,c("prec_12.from", "prec_12.to")],
  na.rm=T
)
country_bigrams_unhcr$tmax_1 = pmax(
  country_bigrams_unhcr$tmax_1.from, country_bigrams_unhcr$tmax_1.to,
  na.rm=T
)
country_bigrams_unhcr$tmax_2 = pmax(
  country_bigrams_unhcr$tmax_2.from, country_bigrams_unhcr$tmax_2.to,
  na.rm=T
)
country_bigrams_unhcr$tmax_3 = pmax(
  country_bigrams_unhcr$tmax_3.from, country_bigrams_unhcr$tmax_3.to,
  na.rm=T
)
country_bigrams_unhcr$tmax_4 = pmax(
  country_bigrams_unhcr$tmax_4.from, country_bigrams_unhcr$tmax_4.to,
  na.rm=T
)
country_bigrams_unhcr$tmax_5 = pmax(
  country_bigrams_unhcr$tmax_5.from, country_bigrams_unhcr$tmax_5.to,
  na.rm=T
)
country_bigrams_unhcr$tmax_6 = pmax(
  country_bigrams_unhcr$tmax_6.from, country_bigrams_unhcr$tmax_6.to,
  na.rm=T
)
country_bigrams_unhcr$tmax_7 = pmax(
  country_bigrams_unhcr$tmax_7.from, country_bigrams_unhcr$tmax_7.to,
  na.rm=T
)
country_bigrams_unhcr$tmax_8 = pmax(
  country_bigrams_unhcr$tmax_8.from, country_bigrams_unhcr$tmax_8.to,
  na.rm=T
)
country_bigrams_unhcr$tmax_9 = pmax(
  country_bigrams_unhcr$tmax_9.from, country_bigrams_unhcr$tmax_9.to,
  na.rm=T
)
country_bigrams_unhcr$tmax_10 = pmax(
  country_bigrams_unhcr$tmax_10.from, country_bigrams_unhcr$tmax_10.to,
  na.rm=T
)
country_bigrams_unhcr$tmax_11 = pmax(
  country_bigrams_unhcr$tmax_11.from, country_bigrams_unhcr$tmax_11.to,
  na.rm=T
)
country_bigrams_unhcr$tmax_12 = pmax(
  country_bigrams_unhcr$tmax_12.from, country_bigrams_unhcr$tmax_12.to,
  na.rm=T
)
country_bigrams_unhcr$tmin_1 = pmin(
  country_bigrams_unhcr$tmin_1.from, country_bigrams_unhcr$tmin_1.to,
  na.rm=T
)
country_bigrams_unhcr$tmin_2 = pmin(
  country_bigrams_unhcr$tmin_2.from, country_bigrams_unhcr$tmin_2.to,
  na.rm=T
)
country_bigrams_unhcr$tmin_3 = pmin(
  country_bigrams_unhcr$tmin_3.from, country_bigrams_unhcr$tmin_3.to,
  na.rm=T
)
country_bigrams_unhcr$tmin_4 = pmin(
  country_bigrams_unhcr$tmin_4.from, country_bigrams_unhcr$tmin_4.to,
  na.rm=T
)
country_bigrams_unhcr$tmin_5 = pmin(
  country_bigrams_unhcr$tmin_5.from, country_bigrams_unhcr$tmin_5.to,
  na.rm=T
)
country_bigrams_unhcr$tmin_6 = pmin(
  country_bigrams_unhcr$tmin_6.from, country_bigrams_unhcr$tmin_6.to,
  na.rm=T
)
country_bigrams_unhcr$tmin_7 = pmin(
  country_bigrams_unhcr$tmin_7.from, country_bigrams_unhcr$tmin_7.to,
  na.rm=T
)
country_bigrams_unhcr$tmin_8 = pmin(
  country_bigrams_unhcr$tmin_8.from, country_bigrams_unhcr$tmin_8.to,
  na.rm=T
)
country_bigrams_unhcr$tmin_9 = pmin(
  country_bigrams_unhcr$tmin_9.from, country_bigrams_unhcr$tmin_9.to,
  na.rm=T
)
country_bigrams_unhcr$tmin_10 = pmin(
  country_bigrams_unhcr$tmin_10.from, country_bigrams_unhcr$tmin_10.to,
  na.rm=T
)
country_bigrams_unhcr$tmin_11 = pmin(
  country_bigrams_unhcr$tmin_11.from, country_bigrams_unhcr$tmin_11.to,
  na.rm=T
)
country_bigrams_unhcr$tmin_12 = pmin(
  country_bigrams_unhcr$tmin_12.from, country_bigrams_unhcr$tmin_12.to,
  na.rm=T
)
country_bigrams_unhcr[,c(
  "to", "from",
  "displaced_persons.from", 
  # "gdp.from", "gdp_growth.from", 
  "prec_1.from",
  "prec_2.from", "prec_3.from", "prec_4.from", "prec_5.from", "prec_6.from", "prec_7.from", "prec_8.from",
  "prec_9.from", "prec_10.from", "prec_11.from", "prec_12.from", "tmin_1.from", "tmin_2.from", "tmin_3.from",
  "tmin_4.from", "tmin_5.from", "tmin_6.from", "tmin_7.from", "tmin_8.from", "tmin_9.from", "tmin_10.from",
  "tmin_11.from", "tmin_12.from", "tmax_1.from", "tmax_2.from", "tmax_3.from", "tmax_4.from", "tmax_5.from",
  "tmax_6.from", "tmax_7.from", "tmax_8.from", "tmax_9.from", "tmax_10.from", "tmax_11.from", "tmax_12.from",
  "displaced_persons.to", 
  # "gdp.to", "gdp_growth.to", 
  "prec_1.to",
  "prec_2.to", "prec_3.to", "prec_4.to", "prec_5.to", "prec_6.to", "prec_7.to", "prec_8.to",
  "prec_9.to", "prec_10.to", "prec_11.to", "prec_12.to", "tmin_1.to", "tmin_2.to", "tmin_3.to",
  "tmin_4.to", "tmin_5.to", "tmin_6.to", "tmin_7.to", "tmin_8.to", "tmin_9.to", "tmin_10.to",
  "tmin_11.to", "tmin_12.to", "tmax_1.to", "tmax_2.to", "tmax_3.to", "tmax_4.to", "tmax_5.to",
  "tmax_6.to", "tmax_7.to", "tmax_8.to", "tmax_9.to", "tmax_10.to", "tmax_11.to", "tmax_12.to"
)] = NULL
country_bigrams_unhcr = country_bigrams_unhcr[complete.cases(country_bigrams_unhcr),]
unhcr_pop_agg = rbindlist(list(unhcr_pop_agg, country_bigrams_unhcr), fill=T)
# unhcr_pop_agg$gdp = NULL

unhcr_pop_agg = unhcr_pop_agg[,c(
  "displaced_persons"
  # ,"gdp_growth"
  ,paste("prec",c(1:12),sep="_")
  ,paste("tmax",c(1:12),sep="_")
  ,"iso3"
  ,"year"
)]
fwrite(unhcr_pop_agg, "intermediate_data/displacement_worldclim2.csv")
