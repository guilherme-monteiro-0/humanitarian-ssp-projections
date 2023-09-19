list.of.packages <- c("data.table", "reshape2", "igraph", "dplyr", "foreach", "doSNOW","snow", "doParallel", "zoo")
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

worldclim_hist = fread("./WorldClim/ACCESS-CM2/processed/historical.csv")
setnames(worldclim_hist,"ISO_A3", "iso3")
worldclim_future = fread("./WorldClim/ACCESS-CM2/processed/future.csv")
setnames(worldclim_future,"ISO_A3", "iso3")
worldclim_hist = subset(worldclim_hist, iso3 %in% unique(worldclim_future$iso3))
worldclim_grid = expand.grid(
  scenario=unique(worldclim_future$scenario),
  iso3=unique(worldclim_hist$iso3),
  year=unique(worldclim_hist$year)
)
worldclim_hist = merge(worldclim_hist, worldclim_grid, all=T)
worldclim = rbindlist(list(worldclim_hist, worldclim_future), use.names=T)

worldclim_grid2 = expand.grid(
  scenario=unique(worldclim$scenario),
  iso3=unique(worldclim$iso3),
  year=min(worldclim$year):max(worldclim$year)
)
worldclim = merge(worldclim, worldclim_grid2, all=T)
worldclim = worldclim[order(worldclim$scenario, worldclim$iso3, worldclim$year),]
worldclim = worldclim[,.(
  prec_1=na.approx(prec_1),
  prec_2=na.approx(prec_2),
  prec_3=na.approx(prec_3),
  prec_4=na.approx(prec_4),
  prec_5=na.approx(prec_5),
  prec_6=na.approx(prec_6),
  prec_7=na.approx(prec_7),
  prec_8=na.approx(prec_8),
  prec_9=na.approx(prec_9),
  prec_10=na.approx(prec_10),
  prec_11=na.approx(prec_11),
  prec_12=na.approx(prec_12),
  tmax_1=na.approx(tmax_1),
  tmax_2=na.approx(tmax_2),
  tmax_3=na.approx(tmax_3),
  tmax_4=na.approx(tmax_4),
  tmax_5=na.approx(tmax_5),
  tmax_6=na.approx(tmax_6),
  tmax_7=na.approx(tmax_7),
  tmax_8=na.approx(tmax_8),
  tmax_9=na.approx(tmax_9),
  tmax_10=na.approx(tmax_10),
  tmax_11=na.approx(tmax_11),
  tmax_12=na.approx(tmax_12),
  tmin_1=na.approx(tmin_1),
  tmin_2=na.approx(tmin_2),
  tmin_3=na.approx(tmin_3),
  tmin_4=na.approx(tmin_4),
  tmin_5=na.approx(tmin_5),
  tmin_6=na.approx(tmin_6),
  tmin_7=na.approx(tmin_7),
  tmin_8=na.approx(tmin_8),
  tmin_9=na.approx(tmin_9),
  tmin_10=na.approx(tmin_10),
  tmin_11=na.approx(tmin_11),
  tmin_12=na.approx(tmin_12),
  year=year
), by=.(scenario, iso3)]


worldclim = subset(worldclim, year > 1992)

forecasting = merge(worldclim, unhcr_pop_agg, by=c("year", "iso3"), all.x=T)
forecasting = forecasting[,c(
  "displaced_persons"
  ,"scenario"
  ,paste("prec",c(1:12),sep="_")
  ,paste("tmax",c(1:12),sep="_")
  ,"iso3"
  ,"year"
)]
fwrite(forecasting, "intermediate_data/large/displacement_worldclim2_forecasting.csv")
