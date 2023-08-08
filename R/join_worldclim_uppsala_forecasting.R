list.of.packages <- c("data.table", "reshape2", "zoo")
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
conflict_w = conflict_w[,c(
  "conflict",
  "scenario",
  "GDPcap",
  "Pop",
  "YMHEP",
  "iso3",
  "year"
)]
conflict_w$conflict[which(conflict_w$conflict==2)] = 1

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

conflict = merge(conflict_w, worldclim)

conflict <- conflict %>%                           
  group_by(scenario, iso3) %>%
  dplyr::mutate(
    gdp = GDPcap * Pop,
    gdpgrowth = (gdp - dplyr::lag(gdp, n = 1, default = NA))  / dplyr::lag(gdp, n = 1, default = NA)
  )
conflict = subset(conflict, !is.na(gdpgrowth))

conflict = conflict[,c(
  "conflict"
  ,"scenario"
  ,"gdpgrowth"
  # ,"Pop"
  # ,"YMHEP"
  ,paste("prec",c(1:12),sep="_")
  # ,paste("tmin",c(1:12),sep="_")
  ,paste("tmax",c(1:12),sep="_")
  ,"iso3"
  ,"year"
)]
training = fread("intermediate_data/conflict_clim.csv")
out_of_set = setdiff(conflict$iso3, training$iso3)
conflict = subset(conflict, !iso3 %in% out_of_set)
fwrite(conflict,"intermediate_data/large/conflict_clim_forecasting.csv")
fwrite(conflict,"intermediate_data/large/conflict_clim_bigram_forecasting.csv")
