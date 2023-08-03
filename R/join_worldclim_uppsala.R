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
    gdp = GDPcap * Pop,
    gdpgrowth = (gdp - dplyr::lag(gdp, n = 1, default = NA))  / dplyr::lag(gdp, n = 1, default = NA)
  )
conflict = subset(conflict, !is.na(gdpgrowth))

conflict = conflict[,c(
  "conflict"
  ,"gdpgrowth"
  # ,"Pop"
  # ,"YMHEP"
  ,paste("prec",c(1:12),sep="_")
  ,paste("tmin",c(1:12),sep="_")
  ,paste("tmax",c(1:12),sep="_")
  ,"iso3"
  ,"year"
)]
fwrite(conflict,"intermediate_data/conflict_clim.csv")
