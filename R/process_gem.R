list.of.packages <- c("data.table","reshape2", "sp","rgdal","rgeos","maptools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

faults_path = "gem-global-active-faults/shapefile/gem_active_faults_harmonized.shp"
faults = readOGR(faults_path)

world_path = "ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp"
world = readOGR(world_path)

overlap = over(faults, world)
overlap$ISO_A3[which(overlap$NAME=="Kosovo")] = "XXK"
overlap_iso3 = overlap$ISO_A3
faults_df = faults@data
faults_df$iso3 = overlap_iso3

faults_agg = data.table(faults_df)[,.(count=.N), by=.(iso3)]
faults_agg = subset(faults_agg, !is.na(iso3) & iso3!="-99")
save(faults_agg, file="intermediate_data/processed_faults.RData")
