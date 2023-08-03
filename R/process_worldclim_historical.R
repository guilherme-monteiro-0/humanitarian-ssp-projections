list.of.packages <- c("data.table","reshape2", "sp","rgdal","rgeos",
                      "maptools", "sf", "leaflet", "geosphere", "s2", "raster"
                      , "foreach", "doSNOW","snow", "doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

load(file="./intermediate_data/land_and_sea.RData")
land_and_sea$ISO_A3[which(land_and_sea$ISO_A3=="ZAR")] = "COD"
land_and_sea$ISO_A3[which(land_and_sea$ISO_A3=="ROM")] = "ROU"
land_and_sea$ISO_A3[which(land_and_sea$ISO_A3=="TMP")] = "TLS"
land_and_sea$ISO_A3[which(land_and_sea$ISO_A3=="ADO")] = "AND"

countries = subset(land_and_sea, !startsWith(ISO_A3, "WB"))
min10 = raster::raster("./WorldClim/ACCESS-CM2/sample/wc2.1_10m_prec_1960-01.tif")
countries_raster = raster::rasterize(countries, min10)

historical_zips = list.files(
  path="./WorldClim/ACCESS-CM2/historical/",
  pattern="*.zip",
  full.names = T
)

data_list = list()
data_index = 1

data_funcs = function(x, vars){
  var = unique(vars)
  if(var == "prec"){
    return(mean(x, na.rm=T))
  }
  if(var == "tmax"){
    return(max(x, na.rm=T))
  }
  return(min(x, na.rm=T))
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

for(historical_zip in historical_zips){
  message(historical_zip)
  unzip(historical_zip, exdir="./WorldClim_tmp")
  tifs = list.files(path="./WorldClim_tmp", pattern="*.tif", full.names=T)
  pb = txtProgressBar(max=length(tifs), style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  zip_data = foreach(i=1:length(tifs), .combine = rbind, .options.snow = opts) %dopar% {
    tif = tifs[i]
    varname = substr(tif, 27, 30)
    year = substr(tif, 32, 35)
    month = substr(tif, 37, 38)
    tif_raster = raster::raster(tif)
    values_df = data.table::data.table(
      ISO_A3=countries$ISO_A3[raster::values(countries_raster)],
      value=raster::values(tif_raster),
      variable=varname
    )[,.(value=data_funcs(value, variable)), by=.(ISO_A3, variable)]
    values_df$year = year
    values_df$month = month
    return(values_df)
  }
  close(pb)
  data_list[[data_index]] = zip_data
  data_index = data_index + 1
  unlink("./WorldClim_tmp", recursive=T)
}

stopCluster(parallelCluster)

worldclim_hist = rbindlist(data_list)
worldclim_hist$month = as.numeric(worldclim_hist$month)
worldclim_hist = worldclim_hist[which(!is.na(worldclim_hist$ISO_A3)),]
worldclim_hist = dcast(worldclim_hist, ISO_A3+year~variable+month)
worldclim_hist = subset(worldclim_hist, !is.nan(prec_1))
fwrite(worldclim_hist, "./WorldClim/ACCESS-CM2/processed/historical.csv")
