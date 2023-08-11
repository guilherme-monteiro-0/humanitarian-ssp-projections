list.of.packages <- c("data.table","jsonlite","httr", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))
# 
# source("R/hpc_caseload_api.R")
# 
# # List to collect FTS plan data
# plan_list = list()
# plan_index = 1
# 
# # Only found plans for these complete years
# # Some discussion on how to parse this, (e.g. spread reqs over multiple locs/years)
# years = c(2018:2023)
# pb = txtProgressBar(min=2018,max=2023,style=3)
# for(year in years){
#   setTxtProgressBar(pb, year)
#   url = paste0("https://api.hpc.tools/v1/public/plan/year/", year)
#   json_data = content(GET(url))$data
#   for(i in 1:length(json_data)){
#     row = json_data[[i]]
#     plan_id = row$id
#     hpc = tryCatch({hpc_api_all(plan_id)}, error = function(e) {
#       return(NULL)
#     })
#     if(is.null(hpc)){
#       next
#     }
#     if(nrow(hpc) == 0){
#       next
#     }
#     original_requirements = row$origRequirements
#     revised_requirements = row$revisedRequirements
#     plan_name = row$planVersion$name
#     locations = row$locations
#     emergencies = row$emergencies
#     # Try to base rows on plan locations, failing that, emergencies
#     # failing that, just return the name
#     if(length(locations)>0){
#       for(j in 1:length(locations)){
#         location = locations[[j]]
#         location_id = location$id
#         location_name = location$name
#         location_iso3 = location$iso3
#         if(is.null(location_iso3)){
#           location_iso3 = NA
#         }
#         location_pcode = location$pcode
#         if(is.null(location_pcode)){
#           location_pcode = NA
#         }
#         location_admin_level = location$adminLevel
#         location_lat = location$latitude
#         if(is.null(location_lat)){
#           location_lat = NA
#         }
#         location_lon = location$longitude
#         if(is.null(location_lon)){
#           location_lon = NA
#         }
#         tmp = data.frame(
#           year,
#           plan_id,
#           plan_name,
#           original_requirements,
#           revised_requirements,
#           location_id,
#           location_name,
#           location_iso3,
#           location_pcode,
#           location_admin_level,
#           location_lat,
#           location_lon
#         )
#         tmp = cbind(tmp, hpc)
#         plan_list[[plan_index]] = tmp
#         plan_index = plan_index + 1
#       }
#     } else if(length(emergencies) > 0) {
#       for(j in 1:length(emergencies)){
#         emergency = emergencies[[j]]
#         emergency_id = emergency$id
#         emergency_name = emergency$name
#         emergency_glide_id = emergency$glideId
#         if(is.null(emergency_glide_id)){
#           emergency_glide_id = NA
#         }
#         tmp = data.frame(
#           year,
#           plan_id,
#           plan_name,
#           original_requirements,
#           revised_requirements,
#           emergency_id,
#           emergency_name,
#           emergency_glide_id
#         )
#         tmp = cbind(tmp, hpc)
#         plan_list[[plan_index]] = tmp
#         plan_index = plan_index + 1
#       }
#     }else{
#       tmp = data.frame(
#         year,
#         plan_id,
#         plan_name,
#         original_requirements,
#         revised_requirements
#       )
#       tmp = cbind(tmp, hpc)
#       plan_list[[plan_index]] = tmp
#       plan_index = plan_index + 1
#     }
#   }
# }
# close(pb)
# 
# fts_plans = rbindlist(plan_list, fill=T)
# fts_plans = subset(fts_plans, metric_id=="inNeed" & sector_id==0)
# fts_plans = subset(fts_plans, !is.na(location_iso3))
# fts_plans = subset(fts_plans, !is.na(value))
# fts_plans = subset(fts_plans, !grepl("covid", plan_name, ignore.case=T))
# keep = c(
#   "plan_id",
#   "location_iso3",
#   "year",
#   "value"
# )
# fts_plans = fts_plans[,keep, with=F]
# fts_plans[,value:=value/.N, by=.(plan_id)]
# fts_plans$plan_id = NULL
# fts_plans = fts_plans[,.(value=sum(value)), by=.(location_iso3, year)]
# setnames(fts_plans, "location_iso3", "iso3")
# setnames(fts_plans, "value", "humanitarian_needs")
# save(fts_plans, file="fts/plans_people_in_need.RData")
# fwrite(fts_plans, "intermediate_data/fts_plans_people_in_need.RData")

pin = fread("fts/f2.1_total_pin.csv")
pin = pin[,c("iso3","countryname", "pin_2018", "pin_2019", "pin_2020", "pin_2021", "pin_2022", "pin_2023")]
pin = melt(pin, id.vars=c("iso3", "countryname"))
pin$year = as.numeric(substr(pin$variable, 5, 8))
pin = pin[,c("iso3","year","value")]
pin = subset(pin, !is.na(value))

covid_pin = fread("fts/covid_plans_pin.csv")
names(covid_pin) = c("plans", "iso3", "covid_value")
covid_pin = covid_pin[,c("iso3", "covid_value")]
covid_pin$year = 2020
covid_pin$covid_value = as.numeric(gsub(",", "", covid_pin$covid_value)) / 1e6
covid_pin = subset(covid_pin, !is.na(covid_value))
pin = merge(pin, covid_pin, all.x=T)
pin[is.na(pin)] = 0
pin$value = pin$value - pin$covid_value
pin$covid_value = NULL
fwrite(pin, "intermediate_data/pin.csv")