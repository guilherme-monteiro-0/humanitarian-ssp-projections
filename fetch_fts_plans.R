list.of.packages <- c("data.table","jsonlite","httr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

# List to collect FTS plan data
plan_list = list()
plan_index = 1

# Only found plans for these complete years
# Some discussion on how to parse this, (e.g. spread reqs over multiple locs/years)
years = c(1999:2022)
pb = txtProgressBar(min=1999,max=2022,style=3)
for(year in years){
  setTxtProgressBar(pb, year)
  url = paste0("https://api.hpc.tools/v1/public/plan/year/", year)
  json_data = content(GET(url))$data
  for(i in 1:length(json_data)){
    row = json_data[[i]]
    plan_id = row$id
    original_requirements = row$origRequirements
    revised_requirements = row$revisedRequirements
    plan_name = row$planVersion$name
    locations = row$locations
    emergencies = row$emergencies
    # Try to base rows on plan locations, failing that, emergencies
    # failing that, just return the name
    if(length(locations)>0){
      for(j in 1:length(locations)){
        location = locations[[j]]
        location_id = location$id
        location_name = location$name
        location_iso3 = location$iso3
        if(is.null(location_iso3)){
          location_iso3 = NA
        }
        location_pcode = location$pcode
        if(is.null(location_pcode)){
          location_pcode = NA
        }
        location_admin_level = location$adminLevel
        location_lat = location$latitude
        if(is.null(location_lat)){
          location_lat = NA
        }
        location_lon = location$longitude
        if(is.null(location_lon)){
          location_lon = NA
        }
        tmp = data.frame(
          year,
          plan_id,
          plan_name,
          original_requirements,
          revised_requirements,
          location_id,
          location_name,
          location_iso3,
          location_pcode,
          location_admin_level,
          location_lat,
          location_lon
        )
        plan_list[[plan_index]] = tmp
        plan_index = plan_index + 1
      }
    } else if(length(emergencies) > 0) {
      for(j in 1:length(emergencies)){
        emergency = emergencies[[j]]
        emergency_id = emergency$id
        emergency_name = emergency$name
        emergency_glide_id = emergency$glideId
        if(is.null(emergency_glide_id)){
          emergency_glide_id = NA
        }
        tmp = data.frame(
          year,
          plan_id,
          plan_name,
          original_requirements,
          revised_requirements,
          emergency_id,
          emergency_name,
          emergency_glide_id
        )
        plan_list[[plan_index]] = tmp
        plan_index = plan_index + 1
      }
    }else{
      tmp = data.frame(
        year,
        plan_id,
        plan_name,
        original_requirements,
        revised_requirements
      )
      plan_list[[plan_index]] = tmp
      plan_index = plan_index + 1
    }
  }
}
close(pb)

fts_plans = rbindlist(plan_list, fill=T)
save(fts_plans, file="fts/plans.RData")
which(duplicated(fts_plans[,c("plan_id", "location_id", "emergency_id")]))
