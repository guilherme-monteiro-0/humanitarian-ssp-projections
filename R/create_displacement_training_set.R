list.of.packages <- c("data.table", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

load("intermediate_data/iiasa.RData")

unhcr_pop = fread("./UNHCR/population.csv", skip=14)
keep = c("Year", "Country of asylum (ISO)", "Refugees under UNHCR's mandate")
unhcr_pop = unhcr_pop[,keep, with=F]
names(unhcr_pop) = c("year", "Region", "refugees")
unhcr_pop = unhcr_pop[,.(refugees=sum(refugees,na.rm=T)), by=.(year, Region)]

training = merge(unhcr_pop, iiasa, by=c("year", "Region"), all=T)
training = subset(training, Scenario=="SSP1" & year <= 2022)
training = training[,c("refugees","Region","year","pop","gdp","urban")]
training$refugees[which(is.na(training$refugees))] = 0
fwrite(training, "intermediate_data/iiasa_unhcr_refugees.csv")
training_l = melt(training, id.vars=c("refugees", "Region","year"))
trailing_l = training_l[complete.cases(training_l),]
fwrite(training_l, "intermediate_data/iiasa_unhcr_refugees_long.csv")