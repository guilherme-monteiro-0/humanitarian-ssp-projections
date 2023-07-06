list.of.packages <- c("data.table","plm","Hmisc")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

load("uppsala_replication/PredictionSSP_1.RData")
load("fts/plans.RData")
u_iso3 = fread("supporting_data/uppsala_iso3.csv")
PredictionSSP_1 = merge(PredictionSSP_1, u_iso3, by="countryname")
PredictionSSP_1 = subset(PredictionSSP_1, year>=2000)

fts_plans = subset(fts_plans,!is.na(location_iso3) & original_requirements > 0)
fts_aggregate = fts_plans[,.(humanitarian_needs=sum(original_requirements,na.rm=T)),by=.(year,location_iso3)]
setnames(fts_aggregate,"location_iso3", "iso3")

analysis_set = merge(PredictionSSP_1, fts_aggregate, by=c("iso3", "year"), all.x=T)
analysis_set$humanitarian_needs[which(is.na(analysis_set$humanitarian_needs))] = 0

# Pooled OLS
fit = lm(humanitarian_needs~
           conflict+
           temp+
           nb_conflict+
           YMHEP+
           lpop+
           lGDPcap
, data=analysis_set)
summary(fit)

# FE
fit_plm = plm(humanitarian_needs~
                conflict+
                temp+
                nb_conflict+
                YMHEP+
                lpop+
                lGDPcap
, data=analysis_set, index=c("iso3", "year"), model="within")
summary(fit_plm)
pFtest(fit_plm, fit) # Significant effects

# Logit
analysis_set$humanitarian = analysis_set$humanitarian_needs > 0
describe(analysis_set$humanitarian)
logit <- glm(humanitarian~
               conflict+
               temp+
               nb_conflict+
               YMHEP+
               lpop+
               lGDPcap
, data=analysis_set, family = "binomial")
summary(logit)
with(logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(logit)
