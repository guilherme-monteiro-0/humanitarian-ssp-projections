list.of.packages <- c("data.table","plm","Hmisc","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

load("fts/plans.RData")

fts_plans = subset(fts_plans,!is.na(location_iso3) & original_requirements > 0)
fts_aggregate = fts_plans[,.(humanitarian_needs=sum(original_requirements,na.rm=T)),by=.(year,location_iso3)]
setnames(fts_aggregate,"location_iso3", "iso3")

load("INFORM/interpolated_inform.RData")
inform = subset(inform, Scenario=="Historical")
setnames(inform, c("Iso3", "Year"), c("iso3", "year"))
inform[,c("Scenario")] = NULL
inform_m = melt(inform, id.vars=c("iso3", "year", "IndicatorId"), measure.vars = "IndicatorScore")
inform_w = dcast(inform_m, iso3+year~IndicatorId)
analysis_set = merge(inform_w, fts_aggregate, by=c("iso3", "year"), all.x=T)
analysis_set$humanitarian_needs[which(is.na(analysis_set$humanitarian_needs))] = 0

# Pooled OLS
fit = lm(humanitarian_needs~
    AFF_DR_REL+
    CON_HIIK_NP+
    CON_HIIK_SN+
    EX_EQ_MMI6_REL+
    EX_EQ_MMI8_REL+
    EX_FL_REL+
    EX_TC_SS1_REL+
    EX_TC_SS3_REL+
    EX_TS_REL+
    POP_DEN
, data=analysis_set)
summary(fit)

# FE
fit_plm = plm(humanitarian_needs~
                AFF_DR_REL+
                CON_HIIK_NP+
                CON_HIIK_SN+
                EX_EQ_MMI6_REL+
                EX_EQ_MMI8_REL+
                EX_FL_REL+
                EX_TC_SS1_REL+
                EX_TC_SS3_REL+
                EX_TS_REL+
                POP_DEN
, data=analysis_set, index=c("iso3", "year"), model="within")
summary(fit_plm)
pFtest(fit_plm, fit) # Significant effects

# Logit
analysis_set$humanitarian = analysis_set$humanitarian_needs > 0
describe(analysis_set$humanitarian)
logit <- glm(humanitarian~
               AFF_DR_REL+
               CON_HIIK_NP+
               CON_HIIK_SN+
               EX_EQ_MMI6_REL+
               EX_EQ_MMI8_REL+
               EX_FL_REL+
               EX_TC_SS1_REL+
               EX_TC_SS3_REL+
               EX_TS_REL+
               POP_DEN
, data=analysis_set, family = "binomial")
summary(logit)
with(logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(logit)
nullhypo <- glm(humanitarian~1, data=analysis_set, family="binomial")
mcFadden = 1-logLik(logit)/logLik(nullhypo)
mcFadden
