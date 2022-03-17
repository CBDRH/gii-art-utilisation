###############################################################################
# Imputations for the Gender Equality and ART utilisation analysis
# This script imputes data
#
# Contents:
# - Import
# - Exclusions
#   * See file .... for details
# - Preliminary linear imputations (interpolation)
# - MCMC imputations
# 
# Created by: Oisin Fitzgerald
###############################################################################

# Imputation parameters
# set to low values for testing (e.g. 2-5)
NBURN <- 2500 # 2500
NITER <- 2500 # 2500
NIMP <- 100 # 100
NIMP_PRED <- 20 # 20

## IMPORT ---------------------------------------------------------------------
## load packages
source("src/tools/gii_comp.R")
library(mitml)
library(lme4)
library(data.table)

# functions
logit <- function(x, d = 0) {
  x <- x + d
  log(x/(1-x))
}
logitinv <- function(x, d = 0) {
  y <- exp(x)/(exp(x) + 1)
  y - d
}

## import data
art_dat <- readRDS("data/art_covar_2002_2014.rds")
setDT(art_dat)

## results location
if (!dir.exists("data/imputations")) {
  dir.create("data/imputations")
}

## EXCLUDE ---------------------------------------------------------------------
# ## data quality
# countries to remove from analyses due to low data reporting rates
countries_remove <- c("SaudiArabia", "UnitedArabEmirates", "Jordan", "Lebanon", "Bahrain",
                      "SyrianArabRepublic", "Morocco",
                      "Tunisia", "Libya", "Palestine,Stateof", "Egypt",
                      "Taiwan")
art_dat$country_remove <- rep(0, nrow(art_dat))
art_dat$country_remove[art_dat$country %in% countries_remove] <- 1
art_dat <- art_dat[art_dat$country_remove == 0, ]
art_dat$country <- factor(art_dat$country)

## PRELIM ----------------------------------------------------------------------
# preliminary single value imputations where 4-8 values missing
## GNI
# missing GNI
miss_gni <- is.na(art_dat$ln_gni_wb)
gni_imp <- lmer(ln_gni_wb ~ 1 + time + (1 + time|country), data = art_dat)
gni_blups <- predict(gni_imp, newdata = art_dat[miss_gni])
art_dat[miss_gni,ln_gni_wb := gni_blups]
## Expected years of schooling
miss_sch <- is.na(art_dat$exp_sch_un)
sch_imp <- lmer(exp_sch_un ~ 1 + time + (1 + time|country), data = art_dat)
sch_blups <- predict(sch_imp, art_dat[miss_sch])
art_dat[miss_sch,exp_sch_un := sch_blups]
## Mean years of schooling
miss_sch <- is.na(art_dat$mean_sch_un)
sch_imp <- lmer(mean_sch_un ~ 1 + time + (1 + time|country), data = art_dat)
sch_blups <- predict(sch_imp, art_dat[miss_sch])
art_dat[miss_sch, mean_sch_un := sch_blups]
## HDI
miss_hdi <- is.na(art_dat$hdi_un)
hdi_imp <- lmer(hdi_un ~ 1 + time + (1 + time|country), data = art_dat)
hdi_blups <- predict(hdi_imp, art_dat[miss_sch])
art_dat[miss_hdi,hdi_un := hdi_blups]

## MCMC IMPUTATIONS ------------------------------------------------------------
### MCMC Imputation based analysis: ART ~ GII components

# prepare dataset
vars <- c("country", "time", "ln_art_utilisation", 
          "hdi_un",
          "ln_gni_wb",
          "female_lfp_wb","lfp_female_ilo", "lfp_male_ilo", "male_lfp_wb", "ratio_mfwork_wb", 
          "women_parl_wb",
          "ln_adol_birth_rate_wb",  "ln_maternal_mortality_wb", 
          "mean_sch_un","exp_sch_un", 
          "EA_3T8_AG25T99_F", "EA_3T8_AG25T99_M", "EA_2T8_AG25T99_F", "EA_2T8_AG25T99_M")
art_dat2 <- art_dat[,..vars]

# summary info and missingness (art, education, then labour data the worst)
summary(art_dat2)

### data transformations for the imputations
# transform some variables so on real line
art_dat2[,women_parl_wb := logit(women_parl_wb,d=0.01)]
art_dat2[,EA_3T8_AG25T99_F := logit(EA_3T8_AG25T99_F/100)]
art_dat2[,EA_3T8_AG25T99_M := logit(EA_3T8_AG25T99_M/100)]
art_dat2[,EA_2T8_AG25T99_F := logit(EA_2T8_AG25T99_F/100,d=-0.01)]
art_dat2[,EA_2T8_AG25T99_M := logit(EA_2T8_AG25T99_M/100,d=-0.01)]
art_dat2[,female_lfp_wb := logit(female_lfp_wb)]
art_dat2[,male_lfp_wb := logit(male_lfp_wb)]
art_dat2[,lfp_female_ilo := logit(lfp_female_ilo)]
art_dat2[,lfp_male_ilo := logit(lfp_male_ilo)]
art_dat2[,hdi_un := logit(hdi_un)]
art_dat2[,ratio_mfwork_wb := ratio_mfwork_wb/100]
summary(art_dat2)
sapply(art_dat2, function(x) sum(is.nan(x)))
sapply(art_dat2, function(x) sum(is.infinite(x)))

### 1. Generate Imputations
rhs <- c("hdi_un","time","female_lfp_wb","ln_gni_wb","ln_maternal_mortality_wb",
         "ln_adol_birth_rate_wb", "ratio_mfwork_wb","mean_sch_un","exp_sch_un")
lhs <- c("EA_2T8_AG25T99_F","EA_2T8_AG25T99_M","women_parl_wb","male_lfp_wb",
         "lfp_female_ilo", "lfp_male_ilo")
art_dat2[,id := as.numeric(as.factor(country))]
fml1 <- as.formula(paste(paste0(lhs,collapse="+"),"~1+",paste0(rhs,collapse="+"),"+(1|id)"))
# imputation
imp1 <- jomoImpute(art_dat2, formula=fml1, n.burn=NBURN, n.iter=NITER, m=NIMP)
summary(imp1)
# to a list
implist1 <- mitmlComplete(imp1, "all")
# retransform the variables
implist11 <- lapply(implist1, function(x) {
  out <- x
  # transforms
  out$women_parl_wb <- logitinv(out$women_parl_wb,d=0.01)
  out$EA_3T8_AG25T99_F <- logitinv(out$EA_3T8_AG25T99_F)
  out$EA_3T8_AG25T99_M <- logitinv(out$EA_3T8_AG25T99_M)
  out$EA_2T8_AG25T99_F <- logitinv(out$EA_2T8_AG25T99_F,d=-0.01)
  out$EA_2T8_AG25T99_M <- logitinv(out$EA_2T8_AG25T99_M,d=-0.01)
  out$lfp_female_ilo <- logitinv(out$lfp_female_ilo)
  out$lfp_male_ilo <- logitinv(out$lfp_male_ilo)
  out$female_lfp_wb <- logitinv(out$female_lfp_wb)
  out$male_lfp_wb <- logitinv(out$male_lfp_wb)
  out$hdi_un <- logitinv(out$hdi_un)
  # calculate gii
  out$gii <- calc_gii(exp(out$ln_maternal_mortality_wb),
                      exp(out$ln_adol_birth_rate_wb),
                      100*(out$female_lfp_wb),
                      100*(out$male_lfp_wb),
                      100*(out$EA_2T8_AG25T99_F),
                      100*(out$EA_2T8_AG25T99_M),
                      100*(out$women_parl_wb))
  out
})
class(implist11) <- c("mitml.list","list")
summary(implist11[[2]])
# save
save(implist11, file= "data/imputations/implist11.RData")

### 2. Generate Imputations (impute art utilisation)
rhs <- c("hdi_un","time","female_lfp_wb","ln_gni_wb","ln_maternal_mortality_wb",
         "ln_adol_birth_rate_wb", "ratio_mfwork_wb","mean_sch_un","exp_sch_un")
lhs <- c("EA_2T8_AG25T99_F","EA_2T8_AG25T99_M","women_parl_wb","male_lfp_wb",
         "lfp_female_ilo", "lfp_male_ilo","ln_art_utilisation")
art_dat2[,id := as.numeric(as.factor(country))]
fml2 <- as.formula(paste(paste0(lhs,collapse="+"),"~1+",paste0(rhs,collapse="+"),"+(1|id)"))
# imputation
imp2 <- jomoImpute(art_dat2, formula=fml2, n.burn=NBURN, n.iter=NITER, m=NIMP)
summary(imp2)
# to a list
implist2 <- mitmlComplete(imp2, "all")
# to a list
implist2 <- mitmlComplete(imp2, "all")
# retransform the variables
implist22 <- lapply(implist2, function(x) {
  out <- x
  # transforms
  out$women_parl_wb <- logitinv(out$women_parl_wb,d=0.01)
  out$EA_3T8_AG25T99_F <- logitinv(out$EA_3T8_AG25T99_F)
  out$EA_3T8_AG25T99_M <- logitinv(out$EA_3T8_AG25T99_M)
  out$EA_2T8_AG25T99_F <- logitinv(out$EA_2T8_AG25T99_F)
  out$EA_2T8_AG25T99_M <- logitinv(out$EA_2T8_AG25T99_M)
  out$lfp_female_ilo <- logitinv(out$lfp_female_ilo)
  out$lfp_male_ilo <- logitinv(out$lfp_male_ilo)
  out$female_lfp_wb <- logitinv(out$female_lfp_wb)
  out$male_lfp_wb <- logitinv(out$male_lfp_wb)
  out$hdi_un <- logitinv(out$hdi_un)
  # calculate gii
  out$gii <- calc_gii(exp(out$ln_maternal_mortality_wb),
                      exp(out$ln_adol_birth_rate_wb),
                      100*(out$female_lfp_wb),
                      100*(out$male_lfp_wb),
                      100*(out$EA_2T8_AG25T99_F),
                      100*(out$EA_2T8_AG25T99_M),
                      100*(out$women_parl_wb))
  out
})
class(implist22) <- c("mitml.list","list")
summary(implist22[[1]])
# save
save(implist22, file= "data/imputations/implist22.RData")

### 3. Generate Imputations for predicting (impute art utilisation)
art_dat3 <- art_dat2[time < 8, ]  # use first 7 years
# imputation
imp3 <- jomoImpute(art_dat3, formula=fml2, n.burn=NBURN, n.iter=NITER, m=NIMP_PRED)
summary(imp3)
# to a list
implist3 <- mitmlComplete(imp3, "all")
# to a list
implist3 <- mitmlComplete(imp3, "all")
# retransform the variables
implist33 <- lapply(implist3, function(x) {
  out <- x
  # transforms
  out$women_parl_wb <- logitinv(out$women_parl_wb,d=0.01)
  out$EA_3T8_AG25T99_F <- logitinv(out$EA_3T8_AG25T99_F)
  out$EA_3T8_AG25T99_M <- logitinv(out$EA_3T8_AG25T99_M)
  out$EA_2T8_AG25T99_F <- logitinv(out$EA_2T8_AG25T99_F)
  out$EA_2T8_AG25T99_M <- logitinv(out$EA_2T8_AG25T99_M)
  out$lfp_female_ilo <- logitinv(out$lfp_female_ilo)
  out$lfp_male_ilo <- logitinv(out$lfp_male_ilo)
  out$female_lfp_wb <- logitinv(out$female_lfp_wb)
  out$male_lfp_wb <- logitinv(out$male_lfp_wb)
  out$hdi_un <- logitinv(out$hdi_un)
  # calculate gii
  out$gii <- calc_gii(exp(out$ln_maternal_mortality_wb),
                      exp(out$ln_adol_birth_rate_wb),
                      100*(out$female_lfp_wb),
                      100*(out$male_lfp_wb),
                      100*(out$EA_2T8_AG25T99_F),
                      100*(out$EA_2T8_AG25T99_M),
                      100*(out$women_parl_wb))
  out
})
class(implist33) <- c("mitml.list","list")
# save
save(implist33, file= "data/imputations/implist33.RData")
