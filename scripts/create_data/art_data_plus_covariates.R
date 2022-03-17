################################################################################
# make a ART utilisation between 2002 and 2010 AND covariates dataset 
# data source: 1. ICMART world reports on ART (see data/rawdata folder)
#              2. UN
#              3. World Bank
# 
# 26/7/17 -> updated 14/09/2021
################################################################################

## -----------------------------------------------------------------------------
## SETUP
# load packages
library(data.table)

## functions for reading in covariates
read_and_melt1 <- function(file, years, var_name) {
  # Read data function for the World Bank data
  # function for reading in and melting data
  dat <- fread(file,header=TRUE)
  years <- as.numeric(names(dat)[-1])
  names(dat) <- c("country", years)
  dat <- melt(dat,id.vars = 1,variable.factor = FALSE)
  names(dat) <- c("country", "year", var_name)
  dat[,year := as.numeric(as.character(year))]
  dat[[var_name]] <- as.numeric(dat[[var_name]])
  # fix south korea, egypt, macedonia, venuzuela, bolivia
  dat[country %in% c("Venezuela, RB","Venezuela,RB","Venezuela(BolivarianRepublicof)"),country:="Venezuela"]
  dat[country %in% c("Egypt, Arab Rep.", "Egypt,ArabRep."),country:="Egypt"]
  dat[country %in% c("Macedonia, FYR", "Macedonia,FYR"),country:="Macedonia"]
  dat[country %in% c("Korea, Rep.","Korea,Rep."),country:="SouthKorea"]
  dat[country %in% c("RussianFederation","Russian"),country:="Russia"]
  dat[country %in% c("Moldova(Republicof)"),country:="Moldova"]
  dat[country %in% c("Coted'Ivoire"),country:="IvoryCoast"]
  setkeyv(dat,c("country","year"))
  dat
}
read_and_melt2 <- function(file, var_name) {
  # Read data function for the World Bank data
  # function for reading in and melting data
  dat <- fread(file,header=TRUE)
  dat <- melt(dat, id.vars = 1,variable.factor = FALSE)
  names(dat) <- c("country", "year", var_name)
  dat[[var_name]] <- gsub("ee","",dat[[var_name]])
  dat[[var_name]] <- as.numeric(dat[[var_name]])
  dat[,year := as.numeric(year)]
  # fix south korea, egypt, macedonia, venuzuela, bolivia
  dat[country == "Bolivia(PlurinationalStateof)",country:="Bolivia"]
  dat[country %in% c("Venezuela, RB","Venezuela,RB","Venezuela(BolivarianRepublicof)"),country:="Venezuela"]
  dat[country == "Moldova(Republicof)",country:="Moldova"]
  dat[country == "TheformerYugoslavRepublicofMacedonia",country:="Macedonia"]
  dat[country == "Korea(Republicof)",country:="SouthKorea"]
  dat[country %in% c("RussianFederation","Russian"),country:="Russia"]
  dat[country %in% c("Coted'Ivoire"),country:="IvoryCoast"]
  setkeyv(dat,c("country","year"))
  dat
}

## read in ART datasets --------------------------------------------------------
#art_dat_wide <- fread("data/art_2002_2014_wide.csv")
if(!file.exists("data/art_2002_2014.csv")) {
  source("scripts/create_data/art_data.R")
}
art_dat <- fread("data/art_2002_2014.csv")

art_dat[country %in% c("Venezuela(BolivarianRepublicof)"),country:="Venezuela"]
art_dat[country %in% c("Moldova(Republicof)"),country:="Moldova"]
art_dat[country %in% c("RussianFederation","Russian"),country:="Russia"]


setkeyv(art_dat,c("country","year"))

## World bank data -------------------------------------------------------------

# female labour force - WB
lab_file <- "data/rawdata/wb/Labour force participation rate, female (% ages 15 and older) WB.csv"
labour_datwb <- read_and_melt1(lab_file, 1992:2016, "female_lfp_wb")
# male labour force - WB
lab_file <- "data/rawdata/wb/Labor force participation rate, male (% of male population ages 15+) .csv"
labour_datwb_m <- read_and_melt1(lab_file, 1992:2016, "male_lfp_wb")
# adolescent birth rate
adol_file <- "data/rawdata/wb/Adolescent birth rate  WB.csv"
adol_dat <- read_and_melt1(adol_file, 1992:2015, "adol_birth_rate_wb")
# women in parliament
parl_file <- "data/rawdata/wb/proportion women parliament WB.csv"
parl_dat <- read_and_melt1(parl_file, 1997:2016, "women_parl_wb")
# maternal mortality
mort_file <- "data/rawdata/wb/maternal mortality WB.csv"
mort_dat <- read_and_melt1(mort_file, 1992:2015, "maternal_mortality_wb")
# maternal mortality
fert_file <- "data/rawdata/wb/fertility rate WB.csv"
fert_dat <- read_and_melt1(fert_file, 1993:2015, "fertility_rate_wb")
# employment services
services_file <- "data/rawdata/wb/employment services female WB.csv"
services_dat <- read_and_melt1(services_file, 1992:2016, "employment_in_services_wb")
# gdp capita
gdp_file <- "data/rawdata/wb/gdp capita wb.csv"
gdp_dat <- read_and_melt1(gdp_file, 1992:2016, "gd_capita_wb")
# life expectancy
life_file <- "data/rawdata/wb/life expectancy birth.csv"
life_dat <- read_and_melt1(life_file, 1992:2016, "life_expect_wb")
# gross national income
gni_file <- "data/rawdata/wb/gni.csv"
gni_dat <- read_and_melt1(gni_file, 1992:2016, "gni_wb")
# ratio male-female workforce participation
ratio_work_file <- "data/rawdata/wb/ratio-male-female-labour-participation-WB.csv"
ratio_work_dat <- read_and_melt1(ratio_work_file, 1992:2016, "ratio_mfwork_wb")

## UN data ---------------------------------------------------------------------

# gender inequality index 
gii_file <- "data/rawdata/un/gender inequality index.csv"
gii_dat <- read_and_melt2(gii_file, "gii_un")
# secondary school females
secondary_file <- "data/rawdata/un/Population with at least some secondary education, female (% ages 25 and older).csv"
secondary_dat <- read_and_melt2(secondary_file, "highschl_perc_un")
# secondary school males
secondary2_file <- "data/rawdata/un/Population with at least some secondary education, male (% ages 25 and older).csv"
secondary2_dat <- read_and_melt2(secondary2_file, "highschl_perc_m_un")
# hdi
hdi_file <- "data/rawdata/un/Human development index (HDI).csv"
hdi_dat <- read_and_melt2(hdi_file, "hdi_un")
# years of school
school_file <- "data/rawdata/un/Mean years of schooling, female (years).csv"
school_dat <- read_and_melt2(school_file, "school_yrs_un")
# labour force - UN
labour_file <- "data/rawdata/un/Labour force participation rate, female (% ages 15 and older).csv"
labour_datun <- read_and_melt2(labour_file, "female_lfp_un")
# income - females
income_file <- "data/rawdata/un/Estimated gross national income per capita, female (2011 PPP$).csv"
income_dat_female <- read_and_melt2(income_file, "female_income_un")
# income - males
income_male_file <- "data/rawdata/un/Estimated gross national income per capita, male (2011 PPP$).csv"
income_dat_male <- read_and_melt2(income_male_file, "male_income_un")
# senior officials/management
senior_file <- "data/rawdata/un/Legislators, senior officials and managers, female (% of total).csv"
senior_dat <- read_and_melt2(senior_file, "female_senior_pos_un")
# adolescent birth rate
adol_birth_file <- "data/rawdata/un/Adolescent birth rate (births per 1,000 women ages 15-19).csv"
adol_birth_dat <- read_and_melt2(adol_birth_file, "adol_birth_rate_un")
# mean years schooling (all)
mean_sch_file <- "data/rawdata/un/Mean years of schooling (years).csv"
mean_sch_dat <- read_and_melt2(mean_sch_file, "mean_sch_un")
# expected years schooling (all)
exp_sch_file <- "data/rawdata/un/Expected years of schooling (years).csv"
exp_sch_dat <- read_and_melt2(exp_sch_file, "exp_sch_un")

## MEAN YEARS SCHOOLING
sch_file <- "data/rawdata/un/mean-years-schooling-uis.csv"
sch_dat <- fread(sch_file)
sch_dat$indicator <- NULL
sch_dat <- dcast(sch_dat,country + year ~ code)
# fix names
sch_dat[country == "Bolivia(PlurinationalStateof)",country :=  "Bolivia"] 
sch_dat[country == "Czechia",country := "CzechRepublic"] 
sch_dat[country == "RepublicofMoldova",country := "Moldova"]
sch_dat[country == "RepublicofKorea",country := "SouthKorea"] 
sch_dat[country == "TheformerYugoslavRepublicofMacedonia",country := "Macedonia"] 
sch_dat[country == "UnitedKingdomofGreatBritainandNorthernIreland",country := "UnitedKingdon"] 
sch_dat[country == "UnitedStatesofAmerica",country := "UnitedStates"] 
sch_dat[country == "Venezuela(BolivarianRepublicof)",country := "Venezuela"] 
sch_dat[country == "Palestine",country := "Palestine,Stateof"] 

## EDUCATIONAL ATTAINMENT
edu_file <- "data/rawdata/un/educational-attainment-uis.csv"
edu_dat <- fread(edu_file)
edu_dat$indicator <- NULL
edu_dat <- dcast(edu_dat,country + year ~ code)
# fix names
edu_dat[country == "Bolivia(PlurinationalStateof)",country :=  "Bolivia"] 
edu_dat[country == "Czechia",country := "CzechRepublic"] 
edu_dat[country == "RepublicofMoldova",country := "Moldova"]
edu_dat[country == "RepublicofKorea",country := "SouthKorea"] 
edu_dat[country == "TheformerYugoslavRepublicofMacedonia",country := "Macedonia"] 
edu_dat[country == "UnitedKingdomofGreatBritainandNorthernIreland",country := "UnitedKingdon"] 
edu_dat[country == "UnitedStatesofAmerica",country := "UnitedStates"] 
edu_dat[country == "Venezuela(BolivarianRepublicof)",country := "Venezuela"] 
edu_dat[country == "Palestine",country := "Palestine,Stateof"] 

## join datasets
mymerge <- function(x,y) merge.data.table(x,y,all.x=TRUE)
art_covar_dat <- Reduce(mymerge,list(art_dat, labour_datwb,labour_datwb_m, adol_dat, parl_dat, mort_dat,
                  fert_dat, services_dat, secondary_dat, secondary2_dat, ratio_work_dat,
                  hdi_dat, school_dat, labour_datun,
                  income_dat_female, income_dat_male,
                  senior_dat, adol_birth_dat, gdp_dat,
                  gii_dat, mean_sch_dat, exp_sch_dat, life_dat, gni_dat,
                  sch_dat,edu_dat))

## clean -----------------------------------------------------------------------

## load packages
source("src/tools/gii_comp.R")

## check country names
art_covar_dat[,.N,by=country]

### TRANSFORM, ADD

## use proportions over percentages
art_covar_dat[,highschl_perc_m_un := highschl_perc_m_un/100]
art_covar_dat[,highschl_perc_un := highschl_perc_un/100]
art_covar_dat[,male_lfp_wb := male_lfp_wb/100]
art_covar_dat[,female_lfp_wb := female_lfp_wb/100]
# art_dat[,lfp_male_ilo := lfp_male_ilo/100]
# art_dat[,lfp_female_ilo := lfp_female_ilo/100]
art_covar_dat[,female_senior_pos_un := female_senior_pos_un/100]
art_covar_dat[,employment_in_services_wb := employment_in_services_wb/100]
art_covar_dat[,women_parl_wb := women_parl_wb/100]

## transformations - log scaling
art_covar_dat[,ln_maternal_mortality_wb := log(maternal_mortality_wb)]
art_covar_dat[,ln_adol_birth_rate_wb := log(adol_birth_rate_wb)]
art_covar_dat[,ln_gni_wb := log(gni_wb)]
art_covar_dat[,ln_male_income_un := log(male_income_un)]
art_covar_dat[,ln_female_income_un := log(female_income_un)]
art_covar_dat[,ln_art_utilisation := log(art_utilisation)]

## variable creation
art_covar_dat[,school_gap := highschl_perc_m_un - highschl_perc_un]
art_covar_dat[,labour_gap_wb := male_lfp_wb - female_lfp_wb]
# art_covar_dat[,labour_gap_ilo := lfp_male_ilo - lfp_female_ilo]
## time
art_covar_dat[,time := year - min(year)]

## variable scaling - those variables likely in models (want similar numbers)
# life expectancy too large
covars <- c("female_lfp_wb", "school_gap", "labour_gap_wb",
            "male_lfp_wb", "ln_adol_birth_rate_wb", "women_parl_wb", "ln_maternal_mortality_wb", 
            "highschl_perc_un", "highschl_perc_m_un", "mean_sch_un", "life_expect_wb", 
            "exp_sch_un", "ln_gni_wb")
lapply(art_covar_dat[,..covars], var,na.rm=TRUE)

## create GII component variables
# gii
art_covar_dat[,gii_own :=
  calc_gii(maternal_mortality_wb, adol_birth_rate_wb,
           100*female_lfp_wb, 100*male_lfp_wb,
           100*highschl_perc_un, 100*highschl_perc_m_un, 
           100*women_parl_wb)]

# check
with(art_covar_dat, plot(gii_own, gii_un))
abline(0,1,col="red")
# health
art_covar_dat[,health := calc_health(maternal_mortality_wb,adol_birth_rate_wb)]
# empowerment
art_covar_dat[,empowerment:=calc_empowerment(100*highschl_perc_un, 
                                             100*highschl_perc_m_un,100*women_parl_wb)]
art_covar_dat[,empowerment := empowerment/100]
# labour
art_covar_dat[,labour := calc_labour(100*female_lfp_wb, 100*male_lfp_wb)]
art_covar_dat[,labour :=  labour/100]

## ---------------------------------------------------------------------------
### SAVE
saveRDS(art_covar_dat, file = "data/art_covar_2002_2014.rds")
fwrite(art_covar_dat, file = "data/art_covar_2002_2014.csv")

