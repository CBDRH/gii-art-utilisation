###############################################################################
#
#
#
#
#
###############################################################################

# libraries
library(ggplot2)
library(data.table)
library(lme4)
library(mitml)
library(MuMIn)

## index models
index_decomposition_complete <- readRDS("models/index_decomposition_complete.rds")
index_decomposition_full <- readRDS("models/index_decomposition_full.rds")         
index_decomposition_partial <- readRDS("models/index_decomposition_partial.rds")     
index_standard_complete <- readRDS("models/index_standard_complete.rds")          
index_standard_full <- readRDS("models/index_standard_full.rds")       
index_standard_partial <- readRDS("models/index_standard_partial.rds")       
index_hdi <- readRDS("models/index_hdi_complete.rds")       
index_gii <- readRDS("models/index_gii_complete.rds")       

# standard
isc_coef <- cbind(fixef(index_standard_complete)[2:4],confint(index_standard_complete)[4:6,])
isf_coef <- cbind(index_standard_full$estimates[2:4,1],confint(index_standard_full)[2:4,])
isp_coef <- cbind(index_standard_partial$estimates[2:4,1],confint(index_standard_partial)[2:4,])
standard <- data.frame(rbind(isc_coef,isf_coef,isp_coef))
names(standard) <- c("beta","lower","upper")
standard$model <- c(rep("Complete cases\n(adjusted)",nrow(isp_coef)),
                    rep("Full imputation",nrow(isp_coef)),
                    rep("Partial imputation\n(main model)",nrow(isp_coef)))
standard$term <- rep(c("Time\n(~per quarter)","Gender inequality\nindex","Human development\nindex"),3)

# individual
gii_coef <- c(fixef(index_gii)[3],confint(index_gii)[5,])
hdi_coef <- c(fixef(index_hdi)[3],confint(index_hdi)[5,])
individual <- data.frame(rbind(gii_coef,
                               hdi_coef))
names(individual) <- c("beta","lower","upper")
individual$model <- c("Complete cases\n(unadjusted)")
individual$term <- c("Gender inequality\nindex","Human development\nindex")

# join
standard <- rbind(standard,individual)
standard$term <- factor(standard$term,levels=c("Time\n(~per quarter)",
                                               "Gender inequality\nindex",
                                               "Human development\nindex"),
                        ordered=TRUE)
standard$model <- factor(standard$model,levels=c("Complete cases\n(unadjusted)",
                                                 "Complete cases\n(adjusted)",
                                                 "Full imputation",
                                                 "Partial imputation\n(main model)"),ordered=TRUE)

ggplot(standard) +
  geom_pointrange(aes(x=term,y=beta,ymin=lower,ymax=upper,col=model),
                  position = position_dodge(width=0.2)) +
  labs(y = "Model coefficient\n(standardised)",x="") +
  geom_hline(yintercept = 0, linetype=2) +
  scale_color_discrete(name="Model") +
  theme_bw(base_size=16) 
ggsave("results/graphs/index_standard_results.png")

## component models
rm(standard)
component_standard_complete <- readRDS("models/component_standard_l275_complete.rds")          
component_standard_full <- readRDS("models/components_standard_l275_full.rds")       
component_standard_partial <- readRDS("models/components_standard_l275_partial.rds")   


csc_coef <- cbind(component_standard_complete$beta,
                  component_standard_complete$beta-1.96*component_standard_complete$beta_se,
                  component_standard_complete$beta+1.96*component_standard_complete$beta_se)
csf_coef <- cbind(component_standard_full$estimates[2:10,1],confint(component_standard_full)[2:10,])
csp_coef <- cbind(component_standard_partial$estimates[2:10,1],confint(component_standard_partial)[2:10,])
standard <- data.frame(rbind(csc_coef,csf_coef,csp_coef))
names(standard) <- c("beta","lower","upper")
standard$model <- c(rep("Complete cases\n(adjusted)",nrow(csc_coef)),
                    rep("Full imputation",nrow(csc_coef)),
                    rep("Partial imputation\n(main model)",nrow(csc_coef)))

component_ln_adol_birth_rate_wb1 <- readRDS("models/component_ln_adol_birth_rate_wb1_complete.rds")   
component_ln_maternal_mortality_wb1 <- readRDS("models/component_ln_maternal_mortality_wb1_complete.rds")          
component_women_parl_wb1 <- readRDS("models/component_women_parl_wb1_complete.rds")          
component_mean_sch_un1 <- readRDS("models/component_mean_sch_un1_complete.rds")    
component_school_gap1 <- readRDS("models/component_school_gap1_complete.rds")       
component_labour_gap_wb1 <- readRDS("models/component_labour_gap_wb1_complete.rds")       
component_life_expect_wb1 <- readRDS("models/component_life_expect_wb1_complete.rds")          
component_ln_gni_wb1 <- readRDS("models/component_ln_gni_wb1_complete.rds")          
ln_adol_birth_rate_wb1_coef <- c(fixef(component_ln_adol_birth_rate_wb1)[3],confint(component_ln_adol_birth_rate_wb1)[5,])
ln_maternal_mortality_wb1_coef <- c(fixef(component_ln_maternal_mortality_wb1)[3],confint(component_ln_maternal_mortality_wb1)[5,])
women_parl_wb1_coef <- c(fixef(component_women_parl_wb1)[3],confint(component_women_parl_wb1)[5,])
mean_sch_un1_coef <- c(fixef(component_mean_sch_un1)[3],confint(component_mean_sch_un1)[5,])
school_gap1_coef <- c(fixef(component_school_gap1)[3],confint(component_school_gap1)[5,])
labour_gap_wb1_coef <- c(fixef(component_labour_gap_wb1)[3],confint(component_labour_gap_wb1)[5,])
life_expect_wb1_coef <- c(fixef(component_life_expect_wb1)[3],confint(component_life_expect_wb1)[5,])
ln_gni_wb1_coef <- c(fixef(component_ln_gni_wb1)[3],confint(component_ln_gni_wb1)[5,])
individual <- data.frame(rbind(ln_adol_birth_rate_wb1_coef,
                               women_parl_wb1_coef,
                               ln_maternal_mortality_wb1_coef,
                               school_gap1_coef,
                               labour_gap_wb1_coef,
                               life_expect_wb1_coef,
                               ln_gni_wb1_coef,
                               mean_sch_un1_coef))
names(individual) <- c("beta","lower","upper")
individual$model <- c("Complete cases\n(unadjusted)")

# join
standard <- rbind(standard,individual)
standard$term <- c(rep(c("Time",
                       "ABR",
                       "Female\nparliament",
                       "MMR",
                       "School\ngap",
                       "Labour\ngap",
                       "Life\nexpect",
                       "GNI",
                       "Education\nlevel"),3),
                       c("ABR",
                       "Female\nparliament",
                       "MMR",
                       "School\ngap",
                       "Labour\ngap",
                       "Life\nexpect",
                       "GNI",
                       "Education\nlevel"))
standard$term <- factor(standard$term,levels=c("Time",
                                               "ABR",
                                               "Female\nparliament",
                                               "MMR",
                                               "School\ngap",
                                               "Labour\ngap",
                                               "Life\nexpect",
                                               "GNI",
                                               "Education\nlevel"),
                        ordered=TRUE)
standard$model <- factor(standard$model,levels=c("Complete cases\n(unadjusted)",
                                                 "Complete cases\n(adjusted)",
                                                 "Full imputation",
                                                 "Partial imputation\n(main model)"),ordered=TRUE)

ggplot(standard) +
  geom_pointrange(aes(x=term,y=beta,ymin=lower,ymax=upper,col=model),
                  position = position_dodge(width=0.2)) +
  labs(y = "Model coefficient\n(standardised)",x="") +
  geom_hline(yintercept = 0, linetype=2) +
  scale_color_discrete(name="Model") +
  theme_bw(base_size=16) +
  theme(axis.text.x = element_text(angle = -45, vjust = 0.6, hjust=0.0))
ggsave("results/graphs/component_standard_results.png")

# plot on exp scale
tmp <- component_standard_partial$estimates[2:10,1:2]
tmp <- data.frame(tmp)
setDT(tmp,keep.rownames = "id")
compute_exp <- function(beta,se,increase=TRUE) {
  if (increase == TRUE) {
    change <- seq(0,1,0.1)
  } else {
    change <- seq(0,-1,-0.1)
  }
  df <- data.frame(change_var=change,
                   change_art=exp(change*beta),
                   change_art_lower=exp(change*(beta-2*se)),
                   change_art_upper=exp(change*(beta+2*se)))
  df
}

tmp1 <- rbind(
  compute_exp(tmp[id=="ln_abr1",Estimate],tmp[id=="ln_abr1",Std.Error],FALSE),
  compute_exp(tmp[id=="women_parl_wb1",Estimate],tmp[id=="women_parl_wb1",Std.Error],TRUE),
  compute_exp(tmp[id=="ln_mmr1",Estimate],tmp[id=="ln_mmr1",Std.Error],FALSE),
  compute_exp(tmp[id=="sch_gap1",Estimate],tmp[id=="sch_gap1",Std.Error],FALSE),
  compute_exp(tmp[id=="lab_gap_wb1",Estimate],tmp[id=="lab_gap_wb1",Std.Error],FALSE),
  compute_exp(tmp[id=="life_expect_wb1",Estimate],tmp[id=="life_expect_wb1",Std.Error],TRUE),
  compute_exp(tmp[id=="ln_gni_wb1",Estimate],tmp[id=="ln_gni_wb1",Std.Error],TRUE),
  compute_exp(tmp[id=="mean_sch_un1",Estimate],tmp[id=="mean_sch_un1",Std.Error],TRUE))
tmp1$var <- rep(c("ABR","Female\nparliament","MMR","School\ngap",
                  "Labour\ngap","Life\nexpect","GNI","Education\nlevel"),each=11)

ggplot(tmp1,aes(x=change_var,y=100*(change_art-1)))+
  geom_line() +
  #scale_y_continuous(breaks=seq(1,2,by=0.25),labels=seq(0,100,by=25)) +
  geom_ribbon(aes(ymin=100*(change_art_lower-1),ymax=100*(change_art_upper-1)),alpha=0.1)+
  labs(x = "Change in variable\n(SD units)",y="Change in ART utilisation (%)") +
  theme_bw(base_size=16) +
  facet_wrap(~var,scales="free")
ggsave("results/graphs/component_one_unit.png",height=8,width=9)
