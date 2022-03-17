######################################################################
# Functions for recreating the indices of the GII
#
# Contents:
# 
######################################################################


## ----------------------------------------------------------------------------
### FUNCTIONS
# calc_health
calc_health <- function(mmr, abr) {
  # Step 1. Treating zeros and extreme values
  mmr[mmr > 1000] <- 1000
  mmr[mmr < 10] <- 10
  abr[abr == 0] <- 0.1
  
  # Step 2. Calculating the geometric mean of the arithmetic
  # means for each indicator
  health <- (sqrt((10/mmr) * (1/abr)) + 1)/2
  
  # return health index
  health
}

# calc_empowerment
calc_empowerment <- function(se_f, se_m, f_parl) {
  # Step 1. Treating zeros and extreme values
  se_f[se_f == 0] <- 0.1
  se_m[se_m == 0] <- 0.1
  f_parl[f_parl == 0] <- 0.1
  
  # Step 2. Calculating the geometric mean of the arithmetic
  # means for each indicator
  t2f <- sqrt(f_parl * se_f)
  t2m <- sqrt((100-f_parl) * se_m)
  empowerment <- (t2f + t2m)/2
  
  # return empowerment index
  empowerment
}

# calc_labour
calc_labour <- function(lfp_f, lfp_m) {
  # Step 1. Treating zeros and extreme values
  lfp_f[lfp_f == 0] <- 0.1
  lfp_m[lfp_m == 0] <- 0.1
  
  # Step 2. Calculating the geometric mean of the arithmetic
  # means for each indicator
  lfpr <- (lfp_m + lfp_f)/2
  
  # return lfpr index
  lfpr
}

# calc_gii:
calc_gii <- function(mmr, abr, lfp_f, lfp_m, se_f, se_m, f_parl) {
  # Step 1. Treating zeros and extreme values
  mmr[mmr > 1000] <- 1000
  mmr[mmr < 10] <- 10
  abr[abr == 0] <- 0.1
  lfp_f[lfp_f == 0] <- 0.1
  lfp_m[lfp_m == 0] <- 0.1
  se_f[se_f == 0] <- 0.1
  se_m[se_m == 0] <- 0.1
  f_parl[f_parl == 0] <- 0.1
  
  # Step 2. Aggregating across dimensions within each gender
  # group, using geometric means
  # females
  t1 <- sqrt((10/mmr) * (1/abr))
  t2f <- sqrt(f_parl * se_f)
  gf <- (t1 * t2f * lfp_f)^(1/3)
  # males
  t2m <- sqrt((100-f_parl) * se_m)
  gm <- (1 * t2m * lfp_m)^(1/3)
  
  # Step 3. Aggregating across gender groups, using a harmonic mean
  harm <- (((1/gf) + (1/gm))/2)^(-1)
  
  # Step 4. Calculating the geometric mean of the arithmetic
  # means for each indicator
  health <- calc_health(mmr, abr)
  empowerment <- calc_empowerment(se_f, se_m, f_parl)
  lfpr <- calc_labour(lfp_f, lfp_m)
  gfm <- (health*empowerment*lfpr)^(1/3)
  
  # Step 5. Calculating the Gender Inequality Index
  gii <- 1 - (harm/gfm)
  
  # return gii
  gii
}

# logit
logit <- function(x) {
  x[x == 0] <- 0.01
  x[x == 1] <- 0.99
  log(x/(1-x))
}

# logistic
logistic <- function(x) {
  exp(x)/(1+exp(x))
}

# arcsin sqrt root
asinsq <- function(x) { 
  base::asin(sqrt(x)) 
}

# inverse of arcsin sqrt root
sin2 <- function(x) { 
  sin(x)^2
}

## end-functions
