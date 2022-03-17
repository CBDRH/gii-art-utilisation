#=====================================================================================
# Name		    : Modelling functions	
# Author		  : Oisin	
# Version		  : 0.1
# Description	: Data modelling functions (e.g. for CRE) 
#=====================================================================================

#' Calculate cluster mean
#'
#' @param x numeric vector to be cluster centered
#' @param id character indicating cluster variable
#' @param incl logical vector indicating if all entries of x should be considered in
#' calculating the cluster means
#' @return a numeric vector of length x populated with the cluster means
.m <- function(x, id = "country", incl = NULL) {
  if (!is.null(incl)) {
    ## use subset of entries of x
    out <- tapply(x[incl], id[incl], mean, na.rm=TRUE)[id]
  } else {
    ## use all entries of x
    out <- tapply(x, id, mean, na.rm=TRUE)[id]
  }
  ## return value
  out
}


#' Cluster center a variable
#'
#' @param x numeric vector to be cluster centered
#' @param id character indicating cluster variable
#' @param incl logical vector indicating if all entries of x should be considered in
#' calculating the cluster mean
#' @return a numeric vector of length x populated with the cluster centered variable
.c <- function(x, id = "country", incl = NULL) {
  ## calculate cluster means
  m <- .m(x, id, incl)
  ## calculate mean centered variables
  out <- x - m
  ## return value
  out
}


#' Calculate sums of squares
#'
#' @param x numeric vector
#' @return sums of squares
ss <- function(x) {
  sum((x - mean(x,na.rm=TRUE))^2,na.rm=TRUE)
}


#' Calculate r-squared per cluster
#' 
#' @param  
#' 
#' 
r2_cluster <- function(formula, dat, id, incl) {
  
  ## checks
  stopifnot(length(incl) == nrow(dat))
  
  ## only use complete data (incl)
  dat <- dat[incl,]
  id <- id[incl]
  
  ## linear models
  lms <- lmList(formula, data = dat)
  
  ## sums of squares
  resp_name <- as.character(formula[[2]])
  resp <- dat[[resp_name]]
  resid <- resp - fitted(lms)
  sse <- tapply(resid, id, function(x) sum(x^2, na.rm=TRUE))
  sst <- tapply(resp, id, ss)
  
  ## combine to dataset
  n <- tapply(resp, id, length)
  dat1 <- data.frame(country = names(n), n)
  r2 <- (sst - sse)/sst
  dat2 <- data.frame(country = names(r2), r2)
  out <- merge(dat1, dat2)
  
  ## output
  out
}


#' Fit a CRE model to unbalanced data
#' 
#' 
cre <- function(f, data, id, REML = TRUE, no_mc = NULL) {
  ## extract variable of interest
  vars <- as.character(f)
  vars <- unlist(strsplit(vars, " "))
  vars <- unlist(strsplit(vars, "[()]"))
  vars <- vars[!vars %in% c("1", "~", "+", "|", "")]
  ## rows to use in centering and mean calculations
  incl <- apply(is.na(data[,vars]), MARGIN = 1, function(x) sum(x) == 0)
  id_var <- data[[id]]
  ## formula for model
  response <- vars[1]
  covars <- vars[2:(length(vars)-1)]
  if(!is.null(no_mc)){
    covars_no_mc <- covars[covars %in% no_mc]
    covars_mc <- covars[!covars %in% no_mc]
    covars_cent <- paste(".c(", covars_mc, ",  id_var, ", "incl", ")", sep = "")
    covars_mean <- paste(".m(", covars_mc, ",  id_var, ", "incl", ")", sep = "")
    f0 <- paste(covars_no_mc, collapse = " + ")
    f1 <- paste(covars_cent, collapse = " + ")
    f2 <- paste(covars_mean, collapse = " + ")
    f3 <- paste(response, " ~ ", f0, " + ", f1, " + ", f2, sep = "")
  } else {
    covars_cent <- paste(".c(", covars, ",  id_var, ", "incl", ")", sep = "")
    covars_mean <- paste(".m(", covars, ",  id_var, ", "incl", ")", sep = "")
    f1 <- paste(covars_cent, collapse = " + ")
    f2 <- paste(covars_mean, collapse = " + ")
    f3 <- paste(response, " ~ ", f1, " + ", f2, sep = "")
  }
  f4 <- paste(f3, "+ (1|",id,")",sep="")
  fcre <- as.formula(f4)
  ## fit model
  lme4::lmer(fcre, data, REML = REML)
}
 
#' Index of no NAs
#'
#'
no_na_index <- function(data, vars) {
  nas <- is.na(data[,vars])
  apply(nas, 1 ,function(x) sum(x) > 1)
}
