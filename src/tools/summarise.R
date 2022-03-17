#=====================================================================================
# Name		    : Summarising functions	
# Author		  : Oisin	
# Version		  : 0.1
# Description	: Functions for creating numeric summaries of data
#=====================================================================================

#' Ratio of between to within cluster variance
#'
#' @param x character indicating variable from art_dat
wb_var <- function(x, ...) {
  
  ## filter to no NAs
  art_dat1 <- art_dat[!is.na(art_dat[[x]]),]
  
  ## between group variance
  bb <- tapply(art_dat1[[x]], art_dat1$year, var)
  bb <- mean(bb, na.rm = TRUE)
  ## within group variance
  ww <- tapply(art_dat1[[x]], art_dat1$country, var)
  ww <- mean(ww, na.rm = TRUE)
  
  ## ratio of within to between variance
  cat(x, "\n",
      "between: ", round(bb,4), "\n", 
      "within: ", round(ww,4), "\n",
      "w/b: ", round(ww/bb,3), "\n",
      "b/w: ", round(bb/ww,2), "\n\n")
}
