#=====================================================================================
# Name		    : Plotting functions	
# Author		  : Oisin	
# Version		  : 0.1
# Description	: Data visualisation functions for use with ART utilisation and gender 
#               equity data
#=====================================================================================


#' Plot the cross-sectional and longitudinal relationship between two variables
#'
#' @param x character indicating x variable from art_dat
#' @param y character indicating y variable from art_dat
#' @param xlab 
#' @param ylab
plot_cross_long <- function(x, y, xlab=NULL, ylab=NULL, ...) {
  
  ## output plots as a 1x2 grid
  par(mfrow = c(1,2))
  
  ## if no x or y lab then use var names
  if(is.null(xlab)) xlab <- x
  if(is.null(ylab)) ylab <- y
  
  ## cross-sectional plot (average vs. average)
  art_dat1 <- art_dat[!is.na(art_dat[[x]]) & !is.na(art_dat[[y]]),]
  xvar1 <- tapply(art_dat1[[x]], art_dat1$country, mean, na.rm = TRUE)
  xvar2 <- tapply(art_dat1[[y]], art_dat1$country, mean, na.rm = TRUE)
  scatter.smooth(xvar1, xvar2, xlab=xlab,ylab=ylab,pch=20,lpars=list(col="blue"),
                 main="Cross-sectional \n (country averages)")
  
  ## longitudinal plot (change in vs. change in)
  lvar1 <- art_dat1[[x]] - xvar1[art_dat1$country]
  lvar2 <- art_dat1[[y]] - xvar2[art_dat1$country]
  scatter.smooth(lvar1, lvar2, xlab=xlab,ylab=ylab,pch=20,lpars=list(col="blue"),
                 main="Longitudinal \n (within country changes)")
  abline(h = 0, lty = 3)
  abline(v = 0, lty = 3)
}


#' Plot the time trend (spaggetti plot) of a covariate
#' 
#' @param x character indicating variable from art_dat
plot_trend <- function(x, ...) {
  
  ## filter to no NAs
  art_dat1 <- art_dat[!is.na(art_dat[[x]]),]
  art_dat1 <- art_dat1[order(art_dat1$country, art_dat1$year),]
  
  ## mean of variable by year
  year_mu <- tapply(art_dat1[[x]], art_dat1$year, mean)
  years <- as.numeric(names(year_mu))
  
  ## plot mean then loop across countries in the dataset
  countries <- unique(art_dat1$country)
  ylim <- c(min(art_dat1[[x]]), max(art_dat1[[x]]))
  plot(years, year_mu, xlab = "year", ylab = x, ylim = ylim, type = "b", lwd = 3)
  for (i in seq_along(countries)) {
    index <- art_dat1$country == countries[i]
    lines(art_dat1$year[index], art_dat1[[x]][index], col = "skyblue", type = "b")
  }
  lines(years, year_mu, lwd = 3)
}

