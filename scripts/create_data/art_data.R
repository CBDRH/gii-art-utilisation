###################################################################################################
# make a ART utilisation between 2002 and 2010 dataset 
# data source: ICMART world reports on ART (see data/rawdata folder)
# 
# 26/7/17
###################################################################################################

# load packages
library(data.table)

read_csv <- function(path) {
  df <- fread(path,header=TRUE)
  df[country == "TheformerYugoslavRepublicofMacedonia",country:="Macedonia"] 
  df <- df[!country == "TheformerYugoslavRepublicofMacedonia"]
  df[country == "RussianFederation",country:="Russian"]
  df <- df[!country == "RussianFederation",]
  df[country == "Bolivia(PlurinationalStateof)",country:="Bolivia"]
  df <- df[!country == "Bolivia(PlurinationalStateof)",]
  df[country == "Korea(Republicof)",country:="SouthKorea"]
  df <- df[!country == "Korea(Republicof)",]
  setkeyv(df,c("country"))
  df
}

## read in ART dataset
art_dat_2002 <- read_csv("data/rawdata/art/art_2002.csv")
art_dat_2003 <- read_csv("data/rawdata/art/art_2003.csv")
art_dat_2004 <- read_csv("data/rawdata/art/art_2004.csv")
art_dat_2005 <- read_csv("data/rawdata/art/art_2005.csv")
art_dat_2006 <- read_csv("data/rawdata/art/art_2006.csv")
art_dat_2007 <- read_csv("data/rawdata/art/art_2007.csv")
art_dat_2008 <- read_csv("data/rawdata/art/art_2008.csv")
art_dat_2009 <- read_csv("data/rawdata/art/art_2009.csv")
art_dat_2010 <- read_csv("data/rawdata/art/art_2010.csv")
art_dat_2011 <- read_csv("data/rawdata/art/art_2011.csv")
art_dat_2012 <- read_csv("data/rawdata/art/art_2012.csv")
art_dat_2013 <- read_csv("data/rawdata/art/art_2013.csv")
art_dat_2014 <- read_csv("data/rawdata/art/art_2014.csv")

country_region <- fread("data/rawdata/country_region.csv")

## join
## join datasets
mymerge <- function(x,y) merge.data.table(x,y,all=TRUE)
art_dat <- Reduce(mymerge,list(art_dat_2002, art_dat_2003, art_dat_2004, art_dat_2005, 
                    art_dat_2006, art_dat_2007, art_dat_2008, art_dat_2009, art_dat_2010, art_dat_2011,
                    art_dat_2012,art_dat_2013,art_dat_2014))
art_dat <- merge(art_dat,country_region,by="country",all.x=TRUE)
names(art_dat)[-1] <- c(2002:2014, "region")

art_dat

## make long
art_dat_long <- melt(art_dat,id.vars = c(1,ncol(art_dat)),value.name = "art_utilisation",
                     variable.name="year")

## save as a CSV
write.csv(art_dat_long, "data/art_2002_2014.csv", row.names = FALSE)
