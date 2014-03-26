# 	Gero Dolfus
# 	University of Helsinki, HECER
# 	Start: February 21, 2014.
#
#	Displaying data by municipalities.
#	
#
#	Geographical data from Statistics Finland.
#
# 	http://geo.stat.fi/geoserver/web/?wicket:bookmarkablePage=:org.geoserver.web.demo.MapPreviewPage .
#	(kunta4500, Shapefiles)
#	Note: 	to get all municipalities, 
#			edit the request in the wfsrequest.txt 
#			that comes along with the Shapefiles
#			(remove the maxFeatures parameter).
# http://geoserv.stat.fi:8080/geoserver/tilastointialueet/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=tilastointialueet:kunta4500&outputFormat=SHAPE-ZIP
#
#
#
#	Code based Oskar Perpinan's work.
#
# 	See https://github.com/oscarperpinan/spacetime-vis/blob/master/choropleth.R .
# 	See also http://oscarperpinan.github.io/spacetime-vis/ .

#	
#
#




# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Setup.
#
# - - - - - - - - - - - - - - - - - - - - - - 

# Clear workspace.
rm(list = ls())

# Load the packages for creating the maps.
library(lattice)
library(ggplot2)
library(latticeExtra)
library(sp)
library(maptools)
library(colorspace)

# Attempts to fix stuff...
library(rgeos)
library(gpclib)
gpclibPermit()



# Set the name of the directories where the data is.
dirname.data <- "~/RRR_finn/data/statfin/geo/kunta4500"

# Set the name of the directory for saving pictures.
dirname.pics <- "~/RRR_finn/pics/"

# Set the name of the directory where other scripts are.
dirname.scripts <- "~/RRR_finn/r/maps/"

# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Choose the input directory and the name of the pictures.
#
# - - - - - - - - - - - - - - - - - - - - - - 


dirname.data.mun <- "~/RRR_finn/data/statfin/municipal/"


sav.pic.name <- "map-fin-unempl-2013"
sav.plot.title <- "Unemployment"

sav.palette<-brewer.pal(9,"YlOrRd")



mun.min = 0
mun.max = 25
mun.at.num = 5


# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Read the data into memory.
#
# - - - - - - - - - - - - - - - - - - - - - - 

# ---------------------------------
# Geographical data.
# ---------------------------------

setwd(dirname.data)
geo.dat <- readShapePoly(fn = "kunta4500")
Encoding(levels(geo.dat$nimi)) <- "latin1"

geo.dat.dataslot <- slot(geo.dat, "data")



# ---------------------------------
# Unemployment data.
# ---------------------------------

mun.dat <- read.table(paste(dirname.data.mun, "municipal-unempl-rate-2013.csv", 
	sep = ""), sep = ",", stringsAsFactors = F, header = T)



# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Manipulate the data.
#
# - - - - - - - - - - - - - - - - - - - - - - 



# Prepare the data for the conventions below:
# The municipality code needs to be mun.code
# The municipality name needs to be mun.name
# The municipality 
names(mun.dat)[(names(mun.dat) == "Kunta.code")] <- "mun.code"
names(mun.dat)[(names(mun.dat) == "Kunta")] <- "mun.name"
names(mun.dat)[(names(mun.dat) == "Kuukausi")] <- "month"
#names(mun.dat)[(names(mun.dat)=="ACTUAL_DATA")]<-"dat"




# ***************** IMPORTANT ********************
# Sort the data according to the municipality numbers.
# The geo data is sorted according to the municipality numbers.
# See the TEST RUN below on that.

mun.dat <- mun.dat[order(mun.dat$mun.code, decreasing = F), ]


# Group the data.
mun.dat$mydat <- mun.dat$dat
mun.dat$mydat[which(mun.dat$dat < 5)] = "1:  <5"
mun.dat$mydat[which(mun.dat$dat >= 5 & mun.dat$dat < 10)] = "2:  5-10"
mun.dat$mydat[which(mun.dat$dat >= 10 & mun.dat$dat < 15)] = "3:  10-15"
mun.dat$mydat[which(mun.dat$dat >= 15 & mun.dat$dat < 20)] = "4: 15-20"
mun.dat$mydat[which(mun.dat$dat >= 20)] = "5: >20"
mun.dat$mydat <- factor(mun.dat$mydat)


# TEST RUN
# Test run to see whether the geographical data is indeed sorted by municipality codes.
# tmp.select=c(49,91,92,235)
# geo.dat.dataslot$nimi[geo.kunta.num%in%tmp.select]
# mtch.ids <- match(tmp.select,geo.kunta.num)
# mun.dat$mun.code[mtch.ids]



# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Match geo data and data to be displayed.
#
# - - - - - - - - - - - - - - - - - - - - - - 

# Fetch the ids from the polygon object (from zero to max number of entries).
geo.ids <- sapply(geo.dat@polygons, function(x) x@ID)

# Match the municipality codes in the data and the polygon object.
geo.kunta.num <- as.numeric(as.character((geo.dat.dataslot$kunta)))

mtch.ids <- match(as.numeric(geo.kunta.num), mun.dat$mun.code)


# Data to be added to the SpatialPolygons object.
dat2add <- mun.dat[mtch.ids, ]

# ****************
# Missing information.
#
# 1) 	Store the ids of the municipality codes for which
#		there is no data. 
missing.data.ids <- which(is.na(dat2add$dat) == T & is.na(dat2add$mun.code) == 
	F)
# 2) 	Store the ids of the missing municipality codes,
# 		i.e. of the missing matches.
missing.mun.ids <- which(is.na(mtch.ids))


# SpatialPolygonsDataFrame uses row names to match polygons with data.
row.names(dat2add) <- geo.ids
map.data <- SpatialPolygonsDataFrame(geo.dat, dat2add)



# **************************************************************************************
#
# EVERYTHING BELOW THIS LINE SHOULDN'T NEED CUSTOMIZING.
#
# **************************************************************************************



source(paste(dirname.scripts,"plot_map.R",sep=''))







