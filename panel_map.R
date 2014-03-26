# 	Gero Dolfus
# 	University of Helsinki, HECER
# 	Start: March 11, 2014.
#
#	Displaying data by municipalities and time.
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
#	Code based Perpinan and Rowlingson's work.
#
# 	See https://github.com/oscarperpinan/spacetime-vis/blob/master/choropleth.R .
# 	See also http://oscarperpinan.github.io/spacetime-vis/ .
#
#	See http://www.maths.lancs.ac.uk/~rowlings/Teaching/UseR2012/crime.html
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
library("reshape2")


# Attempts to fix stuff...
library(rgeos)
library(gpclib)
gpclibPermit()


# Functions.
# Returns string w/o leading or trailing whitespace.
fun.trim <- function(x) gsub("^\\s+|\\s+$", "", x)


# Set the name of the directories where the data is.
dirname.data <- "~/RRR_finn/data/statfin/geo/kunta4500"

# Set the name of the directory for saving pictures.
dirname.pics <- "~/RRR_finn/pics/"


# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Choose the input directory and the name of the pictures.
#
# - - - - - - - - - - - - - - - - - - - - - - 


dirname.data.mun <- "~/RRR_finn/data/statfin/municipal/"


sav.pic.name <- "map-fin-unempl-panel"
sav.plot.title <- "Unemployment"

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

mun.dat <- read.table(paste(dirname.data.mun, "municipal-unempl-rate-march-2006-2013.csv", 
	sep = ""), sep = ",", stringsAsFactors = F, header = T)


years = 2006:2013

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

mun.min = 0
mun.max = 25
mun.step = 25


# Group the data.
mun.dat$mydat <- mun.dat$dat
mun.dat$mydat[which(mun.dat$dat < 5)] = "1:  <5"
mun.dat$mydat[which(mun.dat$dat >= 5 & mun.dat$dat < 10)] = "2:  5-10"
mun.dat$mydat[which(mun.dat$dat >= 10 & mun.dat$dat < 15)] = "3:  10-15"
mun.dat$mydat[which(mun.dat$dat >= 15 & mun.dat$dat < 20)] = "4: 15-20"
mun.dat$mydat[which(mun.dat$dat >= 20)] = "5: >20"
mun.dat$mydat <- factor(mun.dat$mydat)






# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Match geo data and data to be displayed.
#
# - - - - - - - - - - - - - - - - - - - - - - 



# Extract the data in the panel format.
# tmp.df = dcast(mun.dat, mun.code ~ month, value.var = "dat")
tmp.df = dcast(mun.dat, mun.code ~ month, value.var = "mydat")

# Match the municipality codes in the data and the polygon object.
geo.kunta.num <- as.numeric(as.character((geo.dat.dataslot$kunta)))
mtch.ids <- match(as.numeric(geo.kunta.num), tmp.df$mun.code)


# Make it pretty.
tmp.months = names(tmp.df)[grep("kuu", names(tmp.df))]
tmp.months = gsub("Maaliskuu", " ", tmp.months)
tmp.months = gsub("^", "March ", tmp.months)
tmp.months = fun.trim(tmp.months)
names(tmp.df)[grep("kuu", names(tmp.df))] = tmp.months

# # For the color range.
# mun.min = min(mun.dat$dat, na.rm = T)
# mun.max = max(mun.dat$dat, na.rm = T)
# mun.step = 1


# **************************************************************************************
#
# EVERYTHING BELOW THIS LINE SHOULDN'T NEED CUSTOMIZING.
#
# **************************************************************************************




# Data to be added to the SpatialPolygons object.
tmp.df <- tmp.df[mtch.ids, ]

# Combine geo data and data to be displayed.
tmp.geo.dat <- geo.dat
tmp.geo.dat@data = cbind(tmp.geo.dat@data, tmp.df)
names(tmp.geo.dat) <- make.names(names(tmp.geo.dat))



# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Create maps.
#
# - - - - - - - - - - - - - - - - - - - - - - 




# ---------------------------------
# Plot the map with the data.


# tmp.at = seq(mun.min, 1.01*mun.max, mun.step)




# Color setings.
tmp.colors = colorRampPalette(c("yellow", "red"))(5)
# sp.theme(set = TRUE, regions = list(col = tmp.colors, lwd = 2))

# Plot.
tmp.plot <- spplot(tmp.geo.dat, paste("March.",(years), sep = ""), main = sav.plot.title, 
	col.regions = tmp.colors)

pdf(file = paste(dirname.pics, sav.pic.name, ".pdf", sep = ""))
print(tmp.plot)
dev.off()


tmp.p1 <- spplot(tmp.geo.dat, paste("March.", as.character(2006:2007), sep = ""), 
	col.regions = tmp.colors)
tmp.p2 <- spplot(tmp.geo.dat, paste("March.", as.character(2008:2009), sep = ""), 
	col.regions = tmp.colors)
tmp.p3 <- spplot(tmp.geo.dat, paste("March.", as.character(2010:2011), sep = ""), 
	col.regions = tmp.colors)
tmp.p4 <- spplot(tmp.geo.dat, paste("March.", as.character(2012:2013), sep = ""), 
	col.regions = tmp.colors)

pdf(file = paste(dirname.pics, sav.pic.name, "-4-by-4.pdf", sep = ""))
print(tmp.p1, position = c(0, 0.5, 0.5, 1), more = T)
print(tmp.p2, position = c(0.5, 0.5, 1, 1), more = T)
print(tmp.p3, position = c(0, 0, 0.5, 0.5), more = T)
print(tmp.p4, position = c(0.5, 0, 1, 0.5))
dev.off()



# Plot grouped data.

tmp.plot <- spplot(tmp.geo.dat, paste("March.",(years), sep = ""), main = sav.plot.title)


tmp.plot <- spplot(tmp.geo.dat, paste("March.",2006, sep = ""), main = sav.plot.title)



pdf(file = paste(dirname.pics, sav.pic.name, ".pdf", sep = ""))
print(tmp.plot)
dev.off()









