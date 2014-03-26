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


# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Choose the input directory and the name of the pictures.
#
# - - - - - - - - - - - - - - - - - - - - - - 


sav.pic.name <- "map-fin-random-data"
sav.plot.title <- "Random Data"


# dirname.data.mun<-"~/RRR_finn/data/statfin/municipal/"


# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Read the data into memory.
#
# - - - - - - - - - - - - - - - - - - - - - - 



# ---------------------------------
# Geographical data.
# ---------------------------------

dir.old <- getwd()
setwd(dirname.data)
geo.dat <- readShapePoly(fn = "kunta4500")
Encoding(levels(geo.dat$nimi)) <- "latin1"

geo.dat.dataslot <- slot(geo.dat, "data")


# ---------------------------------
# Create some test data.
# ---------------------------------

tmp <- runif(320)
tmp.mun.code <- read.table(paste(dirname.data, "/municipalities-in-2013.csv", 
	sep = ""), header = T, sep = ",")
# tmp <- cbind(tmp, as.numeric(as.character(geo.dat.dataslot$kunta)))
tmp <- cbind(tmp, as.numeric(as.character(tmp.mun.code$x)))
mun.dat <- data.frame(tmp)
names(mun.dat) <- c("dat", "mun.code")


# ****************
# Missing information.
#
# 1) There is a municipal code in the data, but no observation.
# 2) There is no municipal code.
#
# You can comment out 1), 2), or both to check the functionality.

# 1) Add some municipalities for which there is no data.
# mun.dat$dat[match(c(1, 10, 100), as.numeric(mun.dat$mun.code))] <- NA
# I haven't encounter that case.

# 2) Add some missing municipalities to show how to handle that.
mun.dat$dat <- as.numeric(as.character(mun.dat$dat))
mun.dat$mun.code[mun.dat$dat > 0.5] <- NA

# ****************



# ***************** IMPORTANT ********************
# Sort the data according to the municipality numbers.
# The geo data is sorted according to the municipality numbers.
# See the TEST RUN below on that.
# Clearly, this is not an issue in this test data.
# But any other data need not be sorted like this!
mun.dat <- mun.dat[order(mun.dat$mun.code, decreasing = F), ]

# Set the limits for grouping the data.
# tmp.at = seq(min(map.data$dat[-missing.mun.ids]), max(map.data$dat[-missing.mun.ids]) + 
# 0.1, 0.1)
# # tmp.at = seq(0, 1, 0.1)


# tmp.at = seq(min(map.incomplete.data$dat), max(map.incomplete.data$dat) + 
# 0.1, 0.1)
# # tmp.at = seq(0, 1, 0.1)


mun.min <- min(mun.dat$dat)
mun.max <- max(mun.dat$dat)
mun.step <- 0.1

# Group the data.
mun.dat$mydat <- mun.dat$dat
mun.dat$mydat[which(mun.dat$dat < 0.1)] = "1:<0.1"
mun.dat$mydat[which(mun.dat$dat >= 0.1 & mun.dat$dat < 0.2)] = "2:0.1-0.2"
mun.dat$mydat[which(mun.dat$dat >= 0.2 & mun.dat$dat < 0.3)] = "3:0.2-0.3"
mun.dat$mydat[which(mun.dat$dat >= 0.3 & mun.dat$dat < 0.4)] = "4:0.3-0.4"
mun.dat$mydat[which(mun.dat$dat >= 0.4 & mun.dat$dat < 0.5)] = "5:0.4-0.5"
mun.dat$mydat[which(mun.dat$dat >= 0.5 & mun.dat$dat < 0.6)] = "6:0.5-0.6"
mun.dat$mydat[which(mun.dat$dat >= 0.6)] = "7:>0.6"
mun.dat$mydat <- factor(mun.dat$mydat)



# **************************************************************************************
#
# EVERYTHING BELOW THIS LINE SHOULDN'T NEED CUSTOMIZING.
#
# **************************************************************************************






# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Match geo data and data to be displayed.
#
# - - - - - - - - - - - - - - - - - - - - - - 

# Fetch the ids from the polygon object (from zero to max number of entries).
geo.ids <- sapply(geo.dat@polygons, function(x) x@ID)

# Match the municipality codes in the data and the polygon object.
geo.kunta.num <- as.numeric(as.character((geo.dat.dataslot$kunta)))
# Clearly, this is a perfect match by construction with this test data.
# However, it need not be the case.
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


# TEST RUN
# Test run to see whether the geographical data is indeed sorted by municipality codes.
# tmp.select=c(49,91,92,235)
# geo.dat.dataslot$nimi[geo.kunta.num%in%tmp.select]
# mtch.ids <- match(tmp.select,geo.kunta.num)
# mun.dat$mun.code[mtch.ids]

# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Create maps.
#
# - - - - - - - - - - - - - - - - - - - - - - 

# ---------------------------------
# Plot the geographic data only.
# ---------------------------------

# pdf(file = paste(dirname.pics, "map-fin-geo-only.pdf", sep = ""))
# plot(geo.dat)
# dev.off()



# ---------------------------------
# Plotting is conditional on missing information.
# ---------------------------------


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Information is complete.
#
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++




if ((length(missing.mun.ids) == 0 & length(missing.data.ids) == 0) | (length(missing.mun.ids) != 
	0 & length(missing.data.ids) == 0)) {




	# ---------------------------------
	# Plot the map with the data.

	# Prepare the data.
	classes <- levels(factor(map.data$dat))
	nClasses <- length(classes)

	plot.data <- spplot(map.data["dat"], col.regions = colorRampPalette(c("blue", 
		"red"))(nClasses), main = list(label = sav.plot.title))
	pdf(file = paste(dirname.pics, sav.pic.name, ".pdf", sep = ""))
	print(plot.data)
	dev.off()


	# ---------------------------------
	# Plot data by groups - the simple way.


	tmp.at = seq(mun.min, mun.max + mun.step, mun.step)

	plot.data <- spplot(map.data["dat"], at = tmp.at, col.regions = colorRampPalette(c("blue", 
		"red"))(length(tmp.at) + 1), main = list(label = sav.plot.title))


	pdf(file = paste(dirname.pics, sav.pic.name, "-grouped.pdf", sep = ""))
	print(plot.data)
	dev.off()

	# nClasses = length(tmp.at)
	# ## distance between hues
# step <- nClasses 
# ## hues equally spaced
# hue = (30 + step*(seq_len(nClasses)-1))%%360 
# qualPal <- hcl(hue, c=50, l=70)
# plot.data<-spplot(map.incomplete.data["dat"],at=tmp.at,col.regions=qualPal)



	# ---------------------------------
	# Plot grouped data.


	plot.data <- spplot(map.data["mydat"], col.regions = colorRampPalette(c("blue", 
		"red"))(length(unique(mun.dat$mydat))))


	pdf(file = paste(dirname.pics, sav.pic.name, "-grouped-custom.pdf", sep = ""))
	print(plot.data)
	dev.off()

}



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# If some municipalities and observations for some municipalities are missing. 
# Missing information 1) and 2).
#
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++


if (length(missing.data.ids) != 0 & length(missing.mun.ids) != 0) {



	# ---------------------------------
	# Plot the map with the data.

	# - - - - - - - - - - - - - - - - - 
	# The data without the missing information.
# - - - - - - - - - - - - - - - - - 

	map.incomplete.data <- map.data[-c(missing.data.ids, missing.mun.ids), ]

	# Prepare the data.
	classes <- levels(factor(map.incomplete.data$dat))
	nClasses <- length(classes)

	plot.data <- spplot(map.incomplete.data["dat"], col.regions = colorRampPalette(c("blue", 
		"red"))(nClasses), main = list(label = sav.plot.title))
	pdf(file = paste(dirname.pics, "map-fin-step-1.pdf", sep = ""))
	print(plot.data)
	dev.off()


	# ****************
	# Missing information.

	# - - - - - - - - - - - - - - - - - 
	# 1) The data for which there are codes, but no observations.

	map.missing.data <- map.data[missing.data.ids, ]
	# Add a pseudo value.
	map.missing.data$dat <- 10000 * max(map.incomplete.data$dat)
	plot.missing.data <- spplot(map.missing.data["dat"], col.regions = colorRampPalette(c("yellow", 
		"yellow"))(1))
	# plot.missing.data
	# dev.off()
plot.step2 <- plot.data + plot.missing.data

	pdf(file = paste(dirname.pics, "map-fin-step-2.pdf", sep = ""))
	print(plot.step2)
	dev.off()




	# - - - - - - - - - - - - - - - - - 
	# 2) The data for which there no codes.

	map.missing.mun <- map.data[missing.mun.ids, ]
	map.missing.mun$dat <- 10000 * max(map.incomplete.data$dat)

	plot.missing.mun <- spplot(map.missing.mun["dat"], col.regions = colorRampPalette(c("grey", 
		"grey"))(1))
	# plot.missing.mun
	# dev.off()

	# ****************
	

	# - - - - - - - - - - - - - - - - - 
	# The complete map.


	plot.full <- plot.data + plot.missing.data + plot.missing.mun

	pdf(file = paste(dirname.pics, sav.pic.name, ".pdf", sep = ""))
	print(plot.full)
	dev.off()


	# ---------------------------------
	# Plot data by groups - the simple way.



	tmp.at = seq(mun.min, mun.max + mun.step, mun.step)

	plot.data <- spplot(map.incomplete.data["dat"], at = tmp.at, col.regions = colorRampPalette(c("blue", 
		"red"))(length(tmp.at) + 1), main = list(label = sav.plot.title))

	plot.full <- plot.data + plot.missing.data + plot.missing.mun

	pdf(file = paste(dirname.pics, sav.pic.name, "-grouped.pdf", sep = ""))

	print(plot.full)
	dev.off()

	# nClasses = length(tmp.at)
	# ## distance between hues
# step <- nClasses 
# ## hues equally spaced
# hue = (30 + step*(seq_len(nClasses)-1))%%360 
# qualPal <- hcl(hue, c=50, l=70)
# plot.data<-spplot(map.incomplete.data["dat"],at=tmp.at,col.regions=qualPal)



	# ---------------------------------
	# Plot grouped data.



	plot.data <- spplot(map.incomplete.data["mydat"], col.regions = colorRampPalette(c("blue", 
		"red"))(length(unique(mun.dat$mydat)) - 1), main = list(label = sav.plot.title))


	map.missing.data$mydat <- 10000 * max(map.incomplete.data$dat)
	plot.missing.data <- spplot(map.missing.data["mydat"], col.regions = colorRampPalette(c("yellow", 
		"yellow"))(1))

	map.missing.mun$mydat <- 10000 * max(map.incomplete.data$dat)

	plot.missing.mun <- spplot(map.missing.mun["mydat"], col.regions = colorRampPalette(c("grey", 
		"grey"))(1))

	plot.full <- plot.data + plot.missing.data + plot.missing.mun


	pdf(file = paste(dirname.pics, sav.pic.name, "-grouped-custom.pdf", sep = ""))

	print(plot.full)
	dev.off()

}





# +++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# If observations for some municipalities are missing. 
# Missing information 1).
#
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++


if (length(missing.data.ids) != 0 & length(missing.mun.ids) == 0) {


	# ---------------------------------
	# Plot the map with the data.


	# - - - - - - - - - - - - - - - - - 
	# The data without the missing information.


	map.incomplete.data <- map.data[-c(missing.data.ids), ]

	# Prepare the data.
	classes <- levels(factor(map.incomplete.data$dat))
	nClasses <- length(classes)

	plot.data <- spplot(map.incomplete.data["dat"], col.regions = colorRampPalette(c("blue", 
		"red"))(nClasses), main = list(label = sav.plot.title))
	pdf(file = paste(dirname.pics, "map-fin-step-1.pdf", sep = ""))
	print(plot.data)
	dev.off()


	# ****************
	# Missing information.

	# - - - - - - - - - - - - - - - - - 
	# 1) The data for which there are codes, but no observations.


	map.missing.data <- map.data[missing.data.ids, ]
	# Add a pseudo value.
	map.missing.data$dat <- 10000 * max(map.incomplete.data$dat)
	plot.missing.data <- spplot(map.missing.data["dat"], col.regions = colorRampPalette(c("yellow", 
		"yellow"))(1))
	# plot.missing.data
	# dev.off()
plot.step2 <- plot.data + plot.missing.data
	pdf(file = paste(dirname.pics, "map-fin-step-2.pdf", sep = ""))
	print(plot.step2)
	dev.off()




	# - - - - - - - - - - - - - - - - - 
	# The complete map.


	plot.full <- plot.data + plot.missing.data


	pdf(file = paste(dirname.pics, sav.pic.name, ".pdf", sep = ""))
	print(plot.full)
	dev.off()


	# ---------------------------------
	# Plot data by groups - the simple way.
tmp.at = seq(mun.min, mun.max + mun.step, mun.step)


	plot.data <- spplot(map.incomplete.data["dat"], at = tmp.at, col.regions = colorRampPalette(c("blue", 
		"red"))(length(tmp.at) + 1), main = list(label = sav.plot.title))

	plot.full <- plot.data + plot.missing.data

	pdf(file = paste(dirname.pics, sav.pic.name, "-grouped.pdf", sep = ""))
	print(plot.full)
	dev.off()

	# nClasses = length(tmp.at)
	# ## distance between hues
# step <- nClasses 
# ## hues equally spaced
# hue = (30 + step*(seq_len(nClasses)-1))%%360 
# qualPal <- hcl(hue, c=50, l=70)
# plot.data<-spplot(map.incomplete.data["dat"],at=tmp.at,col.regions=qualPal)



	# ---------------------------------
	# Plot grouped data.



	plot.data <- spplot(map.incomplete.data["mydat"], col.regions = colorRampPalette(c("blue", 
		"red"))(length(unique(mun.dat$mydat)) - 1), main = list(label = sav.plot.title))


	map.missing.data$mydat <- 10000 * max(map.incomplete.data$dat)
	plot.missing.data <- spplot(map.missing.data["mydat"], col.regions = colorRampPalette(c("yellow", 
		"yellow"))(1))


	plot.full <- plot.data + plot.missing.data


	pdf(file = paste(dirname.pics, sav.pic.name, "-grouped-custom.pdf", sep = ""))
	print(plot.full)
	dev.off()

}
