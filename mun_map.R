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
#



# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		
#
# - - - - - - - - - - - - - - - - - - - - - - 

# Clear workspace.
rm(list = ls())




# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Packages.
#
# - - - - - - - - - - - - - - - - - - - - - - 

library("sp")
library("ggplot2")
library("rgdal")
library("plyr")
library("rgeos")
library("maptools")
library("reshape2")
library("grid")
library("gridExtra")


# - - - - - - - - - - - - - - - - - - - - - -  
#
# 	Settings.
#
# - - - - - - - - - - - - - - - - - - - - - - 


# Standard map settings.
sav.mapplot.setup <- theme(axis.text = element_blank(), axis.ticks = element_blank(), 
	panel.grid.minor = element_blank(), panel.background = element_blank(), 
	panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), 
	panel.border = element_blank(), legend.title = element_blank(), legend.position = c(0.25, 
		0.5), axis.line = element_blank(), axis.title = element_blank())



# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Functions.
#
# - - - - - - - - - - - - - - - - - - - - - - 

# Returns string w/o leading or trailing whitespace.
fun.trim <- function(x) gsub("^\\s+|\\s+$", "", x)
# Remove all punctuation.
fun.rm.pnct <- function(x) gsub("[[:punct:]]", " ", x)


# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Directories.
#
# - - - - - - - - - - - - - - - - - - - - - - 

dirname.geo.data <- "~/RRR_finn/data/statfin/geo/kunta4500"
dirname.data.mun <- "~/RRR_finn/data/statfin/municipal/"

dirname.pics <- "~/RRR_finn/pics/"



# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Read the data into memory.
#
# - - - - - - - - - - - - - - - - - - - - - - 

# ---------------------------------
# Geographical data.
# ---------------------------------

setwd(dirname.geo.data)

geo.dat = readOGR(dsn = ".", layer = "kunta4500")
geo.dat@data$id = rownames(geo.dat@data)
geo.dat.points = fortify(geo.dat, region = "id")
geo.dat = join(geo.dat.points, geo.dat@data, by = "id")

Encoding(levels(geo.dat$nimi)) <- "latin1"




# ---------------------------------
# Unemployment data.
# ---------------------------------

mun.dat <- read.table(paste(dirname.data.mun, "municipal-unempl-rate-march-2006-2013.csv", 
	sep = ""), sep = ",", stringsAsFactors = F, header = T)








# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		
#
# - - - - - - - - - - - - - - - - - - - - - - 



sav.plot.title <- "Unemployment"

years = 2006:2007

# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Modify municipal data.
#
# - - - - - - - - - - - - - - - - - - - - - - 


# Prepare the data for the conventions below:
names(mun.dat)[(names(mun.dat) == "Kunta.code")] <- "mun.code"
names(mun.dat)[(names(mun.dat) == "Kunta")] <- "mun.name"
names(mun.dat)[(names(mun.dat) == "Kuukausi")] <- "month"
#names(mun.dat)[(names(mun.dat)=="ACTUAL_DATA")]<-"dat"



# Group the data.
mun.dat$mydat <- mun.dat$dat
mun.dat$mydat[which(mun.dat$dat < 5)] = "1: <5"
mun.dat$mydat[which(mun.dat$dat >= 5 & mun.dat$dat < 10)] = "2: 5-10"
mun.dat$mydat[which(mun.dat$dat >= 10 & mun.dat$dat < 15)] = "3: 10-15"
mun.dat$mydat[which(mun.dat$dat >= 15 & mun.dat$dat < 20)] = "4: 15-20"
mun.dat$mydat[which(mun.dat$dat >= 20)] = "5: >20"
mun.dat$mydat <- factor(mun.dat$mydat)


# Extract the data in the panel format.
# tmp.df = dcast(mun.dat, mun.code ~ month, value.var = "dat")
tmp.df = dcast(mun.dat, mun.code ~ month, value.var = "mydat")

# Make it pretty.
tmp.months = names(tmp.df)[grep("kuu", names(tmp.df))]
tmp.months = gsub("Maaliskuu", " ", tmp.months)
tmp.months = gsub("^", "March ", tmp.months)
tmp.months = fun.trim(tmp.months)
names(tmp.df)[grep("kuu", names(tmp.df))] = tmp.months


# - - - - - - - - - - - - - - - - - - - - - -  
#
# 	Combine geo data and data to be displayed.
#
# - - - - - - - - - - - - - - - - - - - - - - 


geo.dat$kunta = as.numeric(as.character(geo.dat$kunta))
map.dat = merge(x = geo.dat, y = tmp.df, by.x = "kunta", by.y = "mun.code")
names(map.dat) <- make.names(names(map.dat))

rm(list = ls(pattern = "tmp"))


# - - - - - - - - - - - - - - - - - - - - - -  
#
# 		Create maps.
#
# - - - - - - - - - - - - - - - - - - - - - - 




# - - - - - - - - - - - - - - - - - - - - - -  
# Loop over years.
# - - - - - - - - - - - - - - - - - - - - - - 


for (i in years) {
	tmp.var = paste("March.", i, sep = "")
	tmp.title = paste(sav.plot.title, fun.rm.pnct(tmp.var), sep = " ")
	tmp.pic.name = paste("map-", tolower(gsub("\\s+", "-", tmp.title)), sep = "")
	# Test how missing data appears.
	map.dat[[tmp.var]][map.dat$kunta == "5"] <- NA
	map.dat[[tmp.var]][map.dat$kunta == "889"] <- NA
	
	
	tmp.plot = ggplot(geo.dat)+  scale_fill_brewer(type = "seq", palette = "YlOrRd") + 
		aes(long, lat, group = group, fill = map.dat[[tmp.var]]) + geom_polygon() + 
		geom_path(color = "black") + coord_equal() + sav.mapplot.setup + ggtitle(tmp.title)

# tmp.plot + geom_point(data=tmp.add[1,],aes(long,lat),shape=4,size=6,show_guide=F)+ geom_point(data=tmp.add[2,],aes(long,lat),shape=8,size=6,show_guide=F)+ geom_point(data=tmp.add[3,],aes(long,lat),shape=13,size=6,show_guide=F)+
# geom_point(data=tmp.add[4,],aes(long,lat),shape=21,size=6,show_guide=F,fill="green")+
# geom_point(data=tmp.add[5,],aes(long,lat),shape=10,size=6,show_guide=F,solid=F,color="green")
	
	
	ggsave(tmp.plot, file = paste(dirname.pics, tmp.pic.name, ".pdf", sep = ""), 
		width = 7, height = 10)

	tmp.plot.1 = tmp.plot + theme(legend.position = "none", title = element_blank())
	assign(paste("sav.plot.", i, sep = ""), tmp.plot.1)


}


# Add points filled with the color of the variable.
tmp.plot +
geom_point(data=map.dat[map.dat$nimi=="Helsinki",][15,],aes(long,lat,fill=map.dat$"March.2006"[map.dat$nimi=="Helsinki"][1]),color="white",shape=21,size=6,show_guide=F)+
geom_point(data=map.dat[map.dat$nimi=="Tampere",][10,],aes(long,lat,fill=map.dat$"March.2006"[map.dat$nimi=="Tampere"][1]),color="white",shape=21,size=6,show_guide=F)+
geom_point(data=map.dat[map.dat$nimi=="Kotka",][10,],aes(long,lat,fill=map.dat$"March.2006"[map.dat$nimi=="Kotka"][1]),color="white",shape=21,size=6,show_guide=F)+
geom_point(data=map.dat[map.dat$nimi=="Rauma",][12,],aes(long,lat,fill=map.dat$"March.2006"[map.dat$nimi=="Rauma"][1]),color="white",shape=21,size=6,show_guide=F)+
geom_point(data=map.dat[map.dat$nimi=="Inari",][15,],aes(long,lat,fill=map.dat$"March.2006"[map.dat$nimi=="Inari"][1]),color="white",shape=21,size=6,show_guide=F)+ geom_text(data = NULL, aes(x = map.dat$long[map.dat$nimi=="Inari"][15], y = map.dat$lat[map.dat$nimi=="Inari"][15]), size = 5, 
	label = "Inari")



# - - - - - - - - - - - - - - - - - - - - - -  
# Panel plots.
# - - - - - - - - - - - - - - - - - - - - - - 

tmp.a = sav.plot.2006
tmp.b = sav.plot.2007
tmp.c = sav.plot.2008
tmp.d = sav.plot.2009
tmp.e = sav.plot.2010
tmp.f = sav.plot.2011
tmp.g = sav.plot.2012
tmp.h = sav.plot.2013

tmp.grid = grid.arrange(tmp.a, tmp.b, tmp.c, tmp.d, tmp.e, tmp.f, tmp.g, tmp.h, 
	nrow = 3, ncol = 3)
	
rm(list = ls(pattern = "tmp"))


# tmp.a <- ggplot_gtable(ggplot_build(tmp.a))
# tmp.b <- ggplot_gtable(ggplot_build(tmp.b))
# maxWidth = grid::unit.pmax(tmp.a$widths[2:3], tmp.b$widths[2:3])
# tmp.a$widths[2:3] <- as.list(maxWidth)
# tmp.b$widths[2:3] <- as.list(maxWidth)
# grid.arrange(tmp.a, tmp.b, ncol=1)


# - - - - - - - - - - - - - - - - - - - - - -  
# Points of interest.
# - - - - - - - - - - - - - - - - - - - - - - 


# Pick points of interest.
tmp.names = c("Helsinki", "Turku", "Tampere", "Rauma", "Kuhmo","Kotka")
tmp.add = data.frame(matrix(NA, length(tmp.names), 3))
names(tmp.add) = c("place", "long", "lat")
tmp.add$place = tmp.names
for (k in tmp.names) {
	tmp = geo.dat[geo.dat$nimi == k, ]
	tmp.add$long[tmp.add$place == k] = tmp$long[5]
	tmp.add$lat[tmp.add$place == k] = tmp$lat[5]
	if(k %in% c("Helsinki","Kuhmo","Kotka")){tmp.add$long.end[tmp.add$place == k] = tmp$long[5]+100000}
	if(k %in% c("Turku","Tampere","Rauma")){tmp.add$long.end[tmp.add$place == k] = tmp$long[5]-100000}
		
	tmp.add$lat.end[tmp.add$place == k] = tmp$lat[5]
	tmp.add$group[tmp.add$place == k] = as.character(tmp$group[1])

}

# The base map.
tmp.plot = ggplot(geo.dat) + aes(long, lat, group = group, fill = group) + geom_polygon() + 
	geom_path(color = "white") + coord_equal() + theme(legend.position = "none")


# Add points to the map.
tmp.plot + geom_point(data=tmp.add[1,],aes(long,lat),shape=21,size=6,)+ geom_point(data=tmp.add[2,],aes(long,lat),shape=22,size=6)+ geom_point(data=tmp.add[3,],aes(long,lat),shape=23,size=6)+
geom_point(data=tmp.add[4,],aes(long,lat),shape=24,size=6)+
geom_point(data=tmp.add[5,],aes(long,lat),shape=25,size=6)



# Add arrows.
tmp.plot=tmp.plot + geom_segment(aes(x=tmp.add$long[1],y=tmp.add$lat[1],xend=tmp.add$long.end[1],yend=tmp.add$lat.end[1]),arrow=arrow(angle=10,length=unit(0.5,'cm'),type="closed",ends="first"))
# Add label.
  tmp.plot=tmp.plot + geom_text(data = NULL, aes(x=tmp.add$long.end[1]*1.15,y=(tmp.add$lat.end[1])), size = 5, label = tmp.names[1])
  # Add arrows.
tmp.plot=tmp.plot + geom_segment(aes(x=tmp.add$long[2],y=tmp.add$lat[2],xend=tmp.add$long.end[2],yend=tmp.add$lat.end[2]),arrow=arrow(angle=10,length=unit(0.5,'cm'),type="closed",ends="first"))
# Add label.
  tmp.plot=tmp.plot + geom_text(data = NULL, aes(x=tmp.add$long.end[2]*1.15,y=(tmp.add$lat.end[2])), size = 5, label = tmp.names[2])
  # Add arrows.
tmp.plot=tmp.plot + geom_segment(aes(x=tmp.add$long[3],y=tmp.add$lat[3],xend=tmp.add$long.end[3],yend=tmp.add$lat.end[3]),arrow=arrow(angle=10,length=unit(0.5,'cm'),type="closed",ends="first"))
# Add label.
  tmp.plot=tmp.plot + geom_text(data = NULL, aes(x=tmp.add$long.end[3]*1.15,y=(tmp.add$lat.end[3])), size = 5, label = tmp.names[3])
    # Add arrows.
tmp.plot=tmp.plot + geom_segment(aes(x=tmp.add$long[4],y=tmp.add$lat[4],xend=tmp.add$long.end[4],yend=tmp.add$lat.end[4]),arrow=arrow(angle=10,length=unit(0.5,'cm'),type="closed",ends="first"))
# Add label.
  tmp.plot=tmp.plot + geom_text(data = NULL, aes(x=tmp.add$long.end[4]*1.15,y=(tmp.add$lat.end[4])), size = 5, label = tmp.names[4])
	



