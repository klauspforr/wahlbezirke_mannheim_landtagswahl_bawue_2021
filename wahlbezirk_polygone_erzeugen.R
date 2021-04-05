rm(list=ls())
gc()

# load data set from hard disk
load("streetdata.Rdata")

# install packages
if(!require("sf")) install.packages("sf") 
if(!require("concaveman")) install.packages("concaveman")

# load packages
library(sf)
library(concaveman)
library(ggplot2)
library(sp)

# create polygons
a<-1
b<-117
Wahlbezirke<-lapply(
  unique(streetdata$Wahlbezirk)[a:b],function(x)
        Polygons(
          list(
            Polygon(
              as_Spatial(
                concaveman(
                  st_as_sf(
                    streetdata[which(streetdata$Wahlbezirk==x),3:4],coords=c('lon','lat'),crs=4326
                  )
                )
              )@polygons[[1]]@Polygons[[1]]@coords,hole=F
            )
          ),list(x)
        )
)
Wahlbezirke<-SpatialPolygons(Wahlbezirke,
                             as.integer(rank(unique(streetdata$Wahlbezirk)[a:b])),
                             proj4string=CRS("EPSG:4326"))

# import and merge election data

plot(Wahlbezirke)
