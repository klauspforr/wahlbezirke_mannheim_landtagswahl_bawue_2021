rm(list=ls())
gc()

#install packages
if(!require("xlsx")) install.packages("xlsx") 
if(!require("tidyr")) install.packages("tidyr")
if(!require("dplyr")) install.packages("dplyr")
if(!require("tmaptools")) install.packages("tmaptools")



#load packages
library(xlsx)
library(tidyr)
library(dplyr)
library(tmaptools)
library(osmdata)
library(sp)
library(sf)
library(ggplot2)
library(ggmap)

# import street data from excel file
streetdata<-as_tibble(read.xlsx2(file="StrVerz_LTW21.xlsx",
                                 sheetName="str_ltw21",
                                 colIndex=1:7,
                                 as.data.frame=T,
                                 header=T))

# repair block addresses
streetdata<-as.data.frame(streetdata)
streetdata[substr(streetdata[,1],1,2) %in% c("A ","B ","C ","D ","E ","F ","G ","H ","I ","J ",
                                             "K ","L ","M ","N ","O ","P ","Q ","R ","S ","T ","U "),1]<-
  sub(" ","",streetdata[substr(streetdata[,1],1,2) %in% c("A ","B ","C ","D ","E ","F ","G ","H ","I ","J ",
                                                          "K ","L ","M ","N ","O ","P ","Q ","R ","S ","T ","U "),1])
# repair streetname abbrevations
streetdata[grep("Stra ",streetdata[,1]),1]<-sub("Stra ","Straße ",streetdata[grep("Stra ",streetdata[,1]),1])
streetdata[grep("Str ",streetdata[,1]),1]<-sub("Str ","Straße ",streetdata[grep("Str ",streetdata[,1]),1])
streetdata[grep("St ",streetdata[,1]),1]<-sub("St ","Straße ",streetdata[grep("St ",streetdata[,1]),1])
streetdata[grep("S ",streetdata[,1]),1]<-sub("S ","Straße ",streetdata[grep("S ",streetdata[,1]),1])
streetdata[grep("Str. ",streetdata[,1]),1]<-sub("Str. ","Straße ",streetdata[grep("Str. ",streetdata[,1]),1])
streetdata[grep("str. ",streetdata[,1]),1]<-sub("str. ","straße ",streetdata[grep("str. ",streetdata[,1]),1])
streetdata[grep("W ",streetdata[,1]),1]<-sub("W ","Weg ",streetdata[grep("W ",streetdata[,1]),1])
streetdata[grep("W. ",streetdata[,1]),1]<-sub("W. ","Weg ",streetdata[grep("W. ",streetdata[,1]),1])
streetdata[grep("Rg. ",streetdata[,1]),1]<-sub("Rg. ","Ring ",streetdata[grep("Rg. ",streetdata[,1]),1])
streetdata[grep("Pl. ",streetdata[,1]),1]<-sub("Pl. ","Platz ",streetdata[grep("Pl. ",streetdata[,1]),1])
streetdata[grep("Am Meßplatz",streetdata[,1]),1]<-sub("Am Meßplatz","Am Messplatz",streetdata[grep("Am Meßplatz",streetdata[,1]),1])
streetdata[grep("An d. Kammerschleuse",streetdata[,1]),1]<-sub("An d. Kammerschleuse","An der Kammerschleuse",streetdata[grep("An d. Kammerschleuse",streetdata[,1]),1])
streetdata[grep("Bürgerm.-Fuchs-Straße",streetdata[,1]),1]<-sub("Bürgerm.-Fuchs-Straße","Bürgermeister-Fuchs-Straße",streetdata[grep("Bürgerm.-Fuchs-Straße",streetdata[,1]),1])
streetdata[grep("Chr.-",streetdata[,1]),1]<-sub("Chr.-","Christian-",streetdata[grep("Chr.-",streetdata[,1]),1])
streetdata[grep("Der Hohe Weg Z.Rhein",streetdata[,1]),1]<-sub("Der Hohe Weg Z.Rhein","Der Hohe Weg Zum Rhein",streetdata[grep("Der Hohe Weg Z.Rhein",streetdata[,1]),1])
streetdata[grep("Dietr.Bonhoeffer",streetdata[,1]),1]<-sub("Dietr.Bonhoeffer","Dietrich-Bonhoeffer",streetdata[grep("Dietr.Bonhoeffer",streetdata[,1]),1])
streetdata[grep("Elis.-V-Thadden",streetdata[,1]),1]<-sub("Elis.-V-Thadden","Elisabeth-Von-Thadden",streetdata[grep("Elis.-V-Thadden",streetdata[,1]),1])
streetdata[grep("Esther-Charl.-Brandes",streetdata[,1]),1]<-sub("Esther-Charl.-Brandes","Esther-Charlotte-Brandes",streetdata[grep("Esther-Charl.-Brandes",streetdata[,1]),1])
streetdata[grep("Friedr.",streetdata[,1]),1]<-sub("Friedr.","Friedrich",streetdata[grep("Friedr.",streetdata[,1]),1])
streetdata[grep("-V-",streetdata[,1]),1]<-sub("-V-","-Von-",streetdata[grep("-V-",streetdata[,1]),1])
streetdata[grep("-V.-",streetdata[,1]),1]<-sub("-V.-","-Von-",streetdata[grep("-V.-",streetdata[,1]),1])
streetdata[grep("E.-Altmann-Gottheiner",streetdata[,1]),1]<-sub("E.-Altmann-Gottheiner","Elisabeth-Altmann-Gottheiner",streetdata[grep("E.-Altmann-Gottheiner",streetdata[,1]),1])
streetdata[grep("Fred-J.-Schoeps-Straße",streetdata[,1]),1]<-sub("Fred-J.-Schoeps-Straße","Fred-Joachim-Schoeps-Straße",streetdata[grep("Fred-J.-Schoeps-Straße",streetdata[,1]),1])
streetdata[grep("Gerh.-Hauptmann-Straße",streetdata[,1]),1]<-sub("Gerh.-Hauptmann-Straße","Gerhart-Hauptmann-Straße",streetdata[grep("Gerh.-Hauptmann-Straße",streetdata[,1]),1])
streetdata[grep("Herm.-Gropengießer",streetdata[,1]),1]<-sub("Herm.-Gropengießer","Hermann-Gropengießer",streetdata[grep("Herm.-Gropengießer",streetdata[,1]),1])
streetdata[grep("Herm.-Heimerich",streetdata[,1]),1]<-sub("Herm.-Heimerich","Hermann-Heimerich",streetdata[grep("Herm.-Heimerich",streetdata[,1]),1])
streetdata[grep("Herm.-Hesse",streetdata[,1]),1]<-sub("Herm.-Hesse","Hermann-Hesse",streetdata[grep("Herm.-Hesse",streetdata[,1]),1])
streetdata[grep("Herm.-Löns",streetdata[,1]),1]<-sub("Herm.-Löns","Hermann-Löns",streetdata[grep("Herm.-Löns",streetdata[,1]),1])
streetdata[grep("Neischwand.",streetdata[,1]),1]<-sub("Neischwand.","Neischwander",streetdata[grep("Neischwand.",streetdata[,1]),1])
streetdata[grep("Phil.-Brunnemer",streetdata[,1]),1]<-sub("Phil.-Brunnemer","Philipp-Brunnemer",streetdata[grep("Phil.-Brunnemer",streetdata[,1]),1])
streetdata[grep("Reichsk.-Müller",streetdata[,1]),1]<-sub("Reichsk.-Müller","Reichskanzler-Müller",streetdata[grep("Reichsk.-Müller",streetdata[,1]),1])
streetdata[grep("Tauberbischofsh.Straße",streetdata[,1]),1]<-sub("Tauberbischofsh.Straße","Tauberbischofsheimer Straße",streetdata[grep("Tauberbischofsh.Straße",streetdata[,1]),1])
streetdata[grep("Verb.-Kanal Li.Ufer",streetdata[,1]),1]<-sub("Verb.-Kanal Li.Ufer","Verbindungskanal Linkes Ufer",streetdata[grep("Verb.-Kanal Li.Ufer",streetdata[,1]),1])
streetdata[grep("Verl.Jungbuschstraße",streetdata[,1]),1]<-sub("Verl.Jungbuschstraße","Verlängerte Jungbuschstraße",streetdata[grep("Verl.Jungbuschstraße",streetdata[,1]),1])
streetdata[grep("Wilh.-Furtwängl.Straße",streetdata[,1]),1]<-sub("Wilh.-Furtwängl.Straße","Wilhelm-Furtwänglänger-Straße",streetdata[grep("Wilh.-Furtwängl.Straße",streetdata[,1]),1])
streetdata[grep("Christian-FriedrichSchwan",streetdata[,1]),1]<-sub("Christian-FriedrichSchwan","Christian-Friedrich-Schwan",streetdata[grep("Christian-FriedrichSchwan",streetdata[,1]),1])
streetdata[grep("Friedrichch",streetdata[,1]),1]<-sub("Friedrichch","Friedrich",streetdata[grep("Friedrichch",streetdata[,1]),1])
streetdata[grep("Hanns-M-Schleyer",streetdata[,1]),1]<-sub("Hanns-M-Schleyer","Hanns-Martin-Schleyer",streetdata[grep("Hanns-M-Schleyer",streetdata[,1]),1])
streetdata[grep("Marienwerder Weg",streetdata[,1]),1]<-sub("Marienwerder Weg","Marienwerderweg",streetdata[grep("Marienwerder Weg",streetdata[,1]),1])
streetdata[grep("Seckenheimer Hauptst",streetdata[,1]),1]<-sub("Seckenheimer Hauptst","Seckenheimer Hauptstraße",streetdata[grep("Seckenheimer Hauptst",streetdata[,1]),1])
streetdata[grep("Wilhelm-Furtwänglänger-Straße",streetdata[,1]),1]<-sub("Wilhelm-Furtwänglänger-Straße","Wilhelm-Furtwängler-Straße",streetdata[grep("Wilhelm-Furtwänglänger-Straße",streetdata[,1]),1])
streetdata<-streetdata[-grep("Gewann auf den Ried",streetdata[,1]),]
streetdata<-streetdata[-grep("Alter Rangierbahnhof",streetdata[,1]),]

# extend streetdata by adresses between starting and end address
# init new data set
#streetdata<-subset(streetdata,Gerade.Ungerade=="F")
streetdata2<- streetdata[1,c(1,2,3,7)]
streetdata2<-streetdata2[-1,]
for (i in 1:nrow(streetdata)) {
  if (streetdata[i,4]=="F") {
    osm_query<-c(
      opq(bbox="Mannheim") %>%
        add_osm_feature(key = 'addr:place', value = streetdata[i,1],value_exact = T) %>%
        add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
        osmdata_sf (),
      opq(bbox="Mannheim") %>%
        add_osm_feature(key = 'addr:street', value = streetdata[i,1],value_exact = T) %>%
        add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
        osmdata_sf ()
    )
    print(i)
    print(streetdata[i,])
    print(iconv(cbind(as.matrix(cbind(osm_query$osm_polygons$addr.place,
                                osm_query$osm_multipolygons$addr.place,
                                osm_query$osm_polygons$addr.street,
                                osm_query$osm_multipolygons$addr.street)),
                as.matrix(cbind(osm_query$osm_polygons$addr.housenumber,
                                osm_query$osm_multipolygons$addr.housenumber))),from="UTF-8",to="UTF-8"))
    streetdata2<-rbind(streetdata2,cbind(as.matrix(cbind(osm_query$osm_polygons$addr.place,
                                                         osm_query$osm_multipolygons$addr.place,
                                                         osm_query$osm_polygons$addr.street,
                                                         osm_query$osm_multipolygons$addr.street)),
                                         as.matrix(cbind(osm_query$osm_polygons$addr.housenumber,
                                                         osm_query$osm_multipolygons$addr.housenumber)),
                                         "",streetdata[i,7]))
  }
  else {
    # streetdata2<-rbind(streetdata2,streetdata[i,c(1,2,3,7)])
    # for (j in seq(from=streetdata[i,2]+ifelse(streetdata[i,4]=="F",1,2),
    #               to=streetdata[i,5]-ifelse(streetdata[i,4]=="F",1,2),
    #               by=ifelse(streetdata[i,4]=="F",1,2))) {
    #   osm_query<-c(
    #     opq(bbox="Mannheim") %>%
    #       add_osm_feature(key = 'addr:place', value = streetdata[i,1]) %>%
    #       add_osm_feature(key = 'addr:housenumber', value = j) %>%
    #       add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
    #       osmdata_sf (),
    #     opq(bbox="Mannheim") %>%
    #       add_osm_feature(key = 'addr:street', value = streetdata[i,1]) %>%
    #       add_osm_feature(key = 'addr:housenumber', value = j) %>%
    #       add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
    #       osmdata_sf ()
    #     )
    #   streetdata2<-rbind(streetdata2,cbind(as.matrix(cbind(osm_query$osm_polygons$addr.place,
    #                                                        osm_query$osm_multipolygons$addr.place,
    #                                                        osm_query$osm_polygons$addr.street,
    #                                                        osm_query$osm_multipolygons$addr.street)),
    #                                        as.matrix(cbind(osm_query$osm_polygons$addr.housenumber,
    #                                                        osm_query$osm_multipolygons$addr.housenumber)),
    #                                        "",streetdata[i,7]))
    #   }
    # streetdata2<-rbind(streetdata2,streetdata[i,c(1,5,6,7)])
  }
}

ggmap(get_map(getbb("Mannheim"),source="osm"))+geom_sf(aes(alpha=0),data=geocode_OSM("Mannheim, Uhlandstrasse 42",as.sf=T),inherit.aes=F)

q<-opq(bbox="Mannheim") %>%
        add_osm_feature(key = 'addr:place', value = 'A1') %>%
        #add_osm_feature(key = 'addr:housenumber', value = '1') %>%
        add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
        osmdata_sf ()

# opq(bbox="Mannheim") %>%
#   add_osm_feature(key = 'addr:street', value = "Am Messplatz",value_exact = T) %>%
#   add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
#   osmdata_sf ()