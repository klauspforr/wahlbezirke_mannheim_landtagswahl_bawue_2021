rm(list=ls())
gc()

# load packages
library(xlsx)
library(tidyr)
library(dplyr)
library(tmaptools)
library(osmdata)
library(sp)
library(sf)
library(ggplot2)
library(ggmap)
library(concaveman)

# Define import objects
other_cities_around<-c("!Viernheim","!Schwetzingen","!Brühl","!Ilvesheim",
                       "!Ludwigshafen am Rhein","!Ludwigshafen",
                       "!Neuhofen","!Altrip","!Lampertheim")
# function 1 Point in Viernheim  -1 not in Viernheim
in_viernheim<-function(x,y) sign((8.560014625341987 - 8.527128161223684) * (y - 49.548191854933705) - (49.523116927432895 - 49.548191854933705) * (x - 8.527128161223684))
# function 1 Point in Ludwigshafen  -1 not in Ludwigshafen
in_lu<-function(x,y) -sign((8.444127013791821 - 8.426280568564767) * (y - 49.53947648803179) - (49.4507411391354 - 49.53947648803179) * (x - 8.426280568564767))
blocklist<-c("A1","A2","A3","A4","A5",
             "B1","B2","B3","B4","B5","B6","B7",
             "C1","C2","C3","C4","C5","C6","C7","C8",
             "D1","D2","D3","D4","D5","D6","D7",
             "E1","E2","E3","E4","E5","E6","E7",
             "F1","F2","F3","F4","F5","F6","F7",
             "G1","G2","G3","G4","G5","G6","G7",
             "H1","H2","H3","H4","H5","H6","H7",
             "I1","I2","I3","I4","I5","I6","I7",
             "K1","K2","K3","K4","K5","K6","K7",
             "L1","L2","L3","L4","L5","L6","L7","L8","L9","L10","L11","L12","L13","L14","L15",
             "M1","M2","M3","M3a","M4","M5","M6","M7",
             "N1","N2","N3","N4","N5","N6","N7",
             "O1","O2","O3","O4","O5","O6","O7",
             "P1","P2","P3","P4","P5","P6","P7",
             "Q1","Q2","Q3","Q4","Q5","Q6","Q7",
             "R1","R2","R3","R4","R5","R6","R7",
             "S1","S2","S3","S4","S5","S6","S7",
             "T1","T2","T3","T4","T5","T6","T7",
             "U1","U2","U3","U4","U5","U6","U7")

# import street data from excel file
streetdata<-as_tibble(read.xlsx2(file="StrVerz_LTW21.xlsx",
                                 sheetName="str_ltw21",
                                 colIndex=1:7,
                                 as.data.frame=T,
                                 header=T,
                                 colClasses="character"))
# add leading zero to Wahlbezirk ID
streetdata$Wahlbezirk[which(nchar(streetdata$Wahlbezirk)==4)]<-
  paste0("0",streetdata$Wahlbezirk[which(nchar(streetdata$Wahlbezirk)==4)])

# repair block addresses
streetdata<-as.data.frame(streetdata)
streetdata[substr(streetdata[,1],1,2) %in% c("A ","B ","C ","D ","E ","F ","G ","H ","I ","J ",
                                             "K ","L ","M ","N ","O ","P ","Q ","R ","S ","T ","U "),1]<-
  sub(" ","",streetdata[substr(streetdata[,1],1,2) %in% c("A ","B ","C ","D ","E ","F ","G ","H ","I ","J ",
                                                          "K ","L ","M ","N ","O ","P ","Q ","R ","S ","T ","U "),1])
# repair streetname abbrevations
streetdata[grep("Stra$",streetdata[,1]),1]<-sub("Stra$","Str",streetdata[grep("Stra$",streetdata[,1]),1])
streetdata[grep("Str$",streetdata[,1]),1]<-sub("Str$","Str",streetdata[grep("Str$",streetdata[,1]),1])
streetdata[grep("St$",streetdata[,1]),1]<-sub("St$","Str",streetdata[grep("St$",streetdata[,1]),1])
streetdata[grep("S$",streetdata[,1]),1]<-sub("S$","Str",streetdata[grep("S$",streetdata[,1]),1])
streetdata[grep("Str.$",streetdata[,1]),1]<-sub("Str.$","Str",streetdata[grep("Str.$",streetdata[,1]),1])
streetdata[grep("str.$",streetdata[,1]),1]<-sub("str.$","str",streetdata[grep("str.$",streetdata[,1]),1])
streetdata[grep("W$",streetdata[,1]),1]<-sub("W$","Weg",streetdata[grep("W$",streetdata[,1]),1])
streetdata[grep("W.$",streetdata[,1]),1]<-sub("W.$","Weg",streetdata[grep("W.$",streetdata[,1]),1])
streetdata[grep("Rg.$",streetdata[,1]),1]<-sub("Rg.$","Ring",streetdata[grep("Rg.$",streetdata[,1]),1])
streetdata[grep("Pl.$",streetdata[,1]),1]<-sub("Pl.$","Platz",streetdata[grep("Pl.$",streetdata[,1]),1])
streetdata[grep("Verl.Jungbuschstr.",streetdata[,1]),1]<-sub("Verl.Jungbuschstr.","Verlängerte Jungbuschstr",streetdata[grep("Verl.Jungbuschstr.",streetdata[,1]),1])
streetdata[grep("Am Meßplatz",streetdata[,1]),1]<-sub("Am Meßplatz","Am Messplatz",streetdata[grep("Am Meßplatz",streetdata[,1]),1])
streetdata[grep("An d. Kammerschleuse",streetdata[,1]),1]<-sub("An d. Kammerschleuse","An der Kammerschleuse",streetdata[grep("An d. Kammerschleuse",streetdata[,1]),1])
streetdata[grep("Bürgerm.-Fuchs-Str",streetdata[,1]),1]<-sub("Bürgerm.-Fuchs-Str","Bürgermeister-Fuchs-Str",streetdata[grep("Bürgerm.-Fuchs-Str",streetdata[,1]),1])
streetdata[grep("Chr.-",streetdata[,1]),1]<-sub("Chr.-","Christian-",streetdata[grep("Chr.-",streetdata[,1]),1])
streetdata[grep("Der Hohe Weg Z.Rhein",streetdata[,1]),1]<-sub("Der Hohe Weg Z.Rhein","Der Hohe Weg Zum Rhein",streetdata[grep("Der Hohe Weg Z.Rhein",streetdata[,1]),1])
streetdata[grep("Dietr.Bonhoeffer",streetdata[,1]),1]<-sub("Dietr.Bonhoeffer","Dietrich-Bonhoeffer",streetdata[grep("Dietr.Bonhoeffer",streetdata[,1]),1])
streetdata[grep("Elis.-V-Thadden",streetdata[,1]),1]<-sub("Elis.-V-Thadden","Elisabeth-Von-Thadden",streetdata[grep("Elis.-V-Thadden",streetdata[,1]),1])
streetdata[grep("Esther-Charl.-Brandes",streetdata[,1]),1]<-sub("Esther-Charl.-Brandes","Esther-Charlotte-Brandes",streetdata[grep("Esther-Charl.-Brandes",streetdata[,1]),1])
streetdata[grep("Friedr.",streetdata[,1]),1]<-sub("Friedr.","Friedrich",streetdata[grep("Friedr.",streetdata[,1]),1])
streetdata[grep("-V-",streetdata[,1]),1]<-sub("-V-","-Von-",streetdata[grep("-V-",streetdata[,1]),1])
streetdata[grep("-V.-",streetdata[,1]),1]<-sub("-V.-","-Von-",streetdata[grep("-V.-",streetdata[,1]),1])
streetdata[grep("E.-Altmann-Gottheiner",streetdata[,1]),1]<-sub("E.-Altmann-Gottheiner","Elisabeth-Altmann-Gottheiner",streetdata[grep("E.-Altmann-Gottheiner",streetdata[,1]),1])
streetdata[grep("Fred-J.-Schoeps-Str",streetdata[,1]),1]<-sub("Fred-J.-Schoeps-Str","Fred-Joachim-Schoeps-Str",streetdata[grep("Fred-J.-Schoeps-Str",streetdata[,1]),1])
streetdata[grep("Gerh.-Hauptmann-Str",streetdata[,1]),1]<-sub("Gerh.-Hauptmann-Str","Gerhart-Hauptmann-Str",streetdata[grep("Gerh.-Hauptmann-Str",streetdata[,1]),1])
streetdata[grep("Herm.-Gropengießer",streetdata[,1]),1]<-sub("Herm.-Gropengießer","Hermann-Gropengießer",streetdata[grep("Herm.-Gropengießer",streetdata[,1]),1])
streetdata[grep("Herm.-Heimerich",streetdata[,1]),1]<-sub("Herm.-Heimerich","Hermann-Heimerich",streetdata[grep("Herm.-Heimerich",streetdata[,1]),1])
streetdata[grep("Herm.-Hesse",streetdata[,1]),1]<-sub("Herm.-Hesse","Hermann-Hesse",streetdata[grep("Herm.-Hesse",streetdata[,1]),1])
streetdata[grep("Herm.-Löns",streetdata[,1]),1]<-sub("Herm.-Löns","Hermann-Löns",streetdata[grep("Herm.-Löns",streetdata[,1]),1])
streetdata[grep("Neischwand.",streetdata[,1]),1]<-sub("Neischwand.","Neischwander",streetdata[grep("Neischwand.",streetdata[,1]),1])
streetdata[grep("Phil.-Brunnemer",streetdata[,1]),1]<-sub("Phil.-Brunnemer","Philipp-Brunnemer",streetdata[grep("Phil.-Brunnemer",streetdata[,1]),1])
streetdata[grep("Reichsk.-Müller",streetdata[,1]),1]<-sub("Reichsk.-Müller","Reichskanzler-Müller",streetdata[grep("Reichsk.-Müller",streetdata[,1]),1])
streetdata[grep("Tauberbischofsh.Str",streetdata[,1]),1]<-sub("Tauberbischofsh.Str","Tauberbischofsheimer Str",streetdata[grep("Tauberbischofsh.Str",streetdata[,1]),1])
streetdata[grep("Verb.-Kanal Li.Ufer",streetdata[,1]),1]<-sub("Verb.-Kanal Li.Ufer","Verbindungskanal Linkes Ufer",streetdata[grep("Verb.-Kanal Li.Ufer",streetdata[,1]),1])
streetdata[grep("Wilh.-Furtwängl.Str",streetdata[,1]),1]<-sub("Wilh.-Furtwängl.Str","Wilhelm-Furtwänglänger-Str",streetdata[grep("Wilh.-Furtwängl.Str",streetdata[,1]),1])
streetdata[grep("Christian-FriedrichSchwan",streetdata[,1]),1]<-sub("Christian-FriedrichSchwan","Christian-Friedrich-Schwan",streetdata[grep("Christian-FriedrichSchwan",streetdata[,1]),1])
streetdata[grep("Friedrichch",streetdata[,1]),1]<-sub("Friedrichch","Friedrich",streetdata[grep("Friedrichch",streetdata[,1]),1])
streetdata[grep("Hanns-M-Schleyer",streetdata[,1]),1]<-sub("Hanns-M-Schleyer","Hanns-Martin-Schleyer",streetdata[grep("Hanns-M-Schleyer",streetdata[,1]),1])
streetdata[grep("Marienwerder Weg",streetdata[,1]),1]<-sub("Marienwerder Weg","Marienwerderweg",streetdata[grep("Marienwerder Weg",streetdata[,1]),1])
streetdata[grep("Seckenheimer Hauptst",streetdata[,1]),1]<-sub("Seckenheimer Hauptst","Seckenheimer Hauptstr",streetdata[grep("Seckenheimer Hauptst",streetdata[,1]),1])
streetdata[grep("Wilhelm-Furtwänglänger-Str",streetdata[,1]),1]<-sub("Wilhelm-Furtwänglänger-Str","Wilhelm-Furtwängler-Str",streetdata[grep("Wilhelm-Furtwänglänger-Str",streetdata[,1]),1])
streetdata<-streetdata[-grep("Gewann auf den Ried",streetdata[,1]),]
streetdata<-streetdata[-grep("Alter Rangierbahnhof",streetdata[,1]),]
streetdata[grep("Straße",streetdata[,1]),1]<-sub("Straße","Str",streetdata[grep("Straße",streetdata[,1]),1])
streetdata[grep("straße",streetdata[,1]),1]<-sub("straße","str",streetdata[grep("straße",streetdata[,1]),1])
streetdata[grep("str$",streetdata[,1]),1]<-sub("str$","straße",streetdata[grep("str$",streetdata[,1]),1])
streetdata[grep("Str$",streetdata[,1]),1]<-sub("Str$","Straße",streetdata[grep("Str$",streetdata[,1]),1])

# loop over Wahlbezirke
# loop over addresses within wahlbezirk
# look up addresses
# combine points to polygon
# combine polygon to SpatialPolygon
streetdata<-streetdata[streetdata$Wahlbezirk=="03111",]

for(w in 1:length(unique(streetdata$Wahlbezirk))) {
  for (i in 1:nrow(streetdata[streetdata$Wahlbezirk==unique(streetdata$Wahlbezirk)[w],])) {
    if (i==1) {
      if ((streetdata[streetdata$Wahlbezirk==unique(streetdata$Wahlbezirk)[w],])[i,"Straße"] %in% blocklist) {
        osm_query<-opq(bbox="Mannheim") %>% 
          add_osm_feature(key = 'addr:place',
                          value = (streetdata[streetdata$Wahlbezirk==unique(streetdata$Wahlbezirk)[w],])[i,"Straße"]) %>% 
          osmdata_sf ()
        if (((streetdata[streetdata$Wahlbezirk==unique(streetdata$Wahlbezirk)[w],])[i,"Straße"])=="A1") {
          osm_query<-c(osm_query,
                       opq(bbox="Mannheim") %>% 
                         add_osm_feature(key = 'name', value =c('Amtsgericht Mannheim',
                                                                'Mensa am Schloss',
                                                                'Schloss Mannheim')) %>%
                         osmdata_sf ()
          )
        }
      } else {
        osm_query<-opq(bbox="Mannheim") %>%
          add_osm_feature(key = 'addr:housenumber',
                          value = seq(from=(streetdata[streetdata$Wahlbezirk==unique(streetdata$Wahlbezirk)[w],])[i,"HNR.von"],
                                      to=(streetdata[streetdata$Wahlbezirk==unique(streetdata$Wahlbezirk)[w],])[i,"HNR.bis"],
                                      by=ifelse((streetdata[streetdata$Wahlbezirk==unique(streetdata$Wahlbezirk)[w],])[i,"Gerade.Ungerade"]=="F",1,2)),
                          value_exact = T) %>%
          add_osm_feature(key = 'addr:street', value = 
                            (streetdata[streetdata$Wahlbezirk==unique(streetdata$Wahlbezirk)[w],])[i,"Straße"]) %>%
          add_osm_feature(key = 'addr:city', value = '!Viernheim') %>%
          add_osm_feature(key = 'addr:city', value = '!Schwetzingen') %>%
          add_osm_feature(key = 'addr:city', value = '!Brühl') %>%
          add_osm_feature(key = 'addr:city', value = '!Ilvesheim') %>%
          add_osm_feature(key = 'addr:city', value = '!Ludwigshafen am Rhein') %>%
          add_osm_feature(key = 'addr:city', value = '!Ludwigshafen') %>%
          add_osm_feature(key = 'addr:city', value = '!Neuhofen') %>%
          add_osm_feature(key = 'addr:city', value = '!Altrip') %>%
          add_osm_feature(key = 'addr:city', value = '!Lampertheim') %>%
          osmdata_sf ()
      }
    } else {
      if ((streetdata[streetdata$Wahlbezirk==unique(streetdata$Wahlbezirk)[w],])[i,"Straße"] %in% blocklist) {
        osm_query<-c(osm_query,opq(bbox="Mannheim") %>% 
                       add_osm_feature(key = 'addr:place',
                                       value = (streetdata[streetdata$Wahlbezirk==unique(streetdata$Wahlbezirk)[w],])[i,"Straße"]) %>% 
                       osmdata_sf ())
        if (((streetdata[streetdata$Wahlbezirk==unique(streetdata$Wahlbezirk)[w],])[i,"Straße"])=="A1") {
          osm_query<-c(osm_query,
                       opq(bbox="Mannheim") %>% 
                         add_osm_feature(key = 'name', value =c('Amtsgericht Mannheim',
                                                                'Mensa am Schloss',
                                                                'Schloss Mannheim')) %>%
                         osmdata_sf ()
          )
        }
      } else {
        osm_query<-c(osm_query,opq(bbox="Mannheim") %>%
                       add_osm_feature(key = 'addr:housenumber',
                                       value = seq(from=(streetdata[streetdata$Wahlbezirk==unique(streetdata$Wahlbezirk)[w],])[i,"HNR.von"],
                                                   to=(streetdata[streetdata$Wahlbezirk==unique(streetdata$Wahlbezirk)[w],])[i,"HNR.bis"],
                                                   by=ifelse((streetdata[streetdata$Wahlbezirk==unique(streetdata$Wahlbezirk)[w],])[i,"Gerade.Ungerade"]=="F",1,2)),
                                       value_exact = T) %>%
                       add_osm_feature(key = 'addr:street', value = (streetdata[streetdata$Wahlbezirk==unique(streetdata$Wahlbezirk)[w],])[i,"Straße"]) %>%
                       add_osm_feature(key = 'addr:city', value = '!Viernheim') %>%
                       add_osm_feature(key = 'addr:city', value = '!Schwetzingen') %>%
                       add_osm_feature(key = 'addr:city', value = '!Brühl') %>%
                       add_osm_feature(key = 'addr:city', value = '!Ilvesheim') %>%
                       add_osm_feature(key = 'addr:city', value = '!Ludwigshafen am Rhein') %>%
                       add_osm_feature(key = 'addr:city', value = '!Ludwigshafen') %>%
                       add_osm_feature(key = 'addr:city', value = '!Neuhofen') %>%
                       add_osm_feature(key = 'addr:city', value = '!Altrip') %>%
                       add_osm_feature(key = 'addr:city', value = '!Lampertheim') %>%
                       add_osm_feature(key = 'addr:city', value = '!Edingen-Neckarhausen') %>%
                       osmdata_sf ())
      }
    }
  }
  # convert points to coordinate object
  osm_query_coord<-coordinates(as_Spatial(osm_query$osm_points))
  osm_query_coord<-(osm_query_coord[in_lu(x=osm_query_coord[,1],
                                         y=osm_query_coord[,2])==-1 &
                                   in_viernheim(x=osm_query_coord[,1],
                                                y=osm_query_coord[,2])==-1,])
  
  if (w==1) {
    # wahlbezirke<-SpatialPolygonsDataFrame(as_Spatial(st_geometry(
    #   concaveman(st_cast(osm_query$osm_polygons,"POINT"),concavity = 2,length_threshold = 0)),IDs=unique(streetdata$Wahlbezirk)[w]),
    #   data=data.frame(ID=unique(streetdata$Wahlbezirk)[w],row.names=unique(streetdata$Wahlbezirk)[w]))
    # wahlbezirke<-SpatialPolygonsDataFrame(as_Spatial(st_geometry(
    #   concaveman(osm_query$osm_points,concavity = 2,length_threshold = 0)),IDs=unique(streetdata$Wahlbezirk)[w]),
    #   data=data.frame(ID=unique(streetdata$Wahlbezirk)[w],row.names=unique(streetdata$Wahlbezirk)[w]))
    wahlbezirke<-SpatialPolygonsDataFrame(as_Spatial(st_geometry(
      concaveman(osm_query_coord,concavity = 2,length_threshold = 0)),IDs=unique(streetdata$Wahlbezirk)[w]),
      data=data.frame(ID=unique(streetdata$Wahlbezirk)[w],row.names=unique(streetdata$Wahlbezirk)[w]))
  } else {
    # wahlbezirke<-rbind(wahlbezirke,SpatialPolygonsDataFrame(as_Spatial(st_geometry(
    #   concaveman(st_cast(osm_query$osm_polygons,"POINT"),concavity = 2,length_threshold = 1)),IDs=unique(streetdata$Wahlbezirk)[w]),
    #   data=data.frame(ID=unique(streetdata$Wahlbezirk)[w],row.names=unique(streetdata$Wahlbezirk)[w])))
    # wahlbezirke<-rbind(wahlbezirke,SpatialPolygonsDataFrame(as_Spatial(st_geometry(
    #   concaveman(osm_query$osm_points,concavity = 2,length_threshold = 1)),IDs=unique(streetdata$Wahlbezirk)[w]),
    #   data=data.frame(ID=unique(streetdata$Wahlbezirk)[w],row.names=unique(streetdata$Wahlbezirk)[w])))
    wahlbezirke<-rbind(wahlbezirke,SpatialPolygonsDataFrame(as_Spatial(st_geometry(
      concaveman(osm_query_coord,concavity = 2,length_threshold = 1)),IDs=unique(streetdata$Wahlbezirk)[w]),
      data=data.frame(ID=unique(streetdata$Wahlbezirk)[w],row.names=unique(streetdata$Wahlbezirk)[w])))
  }
}
 # ding <- as_Spatial(st_geometry(
 #   osm_query$osm_points),IDs=unique(streetdata$Wahlbezirk)["03111"])
 # wahlbezirke<-st_as_sf(ding)
 # ggmap(get_map("Viernheim",source="osm")) +
 #   geom_sf(aes(alpha=0),
 #           data=wahlbezirke,inherit.aes=F,show.legend = F,shape=3)
# 
# #getbb("Viernheim")[[3]]
# osm_query$osm_points<-osm_query$osm_points[in_lu(osm_query$osm_points$geometry[1])]
# osm_query$osm_points
# osm_query_coord<-coordinates(as_Spatial(osm_query$osm_points))
# osm_query_coord<-osm_query_coord[in_lu(osm_query_coord[,1])==-1 & in_viernheim(osm_query_coord[,1])==-1]

wahlbezirke<-st_as_sf(wahlbezirke)
wahlbezirke_points <- sf::st_point_on_surface(wahlbezirke)
wahlbezirke_coords <- as.data.frame(sf::st_coordinates(wahlbezirke_points))
wahlbezirke_coords$ID <- wahlbezirke$ID
ggmap(get_map(getbb("Mannheim"),source="osm")) +
  geom_sf(aes(alpha=0),
          data=wahlbezirke,inherit.aes=F,show.legend = F)+
  geom_text(data = wahlbezirke_coords, aes(X, Y, label = ID), colour = "red",show.legend=F,size=2)
#getbb("Neuhofen")
#getbb("Mannheim")[[1]]

# 
# wahlbezirke<-st_as_sf(wahlbezirke)
# wahlbezirke_points <- sf::st_point_on_surface(wahlbezirke)
# wahlbezirke_coords <- as.data.frame(sf::st_coordinates(wahlbezirke_points))
# wahlbezirke_coords$ID <- wahlbezirke$ID
# ggmap(get_map(getbb("Mannheim"),source="osm")) +
#   geom_sf(aes(alpha=0),
#           data=wahlbezirke[wahlbezirke$ID=="02022",],inherit.aes=F,show.legend = F)+
#   geom_text(data = wahlbezirke_coords[wahlbezirke$ID=="02022",], aes(X, Y, label = ID), colour = "red",show.legend=F,size=2)

#str(getbb("Mannheim"))
#getbb("Mannheim")
#c(8.445,49.48,8.476,49.500)
#streetdata<-streetdata[1:167,]
#osm_query$overpass_call

