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

# Get shape of Mannheim
wfs<-"https://www.gis-mannheim.de/mannheim/mod_ogc/wfs_getmap.php?mapfile=gemark_grenze&service=WFS&Request=GetCapabilities"
mannheim_shape <- st_read(wfs)
mannheim_shape<-st_transform(mannheim_shape, crs = 4326)
mannheim_shape<-as_Spatial(mannheim_shape$msGeometry)
mannheim_shape<-mannheim_shape@polygons[[1]]@Polygons[[1]]@coords

# List of city blocks in inner city for shorter code later on
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
  # convert osm spatial object to coordinates
  #osm_query_coord<-coordinates(as_Spatial(st_cast(osm_query$osm_polygons,"POINT")))
  osm_query_coord<-coordinates(as_Spatial(osm_query$osm_points))
  # sort out coordinates outside of Mannheim (shape downloaded above)
  osm_query_coord<-osm_query_coord[point.in.polygon(point.x=osm_query_coord[,1],
                                                    point.y=osm_query_coord[,2],
                                                    pol.x=mannheim_shape[,1],pol.y=mannheim_shape[,2])==1,]
  # convert to Spatial sf object (ready for concaveman)
  osm_query_coord<-st_as_sf(SpatialPoints(osm_query_coord,proj4string = CRS("EPSG:4326")))
  # convert to Spatial Polygons Data Frame
  if (w==1) {
    wahlbezirke<-SpatialPolygonsDataFrame(as_Spatial(st_geometry(
      concaveman(osm_query_coord,concavity = 2,length_threshold = 0)),IDs=unique(streetdata$Wahlbezirk)[w]),
      data=data.frame(ID=unique(streetdata$Wahlbezirk)[w],row.names=unique(streetdata$Wahlbezirk)[w]))
  } else {
    wahlbezirke<-rbind(wahlbezirke,SpatialPolygonsDataFrame(as_Spatial(st_geometry(
      concaveman(osm_query_coord,concavity = 2,length_threshold = 0)),IDs=unique(streetdata$Wahlbezirk)[w]),
      data=data.frame(ID=unique(streetdata$Wahlbezirk)[w],row.names=unique(streetdata$Wahlbezirk)[w])))
  }
}
# plot map
wahlbezirke<-st_as_sf(wahlbezirke)
wahlbezirke_points <- sf::st_point_on_surface(wahlbezirke)
wahlbezirke_coords <- as.data.frame(sf::st_coordinates(wahlbezirke_points))
wahlbezirke_coords$ID <- wahlbezirke$ID
ggmap(get_map(getbb("Mannheim"),source="osm")) +
  geom_sf(aes(alpha=0),
          data=wahlbezirke,inherit.aes=F,show.legend = F)+
  geom_text(data = wahlbezirke_coords, aes(X, Y, label = ID), colour = "red",show.legend=F,size=3)
