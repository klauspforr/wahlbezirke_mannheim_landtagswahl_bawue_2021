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

# look up addresses individually for each Wahlbezirk by inspecting adresses in Excel sheet
# 01111
osm_query<-c(
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:place', value = c('A1','A2','A3','A4','A5',
                                                  'B1','B2','B3','B4','B5','B6','B7',
                                                  'C1','C2','C3','C4','C6')) %>%
    osmdata_sf (),
    opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'name', value =c('Amtsgericht Mannheim','Mensa am Schloss','Schloss Mannheim')) %>%
    osmdata_sf ()
)
wahlbezirke<-SpatialPolygonsDataFrame(as_Spatial(st_geometry(
  concaveman(osm_query$osm_points,concavity = 2)),IDs="01111"),
  data=data.frame(ID="01111",row.names="01111"))

# 01112
osm_query<-c(
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:place', value = c('C7','C8',
                                                  'D1','D2','D3','D4','D5','D6','D7',
                                                  'E1','E2','E3','E4','E5','E6','E7',
                                                  'F1','F2','F3','F4','F5','F6')) %>%
    osmdata_sf ()
)
wahlbezirke<-rbind(wahlbezirke,
                   SpatialPolygonsDataFrame(as_Spatial(st_geometry(
                     concaveman(osm_query$osm_points,concavity = 2)),IDs="01112"),
                     data=data.frame(ID="01112",row.names="01112")))

# 01121
osm_query<-c(
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:place', value = c('F7',
                                                  'G4','G5','G6','G7',
                                                  'H4','H5','H6','H7',
                                                  'I3','I4','I5','I6','I7',
                                                  'K3','K4','K5')) %>%
    osmdata_sf (),
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'name', value = c('K6','K7')) %>%
    osmdata_sf ()
)
wahlbezirke<-rbind(wahlbezirke,
                   SpatialPolygonsDataFrame(as_Spatial(st_geometry(
                     concaveman(osm_query$osm_points,concavity = 2)),IDs="01121"),
                     data=data.frame(ID="01121",row.names="01121")))

# 01122
osm_query<-c(
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:place', value = c('G2','G3',
                                                  'H1','H2','H3',
                                                  'S1','S2','S3','S4','S6',
                                                  'T4','T5','T6')) %>%
    osmdata_sf ()
)
wahlbezirke<-rbind(wahlbezirke,
                   SpatialPolygonsDataFrame(as_Spatial(st_geometry(
                     concaveman(osm_query$osm_points,concavity = 2)),IDs="01122"),
                     data=data.frame(ID="01122",row.names="01122")))

# 01123
osm_query<-c(
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:place', value = c('I1','I2',
                                                  'K1','K2',
                                                  'T1','T2','T3',
                                                  'U1','U2','U3','U4','U5','U6')) %>%
    osmdata_sf ()
)
wahlbezirke<-rbind(wahlbezirke,
                   SpatialPolygonsDataFrame(as_Spatial(st_geometry(
                     concaveman(st_cast(osm_query$osm_polygons,"POINT"),concavity = 2)),IDs="01123"),
                     data=data.frame(ID="01123",row.names="01123")))

# 01131
osm_query<-c(
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:place', value = c('L1','L2','L3','L4','L5','L6','L7',
                                                  'L8','L9','L10','L11','L12','L13','L14','L15',
                                                  'M1','M2','M3','M3a','M4','M5','M6','M7',
                                                  'N4','N5','N6','N7')) %>%
    osmdata_sf (),
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:street', value = 'Schlossgartenstraße') %>%
    add_osm_feature(key = 'addr:housenumber', value = 1) %>%
    osmdata_sf (),
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:street', value = 'Willy-Brandt-Platz') %>%
    add_osm_feature(key = 'addr:housenumber', value = 1) %>%
    osmdata_sf ()
)
wahlbezirke<-rbind(wahlbezirke,
                   SpatialPolygonsDataFrame(as_Spatial(st_geometry(
                     concaveman(osm_query$osm_points,concavity = 2)),IDs="01131"),
                     data=data.frame(ID="01131",row.names="01131")))
# 01132
osm_query<-c(
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:place', value = c('N2','N3',
                                                  'O3','O4','O5','O6','O7',
                                                  'P1','P2','P3','P4','P5','P6','P7',
                                                  'Q1','Q2','Q3','Q4','Q5','Q6','Q7',
                                                  'R1','R2','R3','R4','R5','R6','R7')) %>%
    osmdata_sf ()
)
wahlbezirke<-rbind(wahlbezirke,
                   SpatialPolygonsDataFrame(as_Spatial(st_geometry(
                     concaveman(osm_query$osm_points,concavity = 2)),IDs="01132"),
                     data=data.frame(ID="01132",row.names="01132")))

# 01241
osm_query<-c(
  opq(bbox="Mannheim") %>%
  add_osm_feature(key = 'addr:street', value = c('Akademiestr','Binnenhafenstr',
                                                 'Fruchtbahnhofstr','Güterhallenstr',
                                                 'Kirchenstr',
                                                 'Landzungenstr','Ludwigsbadstr',
                                                 'Neckarspitze','Parkring',
                                                 'Regattastr','Rheinkaistr','Rheinmühlenstr',
                                                 'Rheinstr','Rheinvorlandstr','Schleusenweg',
                                                 'Verbindungs-Kanal Linkes Ufer','Verlängerte Jungbuschstr',
                                                 'Werfthallenstr'),
                  value_exact = F) %>%
    add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
    osmdata_sf (),
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:street', value = 'Hafenstr',value_exact = F) %>%
    add_osm_feature(key = 'addr:housenumber', value = c(seq(from=7,to=45,by=2),
                                                        seq(from=28,to=50,by=2)),value_exact = T) %>%
    add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
    osmdata_sf (),
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:street', value = 'Jungbuschstr',value_exact = F) %>%
    add_osm_feature(key = 'addr:housenumber', value = c(seq(from=3,to=33,by=2)),value_exact = T) %>%
    add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
    osmdata_sf (),
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:street', value = 'Luisenring',value_exact = F) %>%
    add_osm_feature(key = 'addr:housenumber', value = c(seq(from=14,to=24,by=1)),value_exact = T) %>%
    add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
    osmdata_sf (),
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:street', value = 'Neckarvorlandstr',value_exact = F) %>%
    add_osm_feature(key = 'addr:housenumber', value = seq(from=71,to=99,by=2),value_exact = T) %>%
    add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
    osmdata_sf ()
)
#'Hafenstr','Jungbuschstr','Luisenring','Neckarvorlandstr',

wahlbezirke<-rbind(wahlbezirke,
                   SpatialPolygonsDataFrame(as_Spatial(st_geometry(
                     concaveman(osm_query$osm_points,concavity = 2)),IDs="01241"),
                     data=data.frame(ID="01241",row.names="01241")))

# 01242
osm_query<-c(
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:street', value = c('Am Salzkai','Beilstr','Böckstr','Dalbergstr',
                                                   'Holzstr',
                                                   'Schanzenstr','Seilerstr','Werftstr'),
                    value_exact = F) %>%
    add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
    osmdata_sf (),
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:street', value = 'Hafenstr',value_exact = F) %>%
    add_osm_feature(key = 'addr:housenumber', value = seq(from=58,to=90,by=2),value_exact = T) %>%
    add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
    osmdata_sf (),
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:street', value = 'Jungbuschstr',value_exact = F) %>%
    add_osm_feature(key = 'addr:housenumber', value = seq(from=4,to=36,by=2),value_exact = T) %>%
    add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
    osmdata_sf (),
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:street', value = 'Luisenring',value_exact = F) %>%
    add_osm_feature(key = 'addr:housenumber', value = seq(from=25,to=47,by=1),value_exact = T) %>%
    add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
    osmdata_sf (),
  opq(bbox="Mannheim") %>%
    add_osm_feature(key = 'addr:street', value = 'Neckarvorlandstr',value_exact = F) %>%
    add_osm_feature(key = 'addr:housenumber', value = c(seq(from=17,to=21,by=2),
                                                        seq(from=46,to=48,by=2)),value_exact = T) %>%
    add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
    osmdata_sf ()
  )
wahlbezirke<-rbind(wahlbezirke,
                   SpatialPolygonsDataFrame(as_Spatial(st_geometry(
                     concaveman(osm_query$osm_points,concavity = 2)),IDs="01242"),
                     data=data.frame(ID="01242",row.names="01242")))



# plot wahlbezirke over mannheim map with labels
wahlbezirke<-st_as_sf(wahlbezirke)
wahlbezirke_points <- sf::st_point_on_surface(wahlbezirke)
wahlbezirke_coords <- as.data.frame(sf::st_coordinates(wahlbezirke_points))
wahlbezirke_coords$ID <- wahlbezirke$ID
ggmap(get_map(c(8.438,49.47,8.478,49.510),source="osm")) +
  geom_sf(aes(alpha=0),
          data=wahlbezirke,inherit.aes=F,show.legend = F)+
  geom_text(data = wahlbezirke_coords, aes(X, Y, label = ID), colour = "red")

#str(getbb("Mannheim"))
#getbb("Mannheim")
#c(8.445,49.48,8.476,49.500)

