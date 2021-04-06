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

# import street data from excel file
streetdata<-as_tibble(read.xlsx2(file="StrVerz_LTW21.xlsx",
                                 sheetName="str_ltw21"),
                                 colIndex=c(1,2,3,4,5,6,7),
                                 as.data.frame=T,
                                 header=T)

# #set_overpass_url("https://lz4.overpass-api.de/api/interpreter")
# q<- opq(bbox= c(8.482065251929425,49.494868214443855, 8.48679889194244, 49.497151223827544)) %>%
#   osmdata_sp ()
# #sp::plot(q$osm_points[which(!is.na(q$osm_points$`addr:street`)),])
# ding<-as.data.frame(iconv(q$osm_points$`addr:housenumber`, from="UTF-8", to="UTF-8"))
# ding$street<-as.data.frame(iconv(q$osm_points$`addr:street`, from="UTF-8", to="UTF-8"))
# ding<-ding[which(!is.na(q$osm_points$`addr:street`)),]

q<- opq(bbox = "Mannheim") %>%
  #add_osm_feature(key = 'building', value = 'yes') %>%
  add_osm_feature(key = 'addr:place', value = 'B',value_exact = F) %>%
  add_osm_feature(key = 'addr:city', value = 'Mannheim') %>%
  osmdata_sp ()
plot(q$osm_polygons)
ding<-as.data.frame(iconv(q$osm_polygons$`addr:street`,from="UTF-8",to="UTF-8"))
ding$street<-as.data.frame(iconv(q$osm_polygons$`addr:street`,from="UTF-8",to="UTF-8"))
ding$place<-as.data.frame(iconv(q$osm_polygons$`addr:place`,from="UTF-8",to="UTF-8"))
ding$housenumber<-as.data.frame(iconv(q$osm_polygons$`addr:housenumber`,from="UTF-8",to="UTF-8"))
ding$city<-as.data.frame(iconv(q$osm_polygons$`addr:city`,from="UTF-8",to="UTF-8"))
ding<-ding[,c(2,3,4,5)]
ding<-ding[subset(!(is.na(ding[,1])&is.na(ding[,2]))&!is.na(ding[,3])&ding[,4]=="Mannheim"),,]
ding[which(is.na(ding[,1])),1]<-ding[which(is.na(ding[,1])),2]
ding<-ding[,c(1,3,4)]
  #add_osm_feature(key = 'addr:street', value = 'Uhlandstr') %>%
ding<-ding[order(ding[,1],ding[,2]),]

q <- opq(bbox = 'greater london uk') %>%
  add_osm_feature(key = 'highway', value = 'motorway') %>%
  osmdata_sp ()
plot(q$osm_points)
q
opq_string(q)
available_features()

streetdata[1,]$Straße
for (i in unique(streetdata$Wahlbezirk)) {
  for (j in unique(subset(streetdata,Wahlbezirk==i)$Straße)) {
    seq(from=Hausnummer,to=Hausnummer,by=)
  }
}

# reshape data into long format
# start and end of street as separate rows
streetdata<-pivot_longer(streetdata,cols=c(2,5),
                         names_to="type",
                         values_to="number",
                         values_transform = list(number=as.character))

# drop type
streetdata<-streetdata %>% select(!"type")

# repair block addresses
streetdata<-as.data.frame(streetdata)
streetdata[substr(streetdata[,1],1,2) %in% c("A ","B ","C ","D ","E ","F ","G ","H ","I ","J ",
                                         "K ","L ","M ","N ","O ","P ","Q ","R ","S ","T ","U "),1]<-
  sub(" ","",streetdata[substr(streetdata[,1],1,2) %in% c("A ","B ","C ","D ","E ","F ","G ","H ","I ","J ",
                                                      "K ","L ","M ","N ","O ","P ","Q ","R ","S ","T ","U "),1])

# address house number to street names
streetdata[substr(streetdata[,1],1,1) %in% c("A","B","C","D","E","F","G","H","I","J",
                                         "K","L","M","N","O","P","Q","R","S","T","U") &
           substr(streetdata[,1],2,2) %in% c(1,2,3,4,5,6,7,8,9),1]<-
  paste0(streetdata[substr(streetdata[,1],1,1) %in% c("A","B","C","D","E","F","G","H","I","J",
                                                  "K","L","M","N","O","P","Q","R","S","T","U") &
                    substr(streetdata[,1],2,2) %in% c(1,2,3,4,5,6,7,8,9),1],",",
         streetdata[substr(streetdata[,1],1,1) %in% c("A","B","C","D","E","F","G","H","I","J",
                                                  "K","L","M","N","O","P","Q","R","S","T","U") &
                    substr(streetdata[,1],2,2) %in% c(1,2,3,4,5,6,7,8,9),3])

streetdata[!(substr(streetdata[,1],1,1) %in% c("A","B","C","D","E","F","G","H","I","J",
                                           "K","L","M","N","O","P","Q","R","S","T","U") &
             substr(streetdata[,1],2,2) %in% c(1,2,3,4,5,6,7,8,9)),1]<-
  paste0(streetdata[!(substr(streetdata[,1],1,1) %in% c("A","B","C","D","E","F","G","H","I","J",
                                                    "K","L","M","N","O","P","Q","R","S","T","U") &
                      substr(streetdata[,1],2,2) %in% c(1,2,3,4,5,6,7,8,9)),1]," ",
         streetdata[!(substr(streetdata[,1],1,1) %in% c("A","B","C","D","E","F","G","H","I","J",
                                                    "K","L","M","N","O","P","Q","R","S","T","U") &
                      substr(streetdata[,1],2,2) %in% c(1,2,3,4,5,6,7,8,9)),3])

# drop house numbers
streetdata<-streetdata %>% select(!"number")

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
streetdata[grep("Alter Rangierbahnhof 5",streetdata[,1]),1]<-sub("Alter Rangierbahnhof 5","49.474377122229455, 8.47689138464993",streetdata[grep("Alter Rangierbahnhof 5",streetdata[,1]),1])
streetdata[grep("Christian-FriedrichSchwan",streetdata[,1]),1]<-sub("Christian-FriedrichSchwan","Christian-Friedrich-Schwan",streetdata[grep("Christian-FriedrichSchwan",streetdata[,1]),1])
streetdata[grep("Friedrichch",streetdata[,1]),1]<-sub("Friedrichch","Friedrich",streetdata[grep("Friedrichch",streetdata[,1]),1])
streetdata[grep("Gewann auf den Ried 6",streetdata[,1]),1]<-sub("Gewann auf den Ried 6","49.50161936443979, 8.537328771035872",streetdata[grep("Gewann auf den Ried 6",streetdata[,1]),1])
streetdata[grep("Hanns-M-Schleyer",streetdata[,1]),1]<-sub("Hanns-M-Schleyer","Hanns-Martin-Schleyer",streetdata[grep("Hanns-M-Schleyer",streetdata[,1]),1])
streetdata[grep("Marienwerder Weg",streetdata[,1]),1]<-sub("Marienwerder Weg","Marienwerderweg",streetdata[grep("Marienwerder Weg",streetdata[,1]),1])
streetdata[grep("Seckenheimer Hauptst",streetdata[,1]),1]<-sub("Seckenheimer Hauptst","Seckenheimer Hauptstraße",streetdata[grep("Seckenheimer Hauptst",streetdata[,1]),1])
streetdata[grep("Wilhelm-Furtwänglänger-Straße",streetdata[,1]),1]<-sub("Wilhelm-Furtwänglänger-Straße","Wilhelm-Furtwängler-Straße",streetdata[grep("Wilhelm-Furtwänglänger-Straße",streetdata[,1]),1])

# Paste Mannheim to each address
streetdata[,1]<-paste0("Mannheim, ",streetdata[,1])

# get geocodes for adresses from OSM
streetdata[,3:4]<-geocode_OSM(streetdata[,1])[,2:3]

#repair a few cases
streetdata[grep("Mannheim, A1,4",streetdata[,1]),3]<-49.48556589213508
streetdata[grep("Mannheim, A1,4",streetdata[,1]),4]<-8.462409998320341
streetdata[grep("Mannheim, G4,4",streetdata[,1]),3]<-49.49057027039347
streetdata[grep("Mannheim, G4,4",streetdata[,1]),4]<-8.465042388728204
streetdata[grep("Mannheim, I6,4",streetdata[,1]),3]<-49.49272067890962
streetdata[grep("Mannheim, I6,4",streetdata[,1]),4]<-8.464947911811487
streetdata[grep("Mannheim, O3,4",streetdata[,1]),3]<-49.48675140129906
streetdata[grep("Mannheim, O3,4",streetdata[,1]),4]<-8.468529727345842

# save data set to save search time
save(streetdata,file="streetdata.Rdata")
