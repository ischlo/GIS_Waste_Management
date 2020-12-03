#### PACKS ####
install.packages(c("sf", "tmap", "tmaptools", "RSQLite", "tidyverse"), repos = "https://www.stats.bris.ac.uk/R/")

#sf: simple features, standard way to encode spatial vector data
#tmap: layer-based and easy approach to make thematic maps
#tmaptools: set of tools for reading and processing spatial data
#RSQLite: embeds the SQLite database engine in R

install.packages("rgdal") # FOR MAC

install.packages("raster") # TO MANIPULATE RASTERS

install.packages("shinyjs") #FOR COOL COLORED MAPS

install.packages("leaflet.extras")
install.packages('reshape')

install.packages("spatstat")

#### LOAD LIBRARIES ####

library(rgdal)
library(sf) #TO READ SHAPEFILES 
library(sp)
library(tidyverse) #TO MANIPULATE CSV FILES 
library(tmap) #TO PLOT MAPS
#library(tmaptools) 
library(readr)
library(RSQLite)  #TO CONNECT CSV DATA TO GEOPACKAGE
library(raster)
library(tibble)
library(leaflet)
library(leaflet.extras)
library(maptools)
library(rgeos)
library(rgdal)
library(reshape)
library(dplyr)
library(spatstat)

#### LOAD DATA ####

ukpost <- st_read("Distribution/Sectors.shp")

ukregion <- st_read("Regions/Regions_December_2018_EN_BFC.shp") %>% 
  st_transform(CRS("+proj=longlat +datum=WGS84"))

londonBoroughs <- st_read("LondonMap/London_Borough_Excluding_MHW.shp")

ukwards <- st_read("WARDS2/Local_Authority_Districts_December_2017_Generalised_Clipped_Boundaries_in_United_Kingdom_WGS84.shp")

uklau <- st_read("LAULev1Jan18UK/Local_Administrative_Units_Level_1_January_2018_Full_Extent_Boundaries_in_United_Kingdom.shp")

wasteusela <- read_csv2("LA_and_Regional_Spreadsheet_1819.csv",skip = 3, col_names = TRUE)

regionpopulation <- read_csv2("regionpopulation.csv",col_names= TRUE) #in thousands of people
#niwaste <- read_csv2("lac-municipal-waste-timeseries-csv.csv",col_names= TRUE)
regionpopulation<- data.frame(regionpopulation)
regionpopulation <- regionpopulation[, c("AREA","AGE.GROUP","X2018")] %>% 
  filter(`AGE.GROUP` %in% "All ages")

regionpop <- regionpopulation[, c("AREA","X2018")]
regionpop

Removed <- read_csv2("2018_WDI_Removed.csv",col_names = TRUE, skip = 8)
# nomscolsRemoved <- colnames(removed)


#### DATA CLEANING and investigation ####

removed <- filter(Removed, `Basic Waste Cat` %in% "Hhold/Ind/Com") %>%
  filter(`EWC Chapter` %in% wastecat)
removed

# removed <- removed[,c("Facility RPA","Facility District","Site Name","Operator","Easting","Northing",
#                      "Facility Type","EWC Chapter", "EWC Waste Desc","Recorded Destination","Destination Region","Fate","RAD","Tonnes Removed")]
removed_columns <- colnames(removed)

siteCat <- unique(removed$`Site Category`)

view(removed$Fate[which(removed$`Facility WPA` != removed$`Destination WPA`)])

unique(removed$Fate)

plot(ukpost$name)

view(unique(removed$`Destination Region`))

view(sum(removed$`Tonnes Removed`[which(removed$`Destination Region` %nin% Regions)]))

view(unique(removed[which(removed$`Destination Region` %nin% Regions),]))

#### ANALYSE DATA ####

view(removed[sample(c(1:10000), size = 200),])

#### WASTE MOVEMENT MATRIX ####

# The rows are for origin regions and columns are for destinations, 
# thus matrix element i,j is the amount of waste that was removed from i to j
WasteremovedMatrix <- matrix(0, nrow = 9,ncol = 9, byrow = TRUE, dimnames = list(Regions,Regions))

for (i in c(1:9)) {
  for (j in c(1:9)) {
    wasteremoved <-  filter(removed, `Facility RPA` == Regions[i] & `Destination Region` == Regions[j]) %>% summarize(`wasteremoved`= sum(`Tonnes Removed`))
    WasteremovedMatrix[i,j] <-  wasteremoved$wasteremoved
  }
}

view(WasteremovedMatrix)

matrix.heatmap(WasteremovedMatrix)

#### Removed from london ####
#fromLondonRemoved$`Destination Region`[which(`Destination Region` == Yorks & Humber)] <- "Yorkshire and The Humber"

fromLondonRemoved <- group_by(fromLondonRemoved,`Destination Region`) %>% 
  summarise("Tonnes Removed" = sum(`Tonnes Removed`)) 

fromLondonRemoved <- merge(ukregion,fromLondonRemoved, by.x = "rgn18nm", by.y = "Destination Region" )

tmap_mode("view")

tm_shape(fromLondonRemoved) + tm_polygons("Tonnes Removed",alpha = 0.7)

tm_shape(ukpost) + tm_polygons("name")


#### Facilities ananlysis ####

view(unique(removed$`Recorded Destination`))


#### ####
removedTransf <- filter(removed, `Fate` %in% c("Landfill")
                        & `Facility RPA` != `Destination Region`
                        & `Facility RPA` %in% "London") %>% 
  data.frame()

removedTransf <- group_by(removedTransf,`Facility.District`,`Easting`,`Northing`) %>% 
  summarise("Tonnes Removed" = sum(`Tonnes.Removed`))

removedTransflonglat <- SpatialPoints(receivedTransf[,c("Easting","Northing")],
                                      proj4string=CRS("+proj=tmerc
                                                       +lat_0=49 
                                                       +lon_0=-2 
                                                       +k=0.9996012717 
                                                       +x_0=400000 
                                                       +y_0=-100000 
                                                       +ellps=airy 
                                                       +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 
                                                       +units=m 
                                                       +no_defs ")) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84" ))




#### GRAPH ANALYSIS ####

gremoved <- graph_from_adjacency_matrix(WasteremovedMatrix,mode = "directed", weighted = T, diag = T, add.colnames = T)

gremoved <-gremoved %>% set_vertex_attr("name", value = Regions)

l <- coords <- layout_(gremoved, as_star())

E(gremoved)$width <- E(gremoved)$weight * 20/ max(E(gremoved)$weight)

E(gremoved)$arrow.width <- E(gremoved)$weight / max(E(gremoved)$weight)

E(gremoved)$curved <- 0.2

plot(gremoved, layout = l)

#### ####