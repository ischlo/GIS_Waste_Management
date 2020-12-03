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

install.packages("matrixcalc")
install.packages("plsgenomics")

#### LOAD LIBRARIES ####

library(rgdal)
library(sf) #TO READ SHAPEFILES 
library(sp)
library(tidyverse) #TO MANIPULATE CSV FILES 
library(tmap) #TO PLOT MAPS
library(tmaptools) 
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
library(matrixcalc)
library(methods)
library(igraph)
library(plsgenomics)

#### Definitions ####

`%nin%` = Negate(`%in%`)


#### LOAD DATA ####

ukcountry <- st_read("ukpoly/Uk_poly.shp") %>% st_transform(CRS("+proj=longlat +datum=WGS84"))

plot(ukcountry)

ukregion <- st_read("Regions/Regions_December_2018_EN_BFC.shp") %>% 
  st_transform(CRS("+proj=longlat +datum=WGS84"))

londonBoroughs <- st_read("LondonMap/London_Borough_Excluding_MHW.shp")

ukwards <- st_read("England_wa_2011/england_wa_2011.shp")

#ukwardstest <- st_read("WARDS2/Local_Authority_Districts_December_2017_Generalised_Clipped_Boundaries_in_United_Kingdom_WGS84.shp")

uklau <- st_read("LAULev1Jan18UK/Local_Administrative_Units_Level_1_January_2018_Full_Extent_Boundaries_in_United_Kingdom.shp")

wasteusela <- read_csv2("LA_and_Regional_Spreadsheet_1819.csv",skip = 3, col_names = TRUE)

ukpost <- st_read("Distribution/Sectors.shp")

wardspopulation <- read_csv2("wardPopulation2017.csv",skip = 4, col_names = TRUE)

postcodetola <- read_csv2("postcodetola.csv",skip = 0, col_names = TRUE)

postcodePop <- read_csv2("postcodeSector.csv",skip = 0, col_names = TRUE)

regionpopulation <- read_csv2("regionpopulation.csv",col_names= TRUE) %>% data.frame() #in thousands of people
regionpopulation <- regionpopulation[, c("AREA","AGE.GROUP","X2018")] %>% 
  filter(`AGE.GROUP` %in% "All ages")

regionpop <- regionpopulation[, c("AREA","X2018")]
regionpop$X2018 <-  regionpop$X2018 %>% str_replace_all(. , "[ ]", "") %>% str_replace_all(.,"[,]","." ) %>% as.numeric()

# WDI DATA SET TREATMENT 

Received <- read_csv2("2018_WDI_Received.csv", col_names = TRUE, skip = 8)

unique(Received$`Facility Type`)

head(Received)
# nomscolsReceived <- colnames(Received)
# nomscolsReceived
# unique(Received$`EWC Sub Chapter`)


#### WASTE CATEGORy ####
#

#Selecting the category of waste
wastecat <- unique(Received$`EWC Chapter`)
wastecat <- wastecat[c(10)]
wastecat

wastedesc <- unique(Received$`EWC Waste Desc`)
wastedesc   # we take "20 - MUNICIPAL WASTES"


unique(Received$`EWC Chapter`)

#ONLY KEEP WASTE FROM HOUSEHOLDS
#FILTER ONLY household and municipal waste
received <- filter(Received, `Basic Waste Cat` %in% "Hhold/Ind/Com") %>%
  filter(`EWC Chapter` %in% wastecat)
received

received_columns <- colnames(received)

view(received_columns)

#### DATA STUDY ####

Regions <- unique(Sites$`Facility RPA`)

england <- ukcountry[which(ukcountry$name == "England"),] %>% st_transform(.,27700) %>% as.owin()

class(england)

view(received[sample(c(1:10000), size = 200),]) # inspect data set

view(ukwards)

tmap_mode("view")

tm_shape(ukregion) + tm_polygons("st_areasha",id = "rgn18nm" ,alpha = 0.5)

tm_shape(ukwards) + tm_polygons()



# The total waste received from within England
view(sum(received$`Tonnes Received`[which(received$`Origin Region` %in% Regions)]))

#WASTE facilities that accpet more that 10000 TONNES
receivedBig <- group_by(received, `Site Name`, `Easting`, `Northing`) %>% 
  summarize( `Site Capacity` = sum(`Tonnes Received`)) %>% 
    filter(`Site Capacity` > 10000)

unique(received$Fate)

# MATRIX

WastereceivedMatrix <- matrix(0, nrow = 9,ncol = 9, byrow = TRUE, dimnames = list(Regions,Regions))

# the rows are for destination and the columns are for origins.
# Thus matrix element i,j is the amount of waste received to region i from region j

for (i in c(1:9)) {
  for (j in c(1:9)) {
    wastereceived <-  filter(received, `Facility RPA` == Regions[i] & `Origin Region` == Regions[j] & `Fate` != "") %>% 
      summarize(`wastereceived`= sum(`Tonnes Received`)) %>% round(., digits = 1)
    WastereceivedMatrix[i,j] <-  wastereceived$wastereceived
  }
}

matrix.heatmap(log(WastereceivedMatrix))

matrix.trace(WastereceivedMatrix)/sum(WastereceivedMatrix)

sum(WastereceivedMatrix)/23e6

#
#### SITES ANALYSIS ####

## Site specific colnames : "Site Name" "SitePC" "Facility Sub Region" "Operator" 
## "Facility WPA" "Permit Type" "Facility Type" "Site Category" "Permit" "Easting" "Northing"

Sites <- group_by(received,`Facility RPA`, `Site Name`, `SitePC`, `Operator`, `Site Category`,`Fate`, `Easting`, `Northing`) %>% 
  summarize(`total treated` = sum(`Tonnes Received`))

SitesCor <- Sites %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) %>%
  st_coordinates() %>% as.data.frame()

# Sites <- SpatialPointsDataFrame(Sites[,c("Easting","Northing")],
#                                 proj4string=CRS("+proj=tmerc
#                                               +lat_0=49
#                                               +lon_0=-2
#                                               +k=0.9996012717
#                                               +x_0=400000
#                                               +y_0=-100000
#                                               +ellps=airy
#                                               +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489
#                                               +units=m
#                                               +no_defs "), Sites) %>% spTransform(CRS("+proj=longlat +datum=WGS84" ))

Sites$X <- SitesCor$X

Sites$Y <- SitesCor$Y

# colnames to export to latex doc 
colnames(Sites) %>% as.data.frame() %>% view()

Sites$`Facility RPA` <- str_replace(Sites$`Facility RPA`, pattern = coll("Yorks & Humber"), replacement = "Yorkshire and The Humber")

unique(Sites$`Facility RPA`)


ukcountry <-  ukcountry %>% st_transform(.,crs = 27700)

ukcountry

england <- ukcountry[which(ukcountry$name=="England"),] %>% st_transform(.,27700)

england

tmap_mode("view")

tm_shape(england) + tm_polygons() +
  tm_shape(Sites) + tm_dots()

#londonBbox <-  c(-0.6564,51.2146,0.4271,51.7347)

# tm_shape(londonBoroughs) + tm_polygons(col = "white",
#                                  border.col = "black",
#                                  lwd = .5 ) +
#   tm_shape(Sites[which(Sites$`Site Category` %in% c("Treatment","Transfer")),]) + 
#   tm_bubbles(size = "total treated",
#              col = "Site Category",
#              border.col = "black",
#              clustering = TRUE,
#              legend.size.is.portrait = T,
#              palette = "Accent",
#              n = 8
#              )
#"MRS","Transfer", "Treatment"

window <- as.owin(england)

plot(window)

window$yrange

range(Sites$Y)

plot(x = Sites$X, y = Sites$Y)

#### Transfer facilities point pattern ####


plot.new()
par(mfrow=c(1,1))

sitesTransfer.ppp <- ppp(x = Sites$X[which(Sites$`Site Category`=="Transfer")], 
                         y = Sites$Y[which(Sites$`Site Category`=="Transfer")],
                         window = window)
sitesTransfer.ppp %>% plot(.,pch=16,cex=0.5, 
     main="Transfer facilities")

sitesTransfer.ppp %>% density(., sigma=5000) %>%
  plot()
# Ripley K for transfer 
Ktransfer <- sitesTransfer.ppp %>%
  Kest(., correction="border") %>%
  plot()

#### treatment facilities point pattern ####

sitesTreatment.ppp <- ppp(x = Sites$X[which(Sites$`Site Category`=="Treatment")], 
                       y = Sites$Y[which(Sites$`Site Category`=="Treatment")],
                       window = window)
sitesTreatment.ppp <- sitesTreatment.ppp[which(duplicated(sitesTreatment.ppp)== F),]

sitesTreatment.ppp %>% plot(.,pch=16,cex=0.5, 
                           main="Treatment facilities")

plot.new()
par(mfrow=c(1,1))

sitesTreatment.ppp %>% density(., sigma=5000) %>%
  plot(.,main = "Treatment sites density") + text(x = Cities$X, y = Cities$Y, labels=Cities$name, col = "green", cex = 1, pos = 3)

# Ripley's K treatment 
Ktreatment <- sitesTreatment.ppp %>%
  Kest(., correction="border") %>%
  plot()

#For text plot
Cities <- NULL
Cities$y <- c(51.5085300,53.45,52.4814200)
Cities$x <- c(-0.1257400,-2.55,-1.8998300)
Cities <- Cities %>% as.data.frame() %>% st_as_sf(.,coords = c("x", "y"), crs = 4326) %>% st_transform(.,27700) %>% 
  st_coordinates() %>% as.data.frame()
Cities$name <- c("London", "Manchester + Liverpool", "Birmingham")

#
#### MRS facilities point pattern ####

sitesMRS.ppp <- ppp(x = Sites$X[which(Sites$`Site Category`=="MRS" & Sites$`Facility RPA` == Regions[i])], 
                          y = Sites$Y[which(Sites$`Site Category`=="MRS" & Sites$`Facility RPA` == Regions[i])],
                          window = window)
sitesMRS.ppp %>% plot(.,pch=16,cex=0.5, 
                            main="MRS facilities")

sitesMRS.ppp %>% density(., sigma=0.1) %>%
  plot()

plot(sitesMRS.ppp)

# Ripleys K test for MRS

Kmrs <- sitesMRS.ppp %>%
  Kest(., correction="border") %>%
  plot()

#### K est subplot for regions facilities ####

#window = as.owin(ukregion[which(ukregion$rgn18nm == Regions[2]),])
# i <- 9
# regwindow <- ukregion[which(ukregion$rgn18nm == Regions[9]),] %>% st_transform(.,27700) %>% as.owin(.)
# plot(regwindow)

Regions <- unique(ukregion$rgn18nm)%>% as.character()
summary(Regions)

plot.new()
par(mfrow=c(3,3))
for (i in c(1:9)) {
  regwindow <- ukregion[which(ukregion$rgn18nm == Regions[i]),] %>% st_transform(.,27700) %>% as.owin(.)
  sites.ppp <- ppp(x = Sites$X[which(Sites$`Site Category`=="Transfer" & Sites$`Facility RPA` == Regions[i])], 
                      y = Sites$Y[which(Sites$`Site Category`=="Transfer" & Sites$`Facility RPA` == Regions[i])],
                      window = regwindow)
  #plot(sites.ppp, main = Regions[i])
  Kmrs <- sites.ppp %>%
    Kest(., correction="border") %>%
    plot(.,main = Regions[i],xlab = "r (m)", legend = F)
}


#### GRAPH THEORY ####

g <- graph_from_adjacency_matrix(WastereceivedMatrix,mode = "directed", weighted = T, diag = F, add.colnames = T)

g <-g %>% set_vertex_attr("name", value = Regions)

l <- coords <- layout_(g, as_star())

E(g)$width <- E(g)$weight * 20/ max(E(g)$weight)

E(g)$arrow.width <- E(g)$weight  / max(E(g)$weight)

E(g)$curved <- 0.2

plot(g, layout = l)

#### POPULATION VS WASTE TREATMENT CLUSTERS ####

wardspopulation <- wardspopulation[,c("Ward Code 1","Ward Name 1","Local Authority", "All Ages")]

#c("Ward Code 1","Ward Name 1","Local Authority", "All Ages")
#c("Ward Code 1","Ward Name 1","LA Code (2019 boundaries)","LA name (2019 boundaries)","LA Code (2020 boundaries)","LA name (2020 boundaries)","All Ages")
wardspopulation <- group_by(wardspopulation,`Local Authority`) %>% summarize(`Total`= sum(`All Ages`))

Wards <- merge(ukwards,wardspopulation, by.x = "lad17nm", by.y ="Local Authority")
tmap_mode("plot")
Wards$density <- Wards$Total / Wards$st_areasha

#tm_shape(ukwards) + tm_polygons() +
 tm_shape(Wards) + tm_polygons("Total",
                               title = "Population",
                                palette = "plasma",
                                n = 8,
                                contrast = c(0, 0.69),
                                lwd = 0,
                                )

qtm(Wards)

received$SitePC

colnames(postcodetola)
postcodetola <- postcodetola[,c("pcd7","pcd8","pcds","wd11cd","wd11nm","lad11cd","lad11nm")]

postcodePop <- postcodePop[,c(2,3,4,5)]

PCwaste <- merge(ukpost,Sites[which(Sites$`Site Category` == "Treatment"),], by.x = "name", by.y = "SitePC")

summary(PCwaste)

PCwaste$name <- str_replace_all(PCwaste$name, "[ ]", "")
postcodePop$`geography code` <- str_replace_all(postcodePop$`geography code`, "[ ]", "")
ukpost$name <- str_replace_all(ukpost$name, "[ ]", "")


PCpop <- merge(ukpost,postcodePop, by.x = "name", by.y = "geography code")

colnames(PCpop)[4] <- "TotalPopulation"

###
PCwastePop <- merge(PCwaste, postcodePop,by.x = "name",by.y = "geography code")

PCwaste[which(postcodePop$`geography code` %nin% PCwaste$name),]

summary(PCwastePop)

Cormodel <- lm(`total.treated` ~ `Variable: All usual residents`, PCwastePop)

print(Cormodel)
summary(Cormodel)

plot(log(PCwastePop$total.treated),log(PCwastePop$`Variable: All usual residents`))

tm_shape(PCpop) + tm_polygons("Variable: All usual residents",
                              palette = "plasma",
                              n = 8,
                              contrast = c(0, 0.69),
                              lwd = 0
                              )

# taken from https://gisforthought.com/uk-postcode-breakdown-regex/

# Area regex : ^[a-zA-Z][a-zA-Z]?

# District RegEX: ^[a-zA-Z]+\d\d?[a-zA-Z]?

# Sector RegEX: ^[a-zA-Z]+\d\d?[a-zA-Z]?\s*\d+

#str_extract(Sites$SitePC,"^[a-zA-Z]+\\d\\d?[a-zA-Z]?\\s*\\d+")

Sitesdata$Area <- str_extract(Sitesdata$SitePC,"^[a-zA-Z][a-zA-Z]?")
Sitesdata$District <- str_extract(Sitesdata$SitePC,"^[a-zA-Z]+\\d\\d?[a-zA-Z]?")
Sitesdata$Sector <- str_extract(Sitesdata$SitePC,"^[a-zA-Z]+\\d\\d?[a-zA-Z]?\\s*\\d+")

#Sitesdata$Sector

postcodePop$Area <- str_extract(postcodePop$`geography code`,"^[a-zA-Z][a-zA-Z]?")
postcodePop$District <- str_extract(postcodePop$`geography code`,"^[a-zA-Z]+\\d\\d?[a-zA-Z]?")

# by sector

Sitesdata$Sector <- str_replace_all(Sitesdata$Sector, "[ ]", "")

WasteCorSector <- merge(Sitesdata[which(Sitesdata$`Site Category` == "Transfer"),],postcodePop, by.x = "Sector", by.y = "geography code")

colnames(WasteCorSector)[15] <- "residents"

lm(`total treated` ~ `residents`, WasteCor) %>% summary()

plot(WasteCor$residents,WasteCor$`total treated`)

# by district 

WasteCorDistr <- group_by(Sitesdata[which(Sitesdata$`Site Category` == "Treatment" & Sitesdata$Fate == "Treatment" ),], District) %>% summarise(`distrWaste` = sum(`total treated`))

popDistrict <- group_by(postcodePop, District) %>% summarise(`distrPop` = sum(`Variable: All usual residents`))

WasteCorDistr <- merge(WasteCorDistr, popDistrict, by.x = "District", by.y = "District") 

lm(`distrWaste` ~ `distrPop`, WasteCorDistr ) %>% summary()

plot(WasteCorDistr$distrPop, WasteCorDistr$distrWaste)

view(WasteCorDistr)

### by AREA

wastefate <-  Sitesdata %>% group_by(.,Fate) %>% summarise(`treated` = sum(`total treated`))

sum(wastefate$treated[c(1:6,9)])

#

transfer <- c("Transfer (R)","Transfer (D)")
treatment <- c("Recovery","Treatment","Incineration with energy recovery")
other <- c("Not reported","Incineration without energy recovery","Landfill","Other Fate")

popArea <- group_by(postcodePop, Area) %>% summarise(`areaPop` = sum(`Variable: All usual residents`))

WasteCorArea <- group_by(Sitesdata[which(Sitesdata$`Site Category` == "Treatment"),], Area) %>% summarise(`areaWaste` = sum(`total treated`))

#[which(Sitesdata$`Site Category` == "Treatment"  & Sitesdata$Fate == "Treatment" ),]

WasteCorArea <- merge(WasteCorArea, popArea, by.x = "Area", by.y = "Area") 

wasteareamodel <-  lm(`areaWaste` ~ `areaPop`+ 0, WasteCorArea) #%>% summary()

summary(wasteareamodel)

#london <- c("NW","N","E","SE","SW","W")

plot.new()
plot(x= WasteCorArea$areaPop, y= WasteCorArea$areaWaste,main = "Waste treatment by population" , xlab = "Population", ylab = "Waste received")
lines(x=WasteCorArea$areaPop, y=wasteareamodel$fitted.values, col = "red")
text(x= WasteCorArea$areaPop[which(WasteCorArea$areaWaste == max(WasteCorArea$areaWaste))], y= WasteCorArea$areaWaste[which(WasteCorArea$areaWaste == max(WasteCorArea$areaWaste))], pos = 3, labels = WasteCorArea$area[which(WasteCorArea$areaWaste == max(WasteCorArea$areaWaste))])
legend('bottomright',inset=0.05,c("Fitted"),col = c("red"),lty=1,cex=1.5)
#view(WasteCorArea)

WasteCorArea[which(WasteCorArea$areaWaste == min(WasteCorArea$areaWaste)),]

# VERY IMPORTANT STEPS THAT ARE DONE ONCE FOR UKPOST
#ukpost$area <- str_extract(ukpost$name,"^[a-zA-Z][a-zA-Z]?")
#ukpost <- group_by(ukpost, area) %>% summarise(geometry = sf::st_union(geometry))

WasteCorArea <- merge(ukpost,WasteCorArea, by.x = "area", by.y ="Area")

WasteCorArea$resid <- wasteareamodel$residuals

plot(density(WasteCorArea$resid), main = "Residuals of transfer fit", ylab = "Density of residuals", xlab = "deviation")

#tmap_mode("plot")

  tm_shape(WasteCorArea) + tm_polygons("areaWaste",
                                       title = "Waste treatment by area (T)",
                                       palette = "Greys",
                                       n=5,
                                       contrast = c(.15,.8),
                                       #midpoint = 0,
                                       lwd = 0,
                                       border.alpha = 0) + 
    tmap_options(max.categories = 95)
  
  tm_shape(WasteCorArea) + tm_polygons("areaPop",
                                       title = "Population by area",
                                       palette = "Purples",
                                       n=5,
                                       contrast = c(.15,.8),
                                       #midpoint = 0,
                                       lwd = 0,
                                       border.alpha = 0) + 
    tmap_options(max.categories = 95)
  
  tm_shape(WasteCorArea) + tm_polygons("area",
                                       title = "Areas",
                                       palette = "Spectral",
                                       n=4,
                                       contrast = c(0,1),
                                       midpoint = 0,
                                       lwd = 0,
                                       border.alpha = 0
                                       ) + 
    tmap_options(max.categories = 95,legend.show = F)
  #[which(WasteCorArea$area =="W"), ]
  
#
#### WASTE by REGION CORRELATION ####

ukregion$rgn18nm
regionpop$AREA[7] <- "East of England"

region  <-  merge(ukregion,regionpop,by.x ="rgn18nm", by.y = "AREA")

tmap_mode("view")
tm_shape(region) + tm_polygons("X2018",
                               palette = "viridis", 
                               n = 7, 
                               contrast = c(0, 0.71)
                               )
Sitesdata <- Sites@data
unique(Sitesdata$Fate)

unique(Sitesdata$`Site Category`)
unique(Sitesdata$Fate)

regionwaste <- group_by(Sitesdata[which(Sitesdata$`Site Category` == "Treatment" & Sitesdata$Fate == "Treatment" ),], `Facility RPA`) %>% summarize(`WasteTreated` = sum(`total treated`))

# & Sitesdata$Fate == "Treatment"
# [which(Sitesdata$`Site Category` == "Treatment"),] 

regionWaste <- merge(region,regionwaste, by.x = "rgn18nm", by.y ="Facility RPA")

regionWaste$WasteTreated <- regionWaste$WasteTreated/1000 # in thousantds of tonnes  

model <- lm(`WasteTreated`~`X2018`,regionWaste)
summary(model)

plot.new()
plot(regionWaste$X2018,regionWaste$WasteTreated)

#,labels = regionWaste$rgn18nm, pos = 3

#### MAP OF TONNES OF WASTE THAT COMES FROM ANOTHER REGION ####
#### PLOTS ####

#TYPES OF WASTE : 
#Total
#Landfill
#Incineration with EfW
#Incineration without EfW
#Recycled/composted
#Other
wastetot <- filter(wasteusela, Year %in% "2018-19")
wastetot <-  wastetot[,c("Geographical Code",	"ONS Code",	"Jpp Order", 
                        "Region","Landfilled","Incineration with EfW",	"Incineration without EfW 4",	
                         "Recycled- Composted",
                         "Other1","Total2","Input to intermediate plants3")]#in thousands of tonnes

unique(wastetot$Region)

regionRecycle <- wastetot[,c("Region","Recycled- Composted","Total2")] %>% 
  group_by(`Region`) %>%
  summarise(Total = sum(`Total2`), Recycled = sum(`Recycled- Composted`))

RecycleRate <- (regionRecycle$Recycled/regionRecycle$Total * 100) %>%
  round(0)

wastetot <-  data.frame(regionRecycle,RecycleRate) 
wastetot$Region[10] <- "Yorkshire and The Humber"
wastetot$Region[2] <- "East of England"
wastetot
# names(wastetot) <- c("Geographical Code",	"ONS Code",	"Jpp Order", 
#                      "Region","Landfilled","Incineration with EfW",	"Incineration without EfW 4",	
#                      "Recycled- Composted",
#                      "Other1","Total2","Input to intermediate plants3","Recycle rate")

wasteMap <- merge(ukregion,wastetot, by.x = "rgn18nm", by.y = "Region")
wasteMap

tmap_mode("plot")

#qtm(niMap)


png(filename="RecycleRateEngland")
tm_shape(wasteMap) + 
  tm_polygons("RecycleRate", 
              style="fixed",
              palette = "BuPu", 
              #n = 5,
              midpoint=NA,
              breaks = c(30,34,38,42,46,50),
              title="Recycle rate (%)",
              alpha = 1
              ) + tm_layout(
                legend.title.size=1.5,
                legend.text.size = 1,
                
                legend.position = c("left","top"),
                legend.bg.color = "white",
                legend.bg.alpha = 0)
  
dev.off()