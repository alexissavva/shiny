# fifa <- read.csv(file = "../data.csv")
# tata <- summarise(fifa19_init,"defance" = mean())

#taille <- unique(dataset[c("Nationality")])
#print(taille)
#typeof(taille)
#nbNatio <- fifa19 %>% pull(Nationality)
#df_uniq <- unique(nbNatio)
#length(df_uniq)
#print(df_uniq)
#a <- table(dataset['Nationality'])
#print(a)
#fifa19 <- read.csv(file = "data.csv")
#dataset <- fifa19 %>% select(Name,Age,Nationality,Overall,Potential,Club,Value,Wage,Preferred.Foot,Weak.Foot,Skill.Moves,Position,Contract.Valid.Until,Height,Weight,Release.Clause)
#ds <- fifa19 %>% pull(Nationality)
#ds <- rle(sort(ds))
#b <- data.frame(Nationality=ds$values, number=ds$lengths)


#Pour Info
#iris
#iris_bis <- iris[,-2] # On supprime la 2eme colonne
#iris_ter <- iris[-20,] # On supprime la 20eme ligne

#df2 <- dataset[dataset$Overall>70,]
#df3 <- dataset[dataset$Overall<70,]


#random_dataset <- dataset[sample(nrow(dataset), 3), ]

# library(rgdal)
# library(dplyr)
# library(leaflet)
# library(ggplot2)
# 
# world_spdf <- readOGR(
#   dsn = paste0(getwd(), "/DATA/world_shape_file/") ,
#   layer = "TM_WORLD_BORDERS_SIMPL-0.3",
#   verbose = FALSE
# )
# 
# # Clean the data object
# world_spdf@data$POP2005[which(world_spdf@data$POP2005 == 0)] = NA
# world_spdf@data$POP2005 <-
#   as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)
# 
# 
# # Create a color palette for the map:
# mypalette <-
#   colorNumeric(
#     palette = "viridis",
#     domain = world_spdf@data$POP2005,
#     na.color = "transparent"
#   )
# mypalette(c(45, 43))
# 
# # Basic choropleth with leaflet?
# m <- leaflet(world_spdf) %>%
#   addTiles()  %>%
#   setView(lat = 10, lng = 0 , zoom = 2) %>%
#   addPolygons(fillColor = ~ mypalette(POP2005), stroke = FALSE)
# 
# m
# 
# world_spdf@data %>% 
#   ggplot( aes(x=as.numeric(POP2005))) + 
#   geom_histogram(bins=20, fill='#69b3a2', color='white') +
#   xlab("Population (M)") + 
#   theme_bw()

# m <- leaflet(world_spdf)%>% addTiles()  %>% setView( lat=10, lng=0 , zoom=2) %>%
#   addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", POP2005)(POP2005) )
# m
# 
# # Numeric palette
# m <- leaflet(world_spdf)%>% addTiles()  %>% setView( lat=10, lng=0 , zoom=2) %>%
#   addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorNumeric("YlOrRd", POP2005)(POP2005) )
# m
# 
# # Bin
# m <- leaflet(world_spdf)%>% addTiles()  %>% setView( lat=10, lng=0 , zoom=2) %>%
#   addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorBin("YlOrRd", POP2005)(POP2005) )
# m

# Create a color palette with handmade bins.
# library(RColorBrewer)
# mybins <- c(0,10,20,50,100,500,Inf)
# mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$POP2005, na.color="transparent", bins=mybins)
# 
# # Prepare the text for tooltips:
# mytext <- paste(
#   "Country: ", world_spdf@data$NAME,"<br/>", 
#   "Area: ", world_spdf@data$AREA, "<br/>", 
#   "Population: ", round(world_spdf@data$POP2005, 2), 
#   sep="") %>%
#   lapply(htmltools::HTML)
# 
# # Final Map
# leaflet(world_spdf) %>% 
#   addTiles()  %>% 
#   setView( lat=10, lng=0 , zoom=2) %>%
#   addPolygons( 
#     fillColor = ~mypalette(POP2005), 
#     stroke=TRUE, 
#     fillOpacity = 0.9, 
#     color="white", 
#     weight=0.3,
#     label = mytext,
#     labelOptions = labelOptions( 
#       style = list("font-weight" = "normal", padding = "3px 8px"), 
#       textsize = "13px", 
#       direction = "auto"
#     )
#   ) %>%
#   addLegend( pal=mypalette, values=~POP2005, opacity=0.9, title = "Population (M)", position = "bottomleft" )
# 
# m
# 
# fifa19_natio <- fifa19_final[-c(1)]
# natio <- fifa19_natio[c("Nationality")]
# print(table(natio))
# map <- as.data.frame(table(natio))
# taille <- unique(fifa19_final[c("Nationality")])
# print(map)
# 
# library("ggplot2")
# theme_set(theme_bw())
# library("sf")
# library("rnaturalearth")
# library("rnaturalearthdata")
# library(rgeos)
# 
# world <- ne_countries(scale = "medium", returnclass = "sf")
# class(world)
# ggplot(data = world) + geom_sf()

library(rworldmap)

fifa19_final <- read.csv(file = "../final_fifa.csv")
country <- fifa19_final[c("Nationality")]
print(table(country))
map <- as.data.frame(table(country))
map2 <- read.csv(file = "country.csv",sep = ";")
#write.csv(map,"country.csv", row.names = TRUE)
visitedMap <- joinCountryData2Map(map2, 
                                  joinCode = "NAME",
                                  nameJoinColumn = "country",
                                  zoomControl: TRUE,
                                  verbose = TRUE)
mapParams <- mapCountryData(visitedMap, 
                            nameColumnToPlot="Freq",
                            oceanCol = "azure2",
                            zoomControl: TRUE,
                            catMethod = "categorical",
                            missingCountryCol = gray(.8),
                            colourPalette = c("coral",
                                              "coral2",
                                              "coral3", "orangered", 
                                              "orangered3", "orangered4"),
                            addLegend = F,
                            mapTitle = "",
                            border = NA)
# add legend and display map

# G1 <- gvisGeoMap(map2,locationvar='country',numvar='Freq',options=list(dataMode='regions'))
#plot(G1)


library(dplyr)
library(leaflet)
library(maps)
library(plyr)
library(tidyverse)
library(fmsb)


cnt_country <- read.table(text = "Morocco,57381\nFrance,35729\nTunisia,85563\nSaudi Arabia,10816\nTurkey,6725",
                          sep = ",",
                          header = FALSE,
                          stringsAsFactors = FALSE,
                          col.names = c("country", "n"))

map <- read.csv(file = "country.csv",sep=";")
names(map)[names(map) == "Freq"] <- "n"

# import the world.cities data frame from 'maps'
data(world.cities)

# merge the desired cols from that data frame with yours by country
df <- world.cities %>%
  filter(capital == 1) %>%
  dplyr::select(country = country.etc, lat, lng = long) %>%
  left_join(cnt_country, ., by = "country")

df2 <- world.cities %>%
  filter(capital == 1) %>%
  dplyr::select(country = country.etc, lat, lng = long) %>%
  left_join(map, ., by = "country")


# now map the result
leaflet(df2)%>%
  addTiles()%>%
  #addMarkers(label = ~n) 
  addPolygons()



leaflet(df2)%>% addTiles()%>% setView( lat=10, lng=0 , zoom=2) %>% addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", POP2005)(POP2005) )



world_spdf <- readOGR( 
  dsn= paste0(getwd(),"/DATA/world_shape_file/") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)
world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)

all <- world_spdf@data[c("NAME")]

mypalette <- colorNumeric( palette="viridis", domain=world_spdf@data$POP2005, na.color="transparent")
mypalette(c(45,43))

# Basic choropleth with leaflet?
m <- leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( fillColor = ~mypalette(POP2005), stroke=FALSE )

m

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, 1500)
pal <- colorBin("YlOrRd", domain = df2$n, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  df2$country, df2$n
) %>% lapply(htmltools::HTML)

leaflet(df2) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal(n),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~n, opacity = 0.7, title = NULL,
            position = "bottomright")

m <- leaflet(df2) %>%
  setView( lat=10, lng=0 , zoom=2) %>%
  addTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))


m2 <- leaflet(df2)%>%
  addTiles()%>%
  setView( lat=10, lng=10 , zoom=1)

m2 %>% addPolygons()

library(geojsonio)
library(rgdal)
library(rjson)
library(geojsonR)
file_js = FROM_GeoJson(url_file_string = "countries.geojson")
names(file_js$features[[1]])
str(file_js)
summary(file_js$features[[1]])
str(file_js$features[[1]]$geometry)
file_js$features[[1]]$geometry$coordinates
str(file_js$features[[1]]$properties)

