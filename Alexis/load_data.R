library(plyr)
library(tidyverse)
library(fmsb)


# fifa19_final <- read.csv(file = "../final_fifa.csv")
# fifa19_final <- fifa19_final[-c(1)]
# 
# studen <- read.csv(file = "../students.csv")
# sleep <- read.csv(file = "../sleepStudy.csv")
# 
# age <- fifa19_final[c(2)]
# 
# sexe <- c("H","F","H")
# 
# test <-table(sexe)
# 
# print(typeof(sexe))
# print(typeof(age))
# print(typeof(test))
# 
# head(fifa19_final)
# 
# overall <- fifa19_final[c(4)]
# 
# skill_player <- data.frame(
#   row.names = c("Messi",'Ronaldo',"Neymar Jr","Ramos"),
#   pac = c(88,90,92,75),
#   sho = c(91,93,84,63),
#   pas = c(88,81,83,71),
#   drib = c(96,90,95,72),
#   def = c(32,35,32,91),
#   phy = c(61,79,59,84)
#   
# )
# skill_player
# 
# 
# 
# max_min <- data.frame(
#   pac = c(100, 0), sho = c(100, 0), pas = c(100, 0),
#   drib = c(100, 0), def = c(100, 0), phy = c(100, 0)
# )
# 
# rownames(max_min) <- c("Max", "Min")
# 
# # Rattacher les plages de variables aux données
# df <- rbind(max_min, skill_player)
# df
# 
# student1_data <- df[c("Max", "Min", "Messi"), ]
# #radarchart(student1_data)
# 
# create_beautiful_radarchart <- function(data, color = "#00AFBB", 
#                                         vlabels = colnames(data), vlcex = 0.7,
#                                         caxislabels = NULL, title = NULL, ...){
#   radarchart(
#     data, axistype = 1,
#     # Personnaliser le polygone
#     pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
#     # Personnaliser la grille
#     cglcol = "grey", cglty = 1, cglwd = 0.8,
#     # Personnaliser l'axe
#     axislabcol = "grey", 
#     # Étiquettes des variables
#     vlcex = vlcex, vlabels = vlabels,
#     caxislabels = caxislabels, title = title, ...
#   )
# }
# 
# op <- par(mar = c(1, 2, 2, 1))
# create_beautiful_radarchart(student1_data, caxislabels = c(0, 25, 50, 75, 100))
# par(op)
# 
# 
# # Réduire la marge du graphique à l'aide de par()
# op <- par(mar = c(1, 2, 2, 2))
# # Créer les graphiques radar
# create_beautiful_radarchart(
#   data = df, caxislabels = c(0, 5, 10, 15, 20),
#   color = c("#00AFBB", "#E7B800", "#FC4E07")
# )
# # Ajouter une légende horizontale
# legend(
#   x = "bottom", legend = rownames(df[-c(1,2),]), horiz = TRUE,
#   bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
#   text.col = "black", cex = 1, pt.cex = 1.5
# )
# par(op)
# 
# 
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

# fifa19 <- read.csv(file = "../data.csv")
# dataset <- fifa19 %>% select(Name,Age,Nationality,Overall,Potential,Value,Wage,Preferred.Foot,Weak.Foot,Skill.Moves,Position,Contract.Valid.Until,,Height,Weight,Acceleration,SprintSpeed,Positioning,Finishing,ShotPower,LongShots,Volleys,Penalties,Vision,Crossing,FKAccuracy,ShortPassing,LongPassing,Curve,Agility,Balance,Reactions,BallControl,Dribbling,Composure, Interceptions,HeadingAccuracy,Marking,StandingTackle,SlidingTackle,Jumping,Stamina,Strength,Aggression,Release.Clause)
# write.csv(dataset,"selected_data.csv", row.names = TRUE)
# 
# fifa19_init <- read.csv(file = "../final_fifa.csv")
# fifa19_final <- fifa19_init[-c(1)]
# 
# fifa19_init2 <- read.csv(file = "../final_fifa_stat.csv")

# 
# m <- unlist(fifa19_stat[c(1)])
# print(typeof(m))
# m <- unlist(m)
# t <- c("Messi",'Ronaldo',"Neymar Jr","Ramos")
# print(typeof(t))

# fifa19_final2 <- fifa19_init2[-c(1)]
# 
# fifa19_init <- read.csv(file = "../final_fifa_stat.csv")
# fifa19_final <- fifa19_init[-c(1)]
# fifa19_cor <- fifa19_init[-c(1, 9, 10, 11, 12, 17, 18,19,20,21,22,23,24)]
# 
# 
# data_player = list("name_player" = fifa19_cor  )
# print(typeof(fifa19_cor))
# 
# fifa19_stat <- fifa19_init[c(2,17,18,19,20,21,22)]
# fifa19_stat
# names(fifa19_stat) <- NULL
# unique_name <- make.names(fifa19_stat[,1], unique = TRUE)
# 
# skill_player <- data.frame(
#   row.names = unique_name,
#   pac = unlist(fifa19_stat[2]),
#   sho = unlist(fifa19_stat[3]),
#   pas = unlist(fifa19_stat[4]),
#   drib = unlist(fifa19_stat[5]),
#   def = unlist(fifa19_stat[6]),
#   phy = unlist(fifa19_stat[7])
# )
# 
# skill_player
# max_min <- data.frame(
#   pac = c(100, 0), sho = c(100, 0), pas = c(100, 0),
#   drib = c(100, 0), def = c(100, 0), phy = c(100, 0)
# )
# 
# rownames(max_min) <- c("Max", "Min")
# 
# df <- rbind(max_min, skill_player)
# show_player <- df[c("Max", "Min", "N..Kanté"), ]
# radarchart(show_player)
# 
# name_player <- fifa19_stat[c(1)]
# print(head(name_player,10))
# 
# 
# name_player <- fifa19_final[c(1)]
# name_player <- head(name_player,10)
# names(name_player) <- NULL



#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="DATA/world_shape_file.zip")
#system("unzip DATA/world_shape_file.zip")


# Read this shape file with the rgdal library.
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
# 
# plot(G1)

library(maptools)
data(wrld_simpl)
myCountries = wrld_simpl@data$NAME %in% c("Australia", "United Kingdom", "Germany", "United States", "Sweden", "Netherlands", "New Zealand")
plot(wrld_simpl, col = c(gray(.80), "red")[myCountries+1])

