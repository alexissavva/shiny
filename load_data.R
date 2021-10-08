library(plyr)
library(tidyverse)


fifa19_final <- read.csv(file = "final_fifa.csv")
fifa19_final <- fifa19_final[-c(1)]

studen <- read.csv(file = "students.csv")
sleep <- read.csv(file = "sleepStudy.csv")

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
