library(WikidataQueryServiceR)
library(tidyr)
library(dplyr)
testdata <- data.frame(query_wikidata('SELECT DISTINCT ?ship ?coordinates ?canmore ?shipLabel WHERE{

 ?ship wdt:P31 wd:Q852190 .
 ?ship wdt:P625 ?coordinates .
 ?ship wdt:P718 ?canmore .
 SERVICE wikibase:label{bd:serviceParam wikibase:language"[AUTO_LANGUAGE],en".}

}'))

testdata$coordinates<-gsub("[Point()]", "", testdata$coordinates)

testdata<-separate(testdata, col = coordinates, into=c("Longitude", "Latitude"), sep=" ")

testdata$Longitude<- as.numeric(testdata$Longitude)
testdata$Latitude<- as.numeric(testdata$Latitude)

testdata<-filter(testdata, Longitude>-2,Latitude>57, Latitude<57.5)
