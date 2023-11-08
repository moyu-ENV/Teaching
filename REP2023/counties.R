install.packages("SeerMapper")
library(SeerMapper)
library(tidyverse)
library(rgeos)
library(sf)

USADataFrom2017 <- USA_data_CoordAndState %>%
  filter(year >= "2017") %>%
  select(segment,year,centroid,centerLon,centerLat,state)

stateL <- unique(USADataFrom2017$state)

FloridaData <- USADataFrom2017 %>%
  filter(state == "Florida")

ct <- counties()

FloridaCounties <- counties("Florida")


#USADataFrom2017 <- unite(USADataFrom2017, col = 'coordinates', c('centerLon','centerLat'), sep =', ')
#USADataFrom2017 <- USADataFrom2017 %>% select(segment,state,year,coordinates)

FloridaData <- USADataFrom2017 %>% filter(state=="Florida")
FloridaCoords <- data.frame(Lat = c(FloridaData$centerLat), LON = c(FloridaData$centerLon))
test1coords <- cbind(FloridaCoords[1,])


test1coords

test1 <- FloridaCounties$geometry[1]

gContains(test1,test1coords)


result <- coordinates_in_polygon(FloridaCoords[1], test1)

print(result)

coords_sf <- st_as_sf(test1coords, coords = c("LON","Lat"))

test1

plot(test1)

intersect(coords_sf, test1)

intersect(test1,coords_sf)



