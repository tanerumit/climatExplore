
library(stringr)
library(stringi)

rawdata <- read.csv("./data/climRegionCoords.csv")
cleandata <- rawdata

cleandata$lat1 <- sub(".", "", rawdata$lat1) %>% str_sub(end = -2)
cleandata$lat2 <- sub(".", "", rawdata$lat2) %>% str_sub(end = -2)
cleandata$lat3 <- sub(".", "", rawdata$lat3) %>% str_sub(end = -2)
cleandata$lat4 <- sub(".", "", rawdata$lat4) %>% str_sub(end = -2)
cleandata$lat5 <- sub(".", "", rawdata$lat5) %>% str_sub(end = -2)
cleandata$lat6 <- sub(".", "", rawdata$lat6) %>% str_sub(end = -2)

cleandata$lon1 <- rawdata$lon1 %>% str_sub(end = -2)
cleandata$lon2 <- rawdata$lon2 %>% str_sub(end = -2)
cleandata$lon3 <- rawdata$lon3 %>% str_sub(end = -2)
cleandata$lon4 <- rawdata$lon4 %>% str_sub(end = -2)
cleandata$lon5 <- rawdata$lon5 %>% str_sub(end = -2)
cleandata$lon6 <- rawdata$lon6 %>% str_sub(end = -2)



cleandata$lat1 <- sapply(cleandata$lat1, function(x)
  ifelse(stri_sub(x, from = -1, to = -1) == "N", 
         as.numeric(stri_sub(x, from =  1, to = -2)), 
         -1 * as.numeric(stri_sub(x, from =  1, to = -2))))

cleandata$lat2 <- sapply(cleandata$lat2, function(x)
  ifelse(stri_sub(x, from = -1, to = -1) == "N", 
         as.numeric(stri_sub(x, from =  1, to = -2)), 
         -1 * as.numeric(stri_sub(x, from =  1, to = -2))))

cleandata$lat3 <- sapply(cleandata$lat3, function(x)
  ifelse(stri_sub(x, from = -1, to = -1) == "N", 
         as.numeric(stri_sub(x, from =  1, to = -2)), 
         -1 * as.numeric(stri_sub(x, from =  1, to = -2))))

cleandata$lat4 <- sapply(cleandata$lat4, function(x)
  ifelse(stri_sub(x, from = -1, to = -1) == "N", 
         as.numeric(stri_sub(x, from =  1, to = -2)), 
         -1 * as.numeric(stri_sub(x, from =  1, to = -2))))


cleandata$lat5 <- sapply(cleandata$lat5, function(x)
  ifelse(stri_sub(x, from = -1, to = -1) == "N", 
         as.numeric(stri_sub(x, from =  1, to = -2)), 
         -1 * as.numeric(stri_sub(x, from =  1, to = -2))))

cleandata$lat6 <- sapply(cleandata$lat6, function(x)
  ifelse(stri_sub(x, from = -1, to = -1) == "N", 
         as.numeric(stri_sub(x, from =  1, to = -2)), 
         -1 * as.numeric(stri_sub(x, from =  1, to = -2))))

         
######--------------------------------------------------------------------######

cleandata$lon1 <- sapply(cleandata$lon1, function(x)
  ifelse(stri_sub(x, from = -1, to = -1) == "E", 
         as.numeric(stri_sub(x, from =  1, to = -2)), 
         -1 * as.numeric(stri_sub(x, from =  1, to = -2))))

cleandata$lon2 <- sapply(cleandata$lon2, function(x)
  ifelse(stri_sub(x, from = -1, to = -1) == "E", 
         as.numeric(stri_sub(x, from =  1, to = -2)), 
         -1 * as.numeric(stri_sub(x, from =  1, to = -2))))

cleandata$lon3 <- sapply(cleandata$lon3, function(x)
  ifelse(stri_sub(x, from = -1, to = -1) == "E", 
         as.numeric(stri_sub(x, from =  1, to = -2)), 
         -1 * as.numeric(stri_sub(x, from =  1, to = -2))))

cleandata$lon4 <- sapply(cleandata$lon4, function(x)
  ifelse(stri_sub(x, from = -1, to = -1) == "E", 
         as.numeric(stri_sub(x, from =  1, to = -2)), 
         -1 * as.numeric(stri_sub(x, from =  1, to = -2))))


cleandata$lon5 <- sapply(cleandata$lon5, function(x)
  ifelse(stri_sub(x, from = -1, to = -1) == "E", 
         as.numeric(stri_sub(x, from =  1, to = -2)), 
         -1 * as.numeric(stri_sub(x, from =  1, to = -2))))

cleandata$lon6 <- sapply(cleandata$lon6, function(x)
  ifelse(stri_sub(x, from = -1, to = -1) == "E", 
         as.numeric(stri_sub(x, from =  1, to = -2)), 
         -1 * as.numeric(stri_sub(x, from =  1, to = -2))))


polyNames <- cleandata$name
polyIDs   <- 1:length(polyNames)
data <- list()
for (i in 1:nrow(cleandata)) {
  data[[i]] <- data.frame(lat = as.numeric(cleandata[i,c(4,6,8,10,12,14)]),
                          lon = as.numeric(cleandata[i,c(3,5,7,9,11,13)])) %>% drop_na() 
}

# make each element of list polygon
ps <- lapply(data, Polygon)
# add id variable
p1 <- lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]), ID = polyNames[i]))
# create SpatialPolygons object
my_spatial_polys <- SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84") ) 
# create spatialPolygons dataframe
my_spatial_polys_df <- SpatialPolygonsDataFrame(Sr = my_spatial_polys, 
  data = data.frame(id = polyNames, row.names = polyNames))

dataTemp <- my_spatial_polys_df

