citation('ggmap') # for other packages as well

# Stamen maps
m = get_stamenmap()

# Creat map layer
ggmap(m)

?get_stamenmap

bbox = c(
  -121.772613, 38.532675, # bottom left
  -121.695120, 38.571458 # top right
)

m = get_stamenmap(bbox, zoom = 15) # zo?m default = 10
ggmap(m)

m = get_stamenmap(bbox, zoom = 12, maptype = 'toner-lite') # zoom default = 10
ggmap(m)

bbox = c(-127.75,21.78,-65.31,50.53)
m = get_stamenmap(bbox, zoom = 3, maptype = 'toner-lite')
ggmap(m)



head(ap)
ggplot(ap) + geom_point(ae?(Longitude, Latitude))
ggmap(m) + geom_point(aes(Longitude, Latitude), data = ap)

# We should subset the data to just US airports

us = ap[ap$Country == 'United States', ]
ggmap(m) + geom_point(aes(Longitude, Latitude), data = us, alpha = 0.25, size = 3)
?air = read.csv("2018.01_air_delays.csv",header = TRUE)
# 



# Do any planes "disappear" or "appear" in this data set?

# do any planes move around without schedule flights?

departures = as.data.frame(table(air$ORIGIN))
names(departures) = c('airport','co?nt')
head(departures[order(departures$count),])

dep_ap = merge(departures,ap,by.x = "airport",by.y="IATA")

# what are the 3 airports that disappeared?
setdiff(departures$airport,dep_ap$airport)
match("USA",ap$IATA)
departures[departures$airport=='USA',]
?bbox = c(-130.31,24.41,-63.25,49.7)
m = get_stamenmap(bbox, zoom = 3, maptype = "toner-lite")
ggmap(m) + geom_point(aes(Longitude, Latitude, size = count, color = count), dep_ap, alpha = 0.25)

ggmap(get_googlemap())

loc = c(-121.740092 , 38.544885)
m = g?t_googlemap(loc)
ggmap(m)
get_googlemap("Davis, CA")



###############################  11/1/2018
# str_remove_all()
# shapefiles
#recap

#string processing
 
library(stringr)
x = 'cats are fun'
class(x)

str_remove_all(x, 'cats')

X=c('cats are fun','I l?ke cats')
str_remove_all(X, fixed('cats', ignore_case = TRUE))
str_remove_all(X, fixed('cats'))

str_remove('cats are fun . I like cats','cats')
str_remove_all('cats are fun . I like cats','cats')

str_detect('cats','.')
str_detect('cats',fixed('.'))

# ho? do we go from lat, lon to places or states, cities, etc?
cl = readRDS('cl_apartments.rds')
head(cl)

# shape file 
install.packages("sf")
library(sf)



places = read_sf('2018_place_shapefiles/tl_2018_06_place.shp')
class(places)
head(places)

davis = pla?es[places$NAME == 'Davis',]
ggplot(davis) + geom_sf()
ggplot(places) + geom_sf()

point = cl[1, c("longitude", 'latitude')]
point = as.numeric(point)
point = st_point(point)
?st_point
class(point)

?st_crs

is_in = st_contains(places,point,sparse = FALSE)
?hich(is_in)
places$NAME[is_in]



st_within(point,places)


dogs= readRDS('dogs_full.rds')
by_group = split(dogs$lifetime_cost,dogs$group)
length(by_group)
names(by_group)
by_group[[1]]

mean(by_group[[1]], na.rm=T)
sapply(by_group, mean, na.rm = T)





