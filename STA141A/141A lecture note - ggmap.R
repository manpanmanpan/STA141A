air = read.csv("2018.01_air_delays.csv",header = TRUE)
head(air)
air$ontime = (air$ARR_DELAY <= 0)

library(ggplot2)
ggplot(air,aes(OP_UNIQUE_CARRIER,fill=ontime)) + geom_bar(position = "dodge")

sum(air$ontime,na.rm=TRUE) # NUMBER OF FLIGHT ON TIME PER CA?RIER (no NA)
aggregate(ontime~OP_UNIQUE_CARRIER,air,sum)
table(air$OP_UNIQUE_CARRIER) # total number of flights per carrier (INCLUDING NA )
 
# do we want to include flights where on-time is NA in the grand total?
# how mant NAs are there? Will it make a b?g difference?
# or compute both and compare?

ggplot(air,aes(OP_UNIQUE_CARRIER,fill=ontime)) + geom_bar(position = "fill") # find NA

ggplot(air,aes(OP_UNIQUE_CARRIER,y=stat(prop),fill=ontime,
               group = 1)) + geom_bar(position = "dodge") # fin? NA

total = as.data.frame(table(air$OP_UNIQUE_CARRIER))
names(total) = c('carrier','flights')

ontime = aggregate(ontime~OP_UNIQUE_CARRIER,air,sum)
ontime$ontime / total$flights
m = merge(ontime, total,by.x = "OP_UNIQUE_CARRIER",by.y = "carrier")
head(m)
?$prop = m$ontime/m$flights
ggplot(m,aes(OP_UNIQUE_CARRIER,prop)) + geom_bar(stat = "identity")

m[order(m$prop),]
#
# this won't work
m$OP_UNIQUE_CARRIER = as.character(m$OP_UNIQUE_CARRIER)
# we can use factor() to reorder the categories ????
m$OP_UNIQUE_C?RRIER = factor(m$OP_UNIQUE_CARRIER,levels(m$OP_UNIQUE_CARRIER))

# another way to do that
order_carrier = reorder(m$OP_UNIQUE_CARRIER,m$prop)
m$OP_UNIQUE_CARRIER = order_carrier
ggplot(m,aes(OP_UNIQUE_CARRIER,prop)) + geom_bar(stat = "identity")


########?####################################################################
# are there more delays on certain days of the week? are there more flights?
# how would we find out how many flights are very late (beyond the whisker)?

days = read.csv("L_WEEKDAYS.csv"?header=TRUE)
row_num = match(air$DAY_OF_WEEK,days$Code)
tail(air$DAY_OF_WEEK)

air$DAY_OF_WEEK = days$Description(row_num)
ggplot(air, aes(as.factor(DAY_OF_WEEK), ARR_DELAY)) + geom_boxplot() + ylim(-100,100)

# estimate that whisker ends around 40 mintues?tbl = table(air$ARR_DELAY>40,air$DAY_OF_WEEK)
tbl
prop.table(tbl,2)

### ##################### 3
ap = read.csv('airports.dat',header=TRUE)
head(ap)
names = c('ID',"Name",'City','Country','IATA',"ICAO",'latitude','Longitude','Altitude',
          'Timezone'?'DST','Tz','Type','Source')
names(ap) = names

sapply(ap,class)
str(ap)

# save the data set as an RDS file for future use
saveRDS(ap, 'airports.rds')

#### ggmap
library(ggmap)
install.packages('devtools')
devtools::install_github('dkahle/ggmap', ref = 't?dyup')
citation("ggmap") # or for other packages as well

# stamen maps
m = get_stamenmap()
ggmap(m)


options(repos = getOption("repos")["CRAN"])

ggmap(m) + geom_point(aes(Longitude, latitude),ap)
us = ap[ap$Country == "United Stated",]
ggmap(m) +  geom_?oint(aes(Longitude, latitude),us,alpha = 0.25, size = 0.5)





