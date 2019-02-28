dogs=readRDS("dogs_full.rds")

# A zip file is a kind of archive. An archive bundles a bunch of files into one file
# compresssion shrinks files. EX:
# -- Lossy compression  -- jpeg (photos)
# -- Lossless compression (no infor lost) --zip
#  
# CSV stands ?or comma-separated values
# you can peek at the CSV file with excel/spreadsheet or text editor
#

air = read.csv("2018.01_air_delays.csv",header = TRUE)
names(air)
dim(air)
summary(air)

# CSV files are a bit messy
# need to check that R data types are app?opriate for the statistical data types
# 

day_of_week = factor(air$DAY_OF_WEEK)
summary(day_of_week)
levels(day_of_week)

# rename the levels
levels(day_of_week) = c('Monday',"Tuesday", "...")
# or
levels(day_of_week) = days$Description

match(day_of_week? days$Code)


# reorder the levels
factor(new_day_of_week, levels(new_day_of_week)[C(2,...)])

# what kind of questions can we ask (or answer) with this data?

# what airports are most likely to have delays? or least likely?
# check more seasonal delays
# ?# what area or region is most likely to have delay?
# which carriers have the most on time flights?
# what are the main reason of delay? - specifically with weather
# Does a delay on one flight cause later delays (for the same plane)?
# are there more dela?s on certain days of the week? Are there more flights?
# 



table (air$DAY_OF_WEEK)
# let is look at arrival delay (numeric) and day of week (category)
# make our question more specific the context of the data set

ggplot(air, aes(as.factor(DAY_OF_WEEK), ?RR_DELAY)) + geom_boxplot() 

# most flights are on time
# but flights that aren't on time are mostly very late
ggplot(air, aes(as.factor(DAY_OF_WEEK), ARR_DELAY)) + geom_boxplot() + ylim(-50,50) + 
  geom_hline(yintercept = 0)


# how would we find out ho? many flights are delayed a lot (beyond the whisker) vs not delay?

#  # which carriers have the most on time flights?

# count the number of on-time flights for each carrier
# what is an "on time flight"
# we could look at (CRS_ARR - ACTUAL_ARR ) or any_D?LAY
# we can define on-time in different ways, depending on our perspective:
   #  --- customer perspective : anything <= 0 delay is on - time
   #  --- airport perspective : only 0 delay
# let is take the customer perspective 

# let's use ARR_DELAY

onti?e = air$ARR_DELAY <= 0 
table(ontime)
air$ontime = ontime
table(air$ontime, air$OP_UNIQUE_CARRIER)

ggplot(air, aes(OP_UNIQUE_CARRIER, fill = ontime)) + geom_bar(position = "dodge")










