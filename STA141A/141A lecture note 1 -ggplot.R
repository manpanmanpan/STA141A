dogs=readRDS("dogs_full.rds")
getwd()
head(data)
list.files()
data2=readRDS("dogs_top.rds")
head(data2)
ncol(data2)
nrow(data2)
dim(data)
colnames(data)
names(data) # the same as colnames()
str(data2)
str(data)  # summary the data
head(data,10)
summary(dat?)
sd(data$size)
sd(data$height,na.rm=TRUE)
table(is.na(data$height))
data[1,]

which(is.na(data$height))

names(dogs)

library(ggplot2)
ggplot(dogs)+geom_point(aes(x=datadog,y=popularity))

ggplot(dogs,aes(x = datadog, y = popularity, color = group)) + geo?_point() +
  geom_text(aes(label = breed))


# scale - control hoe data numbers are transformd to screen 
ggplot(dogs,aes(x = datadog, y = popularity, color = group)) + geom_point() +
  geom_text(aes(label = breed)) + scale_y_reverse()
# or
ggplot(dogs,aes?x = datadog, y = -popularity)) + geom_point(color = "blue") +
  geom_text(aes(label = breed), hjust = "left", vjust = "top")


# theme - overall style of the plot

ggplot(dogs,aes(x = datadog, y = popularity, color = group)) + geom_point() +
  geom_text(ae?(label = breed, hjust = "left", vjust = "top")) + scale_y_reverse()  +
  labs(title = "Best in Show", x= "Computed DataDog Score", y = "AKC Popularity Ranking") + 
  guides(color = guide_legend(title = "Dog Type"))

# ggtitle -- title    xlab() --x label  ?lab() -- y label 

# barchart  --  Display the group variable graphically
ggplot(dogs,aes(x = group, fill = size)) + geom_bar(position = "dodge")
table(dogs$group)
table(dogs$size)

# categorical    * Nominal   * Ordinal
# Numerical      * Discrete -- year?, people, etc    * Continuous -- years, height, weight
# Image
# Spatial
# Text


# R 's Data  Type
typeof('hello')
typeof("5.11")

typeof(1+3i)
typeof(4L)
typeof(mean)

# converting types
typeof(as.numeric("5.1"))
typeof(c(5,3,2))


#  type Hierarchy
# do?ble -- 3.1,5.1..
# integers -- 1L, 2L
# logical -- True/False
# complex -- 1+3i
# character -- "hello"

typeof(c(TRUE,3L))

typeof(c(1,TRUE,mean))

x = c(1,TRUE,mean,4)
typeof(x[2])
typeof(x[4])
typeof(x[[4]])
sin(x[4])
sin(x[[4]])


x  = c(1,2,3)
typeof(x?1])
typeof(x[[1]])


# how are [[]] and [] diff?

dogs[4,3]
dogs[[4,3]]

dogs[4,] # [] can get many elements
dogs[[4,]] # [[]] always returns a single element

x = list(1, list("hi","hhh"),3)
x
x[2]
x[[2]]
typeof(x[[2]][1])
typeof(x[[2]][[1]])


dogs=readR?S("dogs_full.rds")
dim(dogs)

dogs[[4]]
head(dogs)

all(dogs[[4]] == dogs$popularity_all)

all(dogs[[3]] == dogs$datadog)
all.equal(dogs[[3]],dogs$datadog)

dogs[[3]] # GET column 3
dogs[[3]][[3]]
dogs[[3]][3]


# every object has a type         
# every o?ject has one or more classes 

typeof(dogs)
unclass(dogs)



class(dogs$breed)
# sapply
sapply(dogs,class)
sapply(dogs,mean,na.rm = TRUE)
mean(dogs$height,na.rm=TRUE)



# EXPLORATORY DATA ANALYSIS
# look for patterns -- make plots, compute summary statist?cs, fit models
# look for errors in the data
# look for relationships between variables
# look at data to get an overview
# check assumptions (models, conclusions........)


# univariate data
# break down by statistical data type
typeof(dogs$group)
class(d?gs$group)
table(dogs$group) # count()  dplyr
summary(dogs$group)
sort(table(dogs$group))

library(ggplot2)
ggplot(dogs, aes(x = group,fill = group)) + geom_bar()+
  scale_fill_manual(values = c("Herding"="#8FBC94", "hound"="lightskyblue","non-sporting"="da?korange","sporting"="darkorange3","terrier"="lightskyblue2","toy"="lightskyblue3","working"="lightskyblue3"))


ggplot(dogs,aes(x = group))+geom_point(stat="count") + coord_cartesian(ylim=c(0,30))

##########################################################?###########

str(dogs)
levels(dogs$size)
levels(dogs$size) = c("huge" ,"medium", "small")
levels(dogs$size)
tail(dogs$size)

levels(dogs$size)[1] = "large"
levels(dogs$size)

size_fix = factor(dogs$size,c("small" ,"medium", "large"))
levels(dogs$size)
leve?s(size_fix)

table(dogs$size)
table(size_fix)

library(ggplot2)
ggplot(dogs,aes(height,weight,color=size_fix))+geom_point()


#  missing value

table(dogs$size)

nrow(dogs)
sum(table(dogs$size))

sum(table(dogs$kids))  #but nrow(dogs) = 172
table(dogs$kids?useNA = "always")

# when you get a new data set, start with:

# 1. load the data
# 2. check structure -- nrow(), str(), head(), summary()
# 3. Explore missing values -- is.na()

is.na(dogs$kids)
shadow = is.na(dogs)
shadow
class(shadow)
class(dogs)
shadow?= as.data.frame(shadow) # back into a data frame
shadow

# now explore shadow data together with original data
# continuous features
fivenum(dogs$datadog)
summary(dogs$datadog)

ggplot(dogs, aes(y=datadog))+geom_boxplot()

ggplot(dogs, aes(x = datadog)) + ?eom_histogram() 
ggplot(dogs, aes(x = datadog)) + geom_histogram(bins = 3) 

ggplot(dogs, aes(x = datadog)) + geom_density()
ggplot(dogs, aes(x = datadog)) + geom_density(bw = 0.01)

# variance / standard deviation 
# - scaling factor
# - how far observati?ns are from the average , on average?
sd(dogs$datadog, na.rm = TRUE)
mu = mean(dogs$datadog, na.rm = TRUE)
min(dogs$datadog, na.rm = TRUE)
max(dogs$datadog, na.rm = TRUE)
m = median(dogs$datadog, na.rm = TRUE)

ggplot(dogs, aes(x = datadog)) + geom_density?) + geom_vline(aes(xintercept = m)) +
  geom_vline(aes(xintercept = mu), color = "red")

# geom_quantile()
# geom_vline() -- vertical
# geom_hline() -- horizontal
# geom_abline() -- any (straight) line

# quantile plots (useful for checking normality)
#


?

# Discrete Features
# we can treat discrete data like categorical data
table(dogs$ailments)
ggplot(dogs,aes(x = ailments)) + geom_bar()

mean(dogs$ailments,na.rm = TRUE)

# Is there a situation where one way is better than the other?
#
# time series -- m?y be better to treat as discrete?
# rankings -- again, discrete but neither way makes sense
# lots of "categories" -- use continuous methods
# few "categories" -- use categorical methods


#  Multivariate  EDA-----

# two categorical variables - frequencie?

tbl = table(size = dogs$size, group = dogs$group)
addmargins(tbl) # total proportions
prop.table(tbl,margin = 1) # 1 means rows   proportions row wise
prop.table(tbl,margin = 2) # proportions cloumn wise

ggplot(dogs, aes(size,fill = group)) + geom_bar(p?sition = "dodge")

props = prop.table(tbl)
props = as.data.frame(props)

ggplot(props, aes(size,fill = group, y =Freq)) +
  geom_bar(stat = "identity") + scale_fill_viridis_d()

# numerical, numerical --- scatter plot

ggplot(dogs, aes(height, weight)) + g?om_point()

ggplot(dogs, aes(height, weight)) + geom_density2d() + geom_point()  # geom_title()    geom_raster()

# numerical, categorical --  split into categories
ggplot(dogs, aes(x = size, y = height)) + geom_boxplot()
ggplot(dogs, aes( height, color = ?ize)) + geom_density()

# aggregation
aggregate(dogs$height - dogs$size,dogs,mean,na.rm = TRUE)





