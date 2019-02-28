dogs=readRDS("dogs_full.rds")

install.packages("ggplot2")
library(ggplot2)
# numerical vs catergorical

# how does height vary for different groups of dogs?
ggplot(dogs, aes(group,height)) + geom_boxplot() + geom_violin()

ggplot(dogs, aes(group,height)) ? geom_density(stat = "identity") 


library("ggridges")
ggplot(dogs,aes(x=height,y=group)) + geom_density_ridge()


# facets
# can we make the same plot (group vs height) with side-by-side plots?

# facet_grid
# facet_wrap

ggplot(dogs, aes(height)) + geom?density() + facet_wrap(~ group)

ggplot(dogs, aes(height, color = size)) + geom_density() + facet_wrap(~ group)

ggplot(dogs, aes(height,size)) + geom_density_ridges() + facet_wrap( ~ group)

# scale "free" to allow scales to vary
ggplot(dogs, aes(height,s?ze)) + geom_density_ridges() + facet_wrap(~ group, scale = "free")

ggplot(dogs, aes(height)) + geom_density() + facet_wrap( ~ size)

# bw parameter to change density smoothness
ggplot(dogs, aes(height)) + geom_density() + facet_grid( grooming ~ size)

ggp?ot(dogs, aes(height)) + geom_density() + facet_grid( size ~ .)

# when should we use facets versus aesthetics?

ggplot(dogs, aes(size)) + geom_bar() + facet_wrap(~ grooming)

# lots of information on the plot (too many lines, etc) --facets
# only 1-2 varia?les, probably better to use aesthetics
# 


# what to do with two continuous variabels?
#  -- usually a scatterplot
#  -- statistical test or model (check assumptions)
#  -- check correlation

ggplot(dogs, aes(height,weight)) + geom_point()

# add lifetime?cost to the plot . how can we do that?
ggplot(dogs, aes(height, weight, color = lifetime_cost, size = lifetime_cost)) + geom_point(alpha = 0.5)

ggplot(dogs, aes(height, lifetime_cost, color = weight, size = weight)) + geom_point(alpha = 0.5)

# Discretiza?ion
ggplot(dogs, aes(lifetime_cost)) + geom_histogram()

# cut()  cut_interval()    cut_number()    cut_width()

cost_d = cut_number(dogs$lifetime_cost, 5)
cost_d
class(cost_d)
table(cost_d)

summary(dogs$lifetime_cost)
cost_d2 = cut_width(dogs$lifetime_co?t,5000)
table(cost_2d)

?cut_interval

ggplot(dogs, aes(height, weight, color = cost_d, shape = cost_d)) + geom_point(alpha = 0.5)

# sorting 

x = c(1,3,4,2)
sort(x)

ord = order(x)
x[ord]

ord = order(dogs$popularity)
dogs[ord,]



options(CRAN = "http:/?cloud.r-project.org/")
#############
#############################  10/18
dogs=readRDS("dogs_full.rds")
library(ggplot2)
ggplot(dogs, aes(datadog,popularity)) + geom_point() + geom_text(aes(label = breed))

install.packages("ggrepel")
library(ggrepel)
ggpl?t(dogs, aes(datadog,popularity)) + geom_point() + geom_text_repel(aes(label = breed))

# overplotting
 mystery = readRDS("mystery.rds")
head(mystery)
ggplot(mystery, aes(x,y)) + geom_point(alpha = 0.25,size = 0.5)
ggplot(mystery, aes(x,y)) + geom_point() +?geom_density2d()


# Aspect Ratio
ggplot(mystery, aes(narrow,wide)) + geom_point() + coord_fixed(ratio = 1)

# statistical Pitfalls

head(anscombe)
cor(anscombe$x1,anscombe$y1)
ggplot(anscombe, aes(x1,y1)) + geom_point()

library(gridExtra)


# Simpson's P?radox
simpson = readRDS("simpson_c.rds")
head(simpson)
cor(simpson$weight, simpson$height)
by_group = split(simpson, simpson$group)
class(by_group)
class(by_group[[1]])
names(by_group)

cor(by_group$A$weight, by_group$A$height)

ggplot(simpson, aes(weight,?height, color = group)) + geom_point()

# the split- apply pattern
# aggregate() -- can split data up, and compute on each group
# split() gives us more control
##

# 1. split the data into group
# 2. apply/sappply to each group


by_group = split(simpson$?eight, simpson$group)
sapply(by_group, mean)

# t stands for table (think table() function)
tapply(simpson$height, simpson$group, mean)

index = match('A', names(by_group))
by_group = by_group[-index]

index = match(c('A','D'), names(by_group))
by_group = ?y_group[-index]
names(by_group)

by_group = split(simpson, simpson$group)
sapply(by_group, cor) # wrong

my_cor = function (x) {
  # x will be one group
  # x will look like a data frame in this case by_group[[1]]
  cor(x$weight, x$height)
}

sapply(by_gro?p, my_cor)









