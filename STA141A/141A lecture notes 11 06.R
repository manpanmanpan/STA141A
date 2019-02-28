##########  11/6
text = c('y','x','zx','xy','a')
match(c('x','y','q'),text,nomatch = 0)

dogs = readRDS('dogs_top.rds')
head(dogs)
index = match('Beagle',dogs$breed)
dogs[index,]

index = match(5,dogs$popularity_all)

my_data = data.frame(x= c(1,1,1,2),y=c?1,1,1,2))
library(ggplot2)
ggplot(my_data,aes(x,y)) + geom_point() # points overlap

ggplot(my_data,aes(x,y)) + geom_jitter(height = .01,width = .01)

# taking subset
# fours ways
dogs$breed
# subset() to get rows
subset(dogs, weight >= 10 & popularity_all?> 5)
subset(dogs, weight >= 10 | popularity_all > 5)
# [[]]
dogs[[1]]

# only get one column
dogs[['breed']]
dogs[[c('breed','group')]] # this is wrong

# []
dogs[1] # with index
dogs[1,3] # keep class
dogs[[1,3]] # drop class

is_heavy = dogs$weight >= 10?length(is_heavy)
dogs[is_heavy,]
which(is_heavy)
dogs[which(is_heavy),]

?readLines
?list.files

STA = readLines('catalog/catalog/STA.txt')
class(STA)
head(STA)

con = file('catalog/catalog/STA.txt','rt')
readLines(con)
close(con)
# 
# how do we get all of?the course description?
# how do we find out the names of all of the text files?

file = list.files('catalog/catalog/')
file[[1]]

file = list.files('catalog/catalog/',full.name = TRUE)
file[[1]]
readLines(file[[1]])

# we want to use readLines() with ever? file path in files
# use sapply()
desc = sapply(file, readLines)
length(desc)
class(desc)
class(desc[[1]])
head(desc[[1]])
head(desc[[length(desc)]])

file

# loops
# a loop is a programming tool or function or command that lets you do something over and ?ver
# fours ways to write loops
#1.  lapply()  sapply() apply() tapply()

sapply(c(1,2,3),sin)
class(sapply(c(1,2,3),sin)) # s stands for simplify

lapply(c(1,2,3),sin) # outputs a list
class(lapply(c(1,2,3),sin)) # l stands for list

# apply function appl?es over rows or columns(or other dimension)
apply(dogs[c('weight','height')], 1, mean,na.rm = T) # rows

# tapply() is similar to aggregate() ;
# split() followed by sapply()

tapply(dogs$height, dogs$group,mean, na.rm = T)

###############################?#########  11/08
# 2. vectorization
sin(c(1,2,3)) # good
dogs = readRDS('dogs_full.rds')
class(dogs) # class() is not vectorized
sapply(dogs, class) # so resonable to use class() with sapply()

# vectorization is much faster than any other kind of loop

# ?hen thinging about writing a loop, try:
# 1 vectorization
# 2 apply function
# 3 for while loops

# 3. for -loops and  while - loops
sapply(c(1,2,3),sin)

for (i in c(1,2,3)){
  message(i)
}
sapply(c(1,2,3,4),message)
message('1')
print('1')

# in this exa?ple, we ask for more memory we need at beginning
# vary inefficient
x = c()
for (i in c(1,2,3)){
  x = c(x, sin(i))
}
x

# Drawbacks for loops:
# 1. we have to explicitly save the result somehow
# 2. We have to remember to preallocate
# 3. we have to get t?e iteration number with seq_along()


# Instead we want to ask for all memory we need at beginning
# This is called 'preallocating'

x = numeric(4) # integer()  character() logical()
to_compute = c(7,5,1,3)
for (i in c(1,2,3,4)) {
  x[i] = sin(to_compute[i?)
}
# x[1] = sin(to_compute[1]) # to_compute[1] is 7
# x[2] = sin(to_compute[2]) # to_compute[2] is 5


x = numeric(4) # integer()  character() logical()
to_compute = c(7,5,1,3)
for (i in seq_along(to_compute)) {
  x[i] = sin(to_compute[i])
}

# when would?we use a for-loop
# we one interation depends on another
to_compute = c(7,5,2,3)
x = 0
for (i in seq_along(to_compute)){
  x = x + to_compute[i]
  if (x > 12){
    x = 0
  }
}
# x = 0 + 7
# x = 7 + 5
# x = 12 + 2
#####

# while-loops are useful if you don'? know how many times you need to iterate
x = 0
i = 1
while(x < 10){
  x = x + rnorm(1)
  i = i +1
}

message('Hello from STA 141A')
message('Hello from STA 141B')
message('Hello from STA 141C')
message('Hello from STA 13')

num = c('141A','141B','141C','13?)
sapply(num, function(m){
  message('Hello from STA',m)
})

sapply(num,message)
message(num[[1]])
message(num[[2]])


# 4.  Recursion
f = function(x){
  message(x[1])
  if (length(x) > 1)
    f(x[-1])
}
f(num)

# in R, recursion is usually the slowest way?to write a loop
# But some problems are naturally recursive, so for those problems, it may make sense to use recursion

# Fibonacci numbers
# 1 1 2 3 5 8 13
#
# Fibonacci numbers are naturally recursive, but even this can be computed faster using other kin?s of loops


summary(list(1:3,5:6))
1:3 + 4:6


sta = readLines('catalog/catalog/STA.txt')

# how do we lead all of hte course description
files = list.files('catalog/catalog', full.names = TRUE)
desc = lapply(files,readLines)

# we can use a loop to load ?ll the files
# we can use a loop to process all the files
# But let's focus on one iteration and make sure er get it right
# what do we actually want to do to this file
#* three letter code   eg STA
#* course number
#* course title
#* number pf credits
#* ?umber of hours
#* prerequisites
#* Description


length(sta)

# we want to use a loop to process each description
desc = sta[[1]]
library(stringr)
str_sub(desc, 1,3) # the fist three character
str_sub(sta, 1,3)

# stringr is vectorized
str_split('dogs / ca?s', '/')
str_split('dogs & cats / parrot','&|/')
str_split('dogs &/ cats // parrot', '[/&]/')

# remember fixed() turns off regular expression
x = c('dogs & cats & parrots','ig & fish')
str_split_fixed(x,'&',2)[,1]










