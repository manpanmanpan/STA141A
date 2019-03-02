#a1  =readLines('messy_cl/messy/losangeles/_ant_apa_d_1bd-1ba-tennis-court_6738904358.txt')
#file = list.files('messy_cl/messy/sfbay',full.name = TRUE)
#file[[1]]
#readLines(file[[317]])
#desc = lapply(file,readLines)

library(stringr)
library(stringi)
lib?ary(lubridate)
# Q1
read_post = function(file){ # file is the path of a text
  text = readLines(file, encoding = 'UTF-8')
  return(text)
}
read_post = function(file,name){  #file is the name of sub-craigslist file, name is the name of each txt
  path = 'me?sy_cl/messy/'
  temp = paste(path,file,sep="")    
  readLines(paste(temp,name,sep="/"))
}
read_post(file = 'losangeles', name = "_ant_apa_d_1bd-1ba-tennis-court_6738904358.txt")

# Q2
# in order to get a data frame contains all posts, 
# I have to get all?columns information from the posts firstly.
# in the txt, the first line is the title
title = function(txt){
  temp = txt[[1]] # get the first line
  return(temp)
}

# in the txt, the last line is the apartments size, I split the strings by " : "
size = fu?ction(txt){
  temp1 = txt[[length(txt)]] # get the last line
  size = sub(".*:", "", temp1) # split the strings by " : "
  size = str_replace_all(size, fixed(" "), "") # remove whitespace
  return(size)
}

# in the txt, the next to last line is the apartme?ts bathroom, I split the strings by " : "
bathr = function(txt){
  temp1 = txt[[length(txt)-1]] # get the next to last line
  bathr = sub(".*:", "", temp1)
  bathr = str_replace_all(bathr, fixed(" "), "")
  return(bathr)
}

# in the txt, the third to last ?ine is the apartments bedroom, I split the strings by " : "
bedr = function(txt){
  temp1 = txt[[length(txt)-2]] # get the third to last line
  bedr = sub(".*:", "", temp1)
  bedr = str_replace_all(bedr, fixed(" "), "")
  return(bedr)
}

# in the txt, the ?th to last line is the apartments longitude, I split the strings by " : "
longi = function(txt){
  temp1 = txt[[length(txt)-3]] # get the 4th to last line
  longi = sub(".*:", "", temp1)
  longi = str_replace_all(longi, fixed(" "), "")
  return(longi)
}

#?in the txt, the 5th to last line is the apartments latitude, I split the strings by " : "
lati = function(txt){
  temp1 = txt[[length(txt)-4]] # get the 5th to last line
  lati = sub(".*:", "", temp1)
  lati = str_replace_all(lati, fixed(" "), "") 
  retur?(lati)
}

# in the txt, the 6th to last line is the apartments bathroom, I split the strings by " : "
price = function(txt){
  temp1 = txt[[length(txt)-5]] # get the 6th to last line
  price = sub(".*:", "", temp1)
  price = str_replace_all(price, fixed(" ?), "")
  price = strsplit(price,split='$', fixed=TRUE)[[1]][2] # remove "$"
  return(price)
}

# in the txt, the 7th to last line is the apartments date posts, I split the strings by " Date Posted: "
# note: there are two ":" in the string
Date_post = func?ion(txt){
  temp1 = txt[[length(txt)-6]]
  Date_post = sub("Date Posted:*", "", temp1)
  Date_post= mdy_hm(Date_post) # change form of time
  Date_post=str_extract(Date_post, "[^A-Z]+")
  return(Date_post)
}

# in the txt, the second line to the 8th to las? line is the apartments all information
text = function(txt){
  temp1 = txt[2:(length(txt)-7)] # get the list from 2nd to 8th to last line
  text = do.call(paste, c(as.list(temp1), sep = "\n")) # combind all lists 
  return(text)
}

# combine all column fu?ctions 
all_col = function(txt){
  title = title(txt)
  text = text(txt)
  latitude = lati(txt)
  longitude = longi(txt)
  date_posted = Date_post(txt)
  price = price(txt)
  sqft = size(txt)
  bedrooms = bedr(txt)
  bathrooms = bathr(txt)
  temp = c(title? text, latitude, longitude,
           date_posted, price, sqft, bedrooms, bathrooms)
  return(temp)
}

read_post = function(files){
  txt = readLines(files)
  return(txt)
}

read_all_posts = function(file_name){
  files = list.files(paste('messy_cl/messy/?,file_name,sep=""), full.names = TRUE)
  desc = sapply(files, read_post)
  all = sapply(desc, all_col)
  data_frame = as.data.frame(t(all))
  colnames(data_frame) = c('title', 'text', 'latitude', 'longitude',
                           'date_posted', 'pric?', 'sqft', 'bedrooms', 'bathrooms')
  rownames(data_frame) = NULL
  # change Rtypes
  data_frame$title = as.character(data_frame$title)
  data_frame$text = as.character(data_frame$text)
  data_frame$latitude = as.numeric(as.character(data_frame$latitude))
? data_frame$longitude = as.numeric(as.character(data_frame$longitude))
  data_frame$date_posted = as.character(data_frame$date_posted)
  data_frame$price = as.numeric(as.character(data_frame$price))
  data_frame$sqft = as.numeric(as.character(data_frame$sq?t))
  data_frame$bedrooms = as.numeric(as.character(data_frame$bedrooms))
  data_frame$bathrooms = as.numeric(as.character(data_frame$bathrooms))
  return(data_frame)
}

losangeles = read_all_posts('losangeles')
sacramento = read_all_posts('sacramento')
sa?diego = read_all_posts('sandiego')
sfbay = read_all_posts('sfbay')
sfbay_eby = read_all_posts('sfbay_eby')
sfbay_nby = read_all_posts('sfbay_nby')
sfbay_pen = read_all_posts('sfbay_pen')
sfbay_sby = read_all_posts('sfbay_sby')
sfbay_sfc = read_all_posts('s?bay_sfc')

craigslist = rep('losangeles',nrow(losangeles))
losangeles = cbind(losangeles,craigslist)
craigslist = rep('sacramento',nrow(sacramento))
sacramento = cbind(sacramento,craigslist)
craigslist = rep('sandiego',nrow(sandiego))
sandiego = cbind(sand?ego,craigslist)
craigslist = rep('sfbay',nrow(sfbay))
sfbay = cbind(sfbay,craigslist)
craigslist = rep('sfbay_eby',nrow(sfbay_eby))
sfbay_eby = cbind(sfbay_eby,craigslist)
craigslist = rep('sfbay_nby',nrow(sfbay_nby))
sfbay_nby = cbind(sfbay_nby,craigslist?
craigslist = rep('sfbay_pen',nrow(sfbay_pen))
sfbay_pen = cbind(sfbay_pen,craigslist)
craigslist = rep('sfbay_sby',nrow(sfbay_sby))
sfbay_sby = cbind(sfbay_sby,craigslist)
craigslist = rep('sfbay_sfc',nrow(sfbay_sfc))
sfbay_sfc = cbind(sfbay_sfc,craigslis?)

# combine all sub- Craigslist  posts  
final_data = rbind(losangeles, sacramento,sandiego,sfbay,sfbay_eby,
                   sfbay_nby,sfbay_pen,sfbay_sby,sfbay_sfc) 

# Q4
# get apartments price from title
final_data$title_price = rep(0,nrow(final_dat?))

for (i in (1:nrow(final_data))){
  temp1 = strsplit(final_data$title[i],split='/', fixed=TRUE)[[1]][1] # get the string in the front of /
  temp1 = str_trim(temp1) # get rid of space 
  temp2 = strsplit(temp1,split='$', fixed=TRUE)[[1]][2] # remove "$"?  temp2 = strsplit(temp2,split=' ', fixed=TRUE)[[1]][1] # get the first string
  final_data$title_price[i] = temp2
}
# get two columns from final_data 
temp1 = final_data[,c('price','title_price')]
temp1 = na.omit(temp1)
not_equal = temp1[!temp1$price==tem?1$title_price,]

# Q5
# get the posts that have deposit information 
# p  = final_data[grep(pattern = "Deposit",final_data[,2]),]

# get the money started with $, choose the max as deposit
final_data$deposit_2 = rep(0,nrow(final_data))
for (i in (1:nrow(fi?al_data))){
  temp1 = final_data$text[i]
  # extract lines have 'deposite' ,  ignore upper/lower character
  temp2 = str_remove_all(temp1, '\\.[0]+')
  temp2 = str_extract_all(temp2, regex('[\n.!-][^.\n!-]*deposit[^.\n!-]*[\n.!-]?',
                       ?               ignore_case = TRUE), simplify = T)
  temp3 = unlist(strsplit(temp2, '\\s'))
  temp4 = str_subset(temp3, "\\$[0-9]")
  temp5 = str_replace_all(temp4, fixed(","), "") # replace ,
  temp6 = str_extract(temp5, "\\$[0-9]+") # remove $ and -
  tem?6 = str_extract(temp6,"[0-9]+")
  if (identical(temp6,character(0))) {
    temp6 = NA
  } 
  else{temp6 = max(as.numeric(temp6), na.rm = T)}
  final_data$deposit_2[i] = temp6
}
# replace deposit from 1 to 95 to NA (those are pet deposit)
final_data$deposit?2[final_data$deposit_2 > 0 & final_data$deposit_2 < 95] = NA

# get no deposit = 0, one month deposit = rent price
final_data$deposit = rep(0,nrow(final_data))
for (i in (1:nrow(final_data))){
  temp1 = final_data$text[i]
  # extract lines have 'deposite' ?  ignore upper/lower character
  temp2 = str_extract_all(temp1, regex('[\n.!-][^.\n!-]*deposit[^.\n!-]*[\n.!-]?',
                                       ignore_case = TRUE), simplify = T)
  temp3 = str_match(temp2,'(one month rent|1 month rent|One Month Re?t|1 Month Rent)')[,1][1]
  temp4 = str_match(temp2,'(no deposit|zero deposit|not deposit|Zero Deposit|Not Deposit|No Deposit)')[,1][1]
  
  if (!is.na(temp4) ) { temp5 = 0}
  else if (!is.na(temp3)){temp5 = final_data$price[i]}
  else{ temp5 = NA }
  final?data$deposit[i] = temp5
}

# combine two deposit columns 
for (i in (1:nrow(final_data))){
  if (is.na(final_data$deposit[i])){final_data$deposit[i] = final_data$deposit_2[i]}
}

# correct some errors
final_data$deposit[23765] = 2900
final_data$deposit[237?6] = 2995
final_data$deposit[37857] = 2350
final_data$deposit[20699] = 3000
final_data = subset( final_data, select = -c(deposit_2) )
saveRDS(final_data, "final_data.rds")

# try one text
#t1 = final_data$text[42151]
# extract lines have 'deposite' ,  igno?e upper/lower character
#t1 = str_remove_all(t1, '\\.[0]+')
#t1 = str_extract_all(t1, regex('[\n.!-][^.\n!-]*deposit[^.\n!-]*[\n.!-]?',
#                               ignore_case = TRUE), simplify = T)
#t1 = unlist(strsplit(t1, '\\s'))
#t1 = str_subset(t1? "\\$[0-9]")
#t1 = str_replace_all(t1, fixed(","), "") # replace ,
#t1 = str_extract(t1, "\\$[0-9]+") # remove $ and -
#t1 = str_extract(t1,"[0-9]+")
#if (identical(t1,character(0))) {
#  t1 = NA
#} else {t1 = max(as.numeric(t1),na.rm = T)}

# relation of ?ental price and deposit
final_data = readRDS('final_data.rds')
# correct errors and limit outliers
final_data$price[final_data$price >= 30000000] = 3408
final_data$price[final_data$price >= 9900000] = 995
final_data$price[final_data$price < 200 ] = NA
fina?_data$price[final_data$price > 20000 ] = NA
final_data$deposit[final_data$deposit > 15000 ] = NA

library(ggplot2)
ggplot(final_data,aes(y = price)) + geom_boxplot() 
# overal plot
data_1 = final_data[,c('price','deposit')]
data_1 = na.omit(data_1)
ggplot(?ata_1, aes(deposit,price)) + geom_point() + geom_smooth(method='loess') +
  labs(title="Relationship of Apartments Deposit and Rental Price",x="Apartments Deposit",y="Apartments Rental Price")+ 
  theme(plot.title=element_text(hjust=0.5,size = 18)) 

# plo? by deposit quartile
data_1$deposit_quartile = rep(0, nrow(data_1))
data_1$deposit_quartile = as.integer(cut(data_1$deposit, quantile(data_1$deposit, probs=0:4/4), include.lowest=TRUE, na.rm = TRUE))
data_1$deposit_quartile = as.factor(data_1$deposit_quart?le)
# 0   500   700  1700 15000 

ggplot(data_1, aes(deposit,price)) + geom_point() + geom_smooth(method='loess') +
  labs(title="Relationship of Apartments Deposit and Rental Price by Deposit Quartile",x="Apartments Deposit",y="Apartments Rental Price")+ ?  theme(plot.title=element_text(hjust=0.5,size = 18)) + facet_wrap(~deposit_quartile, scales = "free")

# Q6 pets
data_3 = final_data[,c('title','text')]
# combine title and text 
data_3$comb = rep(0,nrow(data_3))
for (i in (1:nrow(data_3))){
  data_3$comb?i] =paste(data_3$title[i],data_3$text[i], sep = "\n")
}

get_pets = function(comb){
  # extract both pet (cats and dogs)
  temp1_1 = str_detect(comb, regex('welcome (cats? (and|&) dogs?|dogs? (and|&) cats?)', ignore_case = T))
  temp1_2 = str_detect(comb, ?egex('(cats? (and|&) dogs?|dogs? (and|&) cats?)[ ]?(welcome|allowed|accepted|-friendly|friendly|ok|okay|are ok|only|accepts)', ignore_case = T))
  temp1_3 = str_detect(comb, regex('pets?[ ]?(welcome|allowed|accepted|-friendly|friendly|ok|okay|are ok|genera?ly accepted|accepts|rent|deposit|upon approval)', ignore_case = T))
  temp1_4 = str_detect(comb, regex('(welcome|love|accept) (pets?|(cats? (and|&) dogs?|dogs? (and|&) cats?))', ignore_case = T))
  temp1_5 = str_detect(comb, regex('accept (cats? (and|&) do?s?|dogs? (and|&) cats?)', ignore_case = T))
  temp1_6 = str_detect(comb, regex('allow (cats? (and|&) dogs?|dogs? (and|&) cats?)', ignore_case = T))
  temp1_7 = str_detect(comb, regex('cats? (and|&) small dogs? friendly', ignore_case = T))
  temp1_8 = str_d?tect(comb, regex('small caged animals', ignore_case = T))
  # extract dogs only and cats only
  temp2_1 = str_detect(comb, regex('no dogs?|dogs? not allowed', ignore_case = T))
  temp2_2 = str_detect(comb, regex('dogs? (welcome|allowed|accepted|-friendly|f?iendly|ok|okay|are ok|only|accepts|deposit)', ignore_case = T))
  temp3_1 = str_detect(comb, regex('no cats?|cats? not allowed', ignore_case = T))
  temp3_2 = str_detect(comb, regex('cats? (welcome|allowed|accepted|-friendly|friendly|ok|okay|are ok|only|ac?epts|deposit)', ignore_case = T))
  # extract no pets
  temp4 = str_detect(comb, regex('no (pets?|animals?)|no (dogs? or cats?|cats? or dogs?) allowed', ignore_case = T))
  
  if(temp4){temp5 = 'none'}
  else if(temp1_1){temp5 = 'both'}
  else if(temp1_2){?emp5 = 'both'}
  else if(temp1_3){temp5 = 'both'}
  else if(temp1_4){temp5 = 'both'}
  else if(temp1_5){temp5 = 'both'}
  else if(temp1_6){temp5 = 'both'}
  else if(temp1_7){temp5 = 'both'}
  else if(temp1_8){temp5 = 'both'}
  
  else if(temp2_1 & temp3_1)?temp5 = 'none'}
  else if(temp2_1 & temp3_2){temp5 = 'cat'}
  else if(temp2_2 & temp3_1){temp5 = 'dog'}
  else if(temp2_2 & temp3_2){temp5 = 'both'}
  
  else if(temp2_2){temp5 = 'dog'}
  else if(temp3_2){temp5 = 'cat'}
  
  else {temp5 = NA}
  return(temp?)
}
data_3$pet = rep(0,nrow(data_3))
data_3$pet = sapply(data_3$comb, get_pets)

length(which(data_3$pet == 'both')) # 16357
length(which(data_3$pet == 'none')) # 6310
length(which(data_3$pet == 'dog')) # 188
length(which(data_3$pet == 'cat')) # 1176

###p?t deposit
data_3$pet_deposit = rep(0,nrow(data_3))
for (i in (1:nrow(data_3))){
  temp1 = final_data$text[i]
  # extract lines have 'deposite' ,  ignore upper/lower character
  temp2 = str_remove_all(temp1, '\\.[0]+')
  temp2 = str_extract_all(temp2, regex?'[\n.!-][^.\n!-]*(pets?|cats?|dogs?) deposit[^.\n!-]*[\n.!-]?',
                                       ignore_case = TRUE), simplify = T)
  temp3 = unlist(strsplit(temp2, '\\s'))
  temp4 = str_subset(temp3, "\\$[0-9]")
  temp5 = str_replace_all(temp4, fixe?(","), "") # replace ,
  temp6 = str_extract(temp5, "\\$[0-9]+") # remove $ and -
  temp6 = str_extract(temp6,"[0-9]+")
  if (identical(temp6,character(0))) {
    temp6 = NA
  } 
  else{temp6 = max(as.numeric(temp6), na.rm = T)}
  data_3$pet_deposit[i] = t?mp6
}

data_3$pet_deposit[data_3$pet_deposit > 1200] = NA
sum(!is.na(data_3$pet_deposit))  # 1852

library(ggplot2)
ggplot(data = data_3, aes(x=pet_deposit))+
  geom_density( alpha = 0.2,fill="#56B4E9",size = 0.8) +
  # Change the fill colour to differenti?te it
  labs(title = "Distribution of Pet Deposits") + theme(plot.title = element_text(hjust = 0.5,size = 15))+
  labs(y="")+labs(x="Pet Deposit") 

##### other pets
data_3$other_pet = rep(0,nrow(data_3))
for (i in (1:nrow(data_3))){
  temp1 = data_3$text[?]
  temp2 = str_extract_all(temp1, regex('[\n.!-][^.\n!-]*small caged animals[^.\n!-]*[\n.!-]?',
                                       ignore_case = TRUE), simplify = T)[1,]
  if (identical(temp2,character(0))) {
    temp3 = NA
  } 
  else{temp3 = temp2}
? data_3$other_pet[i] = temp3
}
#Small caged animals that are allowed are common domesticated household birds, hamsters, gerbils, rabbits, guinea pigs, 
#chinchillas and aquarium/terrarium animals including fish, hermit crabs, turtles, frogs, and small liza?ds.  
#Animals such as birds of prey, iguanas, ferrets, snakes, rats, mice, insects, arachnids (including spiders and scorpions), 
#livestock, or any other exotic animals are not allowed. 

# Q7
# find air conditioning
final_data$Air_conditioning = rep(0,n?ow(final_data))
for (i in (1:nrow(final_data))){
  temp1 = final_data$text[i]
  temp2 = str_detect(temp1, regex('air conditioning| A/C| air| air conditioner', ignore_case = TRUE))
  if (temp2 ) { temp3 = 'Yes'}
  else {temp3 = 'None'}
  final_data$Air_cond?tioning[i] = temp3
}

# find heating
data_2 = final_data[,c('text','Air_conditioning')]
data_2$Heat = rep(0,nrow(data_2))
for (i in (1:nrow(data_2))){
  temp1 = data_2$text[i]
  temp2 = str_detect(temp1, regex(' Heating| Heat| Heater', ignore_case = TRUE))?  if (temp2 ) { temp3 = 1}
  else {temp3 = 0}
  data_2$Heat[i] = temp3
}

# find fireplace
data_2$Fireplace = rep(0,nrow(data_2))
for (i in (1:nrow(data_2))){
  temp1 = data_2$text[i]
  temp2 = str_detect(temp1, regex('Fireplaces?| fire places?| burning', ?gnore_case = TRUE))
  if (temp2 ) { temp3 = 1}
  else {temp3 = 0}
  data_2$Fireplace[i] = temp3
}

length(which(data_2$Air_conditioning == 'Yes')) # air conditioner 14203

data_2$Heat_Fireplace = rep(0,nrow(data_2))
for (i in (1:nrow(data_2))){
  if(data_2?Heat[i] == 1 & data_2$Fireplace[i] == 1){data_2$Heat_Fireplace[i]='both'}
  else if (data_2$Heat[i] == 1 & data_2$Fireplace[i] == 0){data_2$Heat_Fireplace[i]='Heat'}
  else if (data_2$Heat[i] == 0 & data_2$Fireplace[i] == 1){data_2$Heat_Fireplace[i]='Firep?ace'}
  else {data_2$Heat_Fireplace[i]='None'}
}
length(which(data_2$Heat_Fireplace == 'both')) # 2844
length(which(data_2$Heat_Fireplace == 'Fireplace')) # 4728
length(which(data_2$Heat_Fireplace == 'Heat')) # 8878
length(which(data_2$Heat_Fireplace == 'N?ne'))  # 29395
# fireplace & heat  16450

fire_heat = data_2[data_2[,'Heat_Fireplace']%in%c('both','Fireplace','Heat'),] #16766
length(which(fire_heat$Air_conditioning == 'Yes')) # 9139

Air_con = data_2[data_2$Air_conditioning =='Yes',] # 14203
length(whi?h(Air_con$Heat_Fireplace == 'both')) # 1925
length(which(Air_con$Heat_Fireplace == 'Fireplace')) # 1510
length(which(Air_con$Heat_Fireplace == 'Heat')) # 5704
length(which(Air_con$Heat_Fireplace == 'None'))  # 5064

# Q8
# find email
data_2$Email = rep(0,n?ow(data_2))
get_email = function(txt){
  temp1 = str_extract(txt,"[A-z0-9-]+\\@[A-z0-9-]+\\.[A-z0-9-]{0,5}")
  if (is.na(temp1)){temp2 = NA}
  else {temp2 = 'Email'}
  return(temp2)
}
data_2$Email = rep(0,nrow(data_2))
data_2$Email = sapply(data_2$text,get?email)
length(which(data_2$Email == 'Email')) # 73

# find phone
# str_extract(temp1,"\\b1?-?\\(?[0-9]{3}\\)?-?[0-9]{3}-?[0-9]{4}\\b")
# extract phone number like (123) 456 6789
data_2$Phone = rep(0,nrow(data_2))
for (i in (1:nrow(data_2))){
  temp1 = data?2$text[i]
  # extract email
  temp2 = str_match(temp1, regex("\\((\\d{3})\\)\\s(\\d{3})", comments = TRUE))[,1]
  if (is.na(temp2)){data_2$Phone[i] = NA}
  else {data_2$Phone[i] = 'phone'}
}
length(which(data_2$Phone == 'phone')) # 68

#extract phone numbe? like 123-456-7890
data_2$Phone2 = rep(0,nrow(data_2))
for (i in (1:nrow(data_2))){
  temp1 = data_2$text[i]
  # extract email
  temp2 = str_match(temp1, regex("(\\d{3})\\-(\\d{3})\\-(\\d{3,4})", comments = TRUE))[,1]
  if (is.na(temp2)){data_2$Phone2[i] =?NA}
  else {data_2$Phone2[i] = 'phone'}
}
length(which(data_2$Phone2 == 'phone')) # 34
# extract phone number 99

# regex("\\(?     # optional opening parens
#               (\\d{3}) # area code
#               [)- ]?   # optional closing parens, dash, or ?pace
#               (\\d{3}) # another three numbers
#               [ -]?    # optional space or dash
#               (\\d{4}) # four more numbers
#               ", comments = TRUE)

# find hide information posts
data_2$contact = rep(0,nrow(data_2))
for?(i in (1:nrow(data_2))){
  temp1 = data_2$text[i]
  temp2 = str_detect(temp1, regex('show contact info', ignore_case = TRUE))
  if (temp2 ) { data_2$contact[i] = 'hide'}
  else if(str_detect(temp1, regex(' email| text| phone| call| contact', ignore_case = ?RUE))) {data_2$contact[i] = 'not hide'}
  else {data_2$contact[i] = NA}
}
length(which(data_2$contact == 'hide'))  #34832 
length(which(data_2$contact == 'not hide'))  # 4140 

# combine email , phone numbers
for (i in (1:nrow(data_2))){
  if (is.na(data_2?Phone[i])){data_2$Phone[i] = data_2$Phone2[i]}
}
for (i in (1:nrow(data_2))){
  if (is.na(data_2$Phone[i])){data_2$Phone[i] = data_2$Email[i]}
}