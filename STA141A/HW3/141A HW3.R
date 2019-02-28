apartments = readRDS("cl_apartments.rds")
nrow(apartments)
ncol(apartments)
levels(apartments$place)
levels(apartments$craigslist)
levels(apartments$county)
levels(apartments$city)
levels(as.factor(apartments$state))

# time span
apartments$date_posted = a?.character(apartments$date_posted)
time = NULL
for (i in (1:21948)){
  temp = strsplit(apartments$date_posted, " ")[[i]][1]
  temp = as.data.frame(temp)
  time = rbind(time,temp)
  time = unique(time)
}
time

library(ggplot2)
library(gridExtra)

# Def majo? cities: Los Angeles, San Diego, San Jose, San Francisco, Fresno,Sacramento,Long Beach,Oakland 
# the population are large than 400000
city_type = rep(0, nrow(apartments))
apartments = cbind(apartments, city_type)
apartments = na.omit(apartments)

for (i i? 1:nrow(apartments)){
  apartments$city_type[i] = "suburbs"
  for (j in c("Los Angeles","San Diego","San Jose","San Francisco",
              "Fresno","Sacramento","Long Beach","Oakland")){
    if (apartments$city[i] == j) {apartments$city_type[i] = "major?city"}
  }
}

table(apartments$city_type)

# city type VS famliy friendly
h1 = ggplot(apartments, aes(x = as.factor(bedrooms) , fill = city_type)) + 
  geom_bar(position = "dodge")+labs(title = "Number of Bedrooms by City Type", x = "Number of Bedrooms") +?  theme(axis.ticks.x = element_blank())+theme(plot.title=element_text(hjust=0.5))+
  scale_fill_manual(values = c("darkorange","lightskyblue2"))+
  theme(legend.background = element_blank(),legend.justification=c(1,1), legend.position=c(1, 1))+
  geom_text?stat='count', aes(label=..count..), vjust=-0.2,position = position_dodge(width = 1))

h2 = ggplot(apartments, aes(x = as.factor(pets) , fill = city_type)) + 
  geom_bar(position = "dodge")+labs(title = "Pets Allowed by City Type", x = "Pets Allowed") +
  t?eme(axis.ticks.x = element_blank())+theme(plot.title=element_text(hjust=0.5))+
  scale_fill_manual(values = c("darkorange","lightskyblue2"))+
  theme(legend.background = element_blank(),legend.justification=c(1,1), legend.position=c(1, 1))+
  geom_text(sta?='count', aes(label=..count..), vjust=-0.2,position = position_dodge(width = 1))

h3 = ggplot(apartments, aes(x = as.factor(parking) , fill = city_type)) + 
  geom_bar(position = "dodge")+labs(title = "Parking Policy by City Type", x = "Parking Policy") +
? theme(axis.ticks.x = element_blank())+theme(plot.title=element_text(hjust=0.5))+
  scale_fill_manual(values = c("darkorange","lightskyblue2"))+
  theme(legend.background = element_blank(),legend.justification=c(1,1), legend.position=c(1, 1))+
  geom_text(?tat='count', aes(label=..count..), vjust=-0.2, position = position_dodge(width = 1))


h4 = ggplot(apartments, aes(x = as.factor(laundry) , fill = city_type)) + 
  geom_bar(position = "dodge")+labs(title = "Laundry Situation by City Type", x = "Laundry Sit?ation") +
  theme(axis.ticks.x = element_blank())+theme(plot.title=element_text(hjust=0.5))+
  scale_fill_manual(values = c("darkorange","lightskyblue2"))+
  theme(legend.background = element_blank(),legend.justification=c(1,1), legend.position=c(1, 1))+
 ?geom_text(stat='count', aes(label=..count..), vjust=-0.2, position = position_dodge(width = 1))

grid.arrange(h1,h2,h3,h4,nrow = 2)

# rent VS beadrooms/bathrooms
h5 = ggplot(apartments, aes(x = bedrooms, y = price)) + geom_point(size = 3,color = 'tomato1'?+
  labs(title = "Relation of The Number of Bedrooms and Apartments Rent", x= "The Number of Bedrooms")+
  theme(plot.title=element_text(hjust=0.5))
h6 = ggplot(apartments, aes(x = bathrooms, y = price)) + geom_point(size = 3.5,color = 'lightslateblue')+
 ?labs(title = "Relation of The Number of bedrooms and Apartments Rent", x = "The Number of Bathrooms",y="")+
  theme(plot.title=element_text(hjust=0.5))
grid.arrange(h5,h6,nrow = 1)

# LA, San Diego, San Jose
LA_SD_SJ = apartments[apartments[,"city"] %in% c?"Los Angeles", "San Diego", "San Jose"),]

h7 = ggplot(LA_SD_SJ, aes(x = as.factor(bedrooms), fill = city)) + 
  geom_bar( position = "dodge")+labs(title = "Number of Bedrooms by City Type", x = "Number of Bedrooms") +
  theme(axis.ticks.x = element_blank(?)+theme(plot.title=element_text(hjust=0.5))+
  scale_fill_manual(values = c("khaki","lightskyblue2","mediumaquamarine"))+
  theme(legend.background = element_blank(),legend.justification=c(1,1), legend.position=c(1, 1))+
  geom_text(stat='count', aes(label?..count..), vjust=0.5, position = position_dodge(width = 1))

h8 = ggplot(LA_SD_SJ, aes(x = as.factor(bathrooms), fill = city)) + 
  geom_bar( position = "dodge")+labs(title = "Number of Bathrooms by City Type", x = "Number of Bathrooms") +
  theme(axis.ti?ks.x = element_blank())+theme(plot.title=element_text(hjust=0.5))+
  scale_fill_manual(values = c("khaki","lightskyblue2","mediumaquamarine"))+
  theme(legend.background = element_blank(),legend.justification=c(1,1), legend.position=c(1, 1))+
  geom_text(s?at='count', aes(label=..count..), vjust=0.5, position = position_dodge(width = 1))

h9 = ggplot(LA_SD_SJ, aes(x = as.factor(parking), fill = city)) + 
  geom_bar( position = "dodge")+labs(title = "Parking Policy by City Type", x = "Parking Policy") +
  the?e(axis.ticks.x = element_blank())+theme(plot.title=element_text(hjust=0.5))+
  scale_fill_manual(values = c("khaki","lightskyblue2","mediumaquamarine"))+
  theme(legend.background = element_blank(),legend.justification=c(1,1), legend.position=c(1, 1))+
  g?om_text(stat='count', aes(label=..count..), vjust=0.5, position = position_dodge(width = 1))

h10 = ggplot(LA_SD_SJ, aes(x = as.factor(laundry), fill = city)) + 
  geom_bar( position = "dodge")+labs(title = "Laundry Situation by City Type", x = "Laundry Si?uation") +
  theme(axis.ticks.x = element_blank())+theme(plot.title=element_text(hjust=0.5))+
  scale_fill_manual(values = c("khaki","lightskyblue2","mediumaquamarine"))+
  theme(legend.background = element_blank(),legend.justification=c(0,1), legend.posit?on=c(0, 1))+
  geom_text(stat='count', aes(label=..count..), vjust=0.5, position = position_dodge(width = 1))

h11 = ggplot(LA_SD_SJ, aes(x = as.factor(pets), fill = city)) + 
  geom_bar( position = "dodge")+labs(title = "Pets Allowed by City Type", x = "P?ts Allowed") +
  theme(axis.ticks.x = element_blank())+theme(plot.title=element_text(hjust=0.5))+
  scale_fill_manual(values = c("khaki","lightskyblue2","mediumaquamarine"))+
  theme(legend.background = element_blank(),legend.justification=c(1,1), legend.p?sition=c(1, 1))+
  geom_text(stat='count', aes(label=..count..), vjust=0.5, position = position_dodge(width = 1))

h12 = ggplot(LA_SD_SJ, aes(x=city, y=price, fill=city)) +
  geom_violin(alpha = 0.4,size = 0.8) +geom_boxplot(width=0.3, fill="white",size = ?.8)+
  labs(title = "Apartments Rent by City Type", x = "", y = "Apartments   Rent") + 
  scale_fill_manual(values = c("khaki","lightskyblue2","mediumaquamarine"))+
  stat_summary(geom="text", fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..?),
               position=position_nudge(x=0.23), size=5.5)+theme(plot.title=element_text(hjust=0.5))+
  theme(legend.background = element_blank(),legend.justification=c(1,1), legend.position=c(1, 1))

h13 = ggplot(LA_SD_SJ, aes(x=city, y=sqft, fill=city)? +
  geom_violin(alpha = 0.4,size = 0.8) +geom_boxplot(width=0.3, fill="white",size = 0.8)+
  labs(title = "Apartments Size by City Type", x = "", y = "Apartments   Size") + 
  scale_fill_manual(values = c("khaki","lightskyblue2","mediumaquamarine"))+
  st?t_summary(geom="text", fun.y=quantile,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.23), size=5.5)+theme(plot.title=element_text(hjust=0.5))+
  theme(legend.background = element_blank(),legend.justification=?(1,1), legend.position=c(1, 1))

grid.arrange(h7,h8,h9,h11,h10)
grid.arrange(h12,h13,nrow = 1)

### CA map
library(maps)
us_states = map_data ("state")
ca_df = subset(us_states, region == "california")
counties = map_data("county")
ca_county = subset(count?es, region == "california")

ca_base = ggplot(ca_df, mapping = aes(x = long, y = lat, group = group)) + coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")

# category three region 
Three_Region = rep(0, nrow(ca_county))
ca_county = cbind(ca_?ounty, Three_Region)

for (i in 1:nrow(ca_county)){
  ca_county$Three_Region[i] = "Northern California"
  for (j in c("mono","madera","fresno","kings","tulare",
              "inyo","kern","san bernardino","riverside","orange","san diego","imperial")){
   ?if (ca_county$subregion[i] == j) {ca_county$Three_Region[i] = "Southern California"}
  }
  for (k in (c("monterey","san benito","san luis obispo","santa barbara","ventura","los angeles"))){
    if (ca_county$subregion[i] == k) {ca_county$Three_Region[i] = ?California"}
  }
}

p = ca_base +  geom_polygon(data = ca_county, aes(fill = Three_Region), color = "white") +
  geom_polygon(color = "black", fill = NA) + theme(axis.text = element_blank(),axis.line = element_blank(),
    axis.ticks = element_blank(),rect?= element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank())+
  scale_fill_manual(values = c("California"="dodgerblue4",
                               "Northern California"= "skyblue3",
                               "Southern Califor?ia"="lightskyblue1"))
# add points 
apartments = na.omit(apartments)
h1 = p + geom_point(data = apartments, aes(x =longitude, y = latitude , color = price ),inherit.aes = FALSE)+
  labs(title = "Apartments Rent by Region")+theme(plot.title=element_text(hju?t=0.5,size = 15))+
  theme(legend.background = element_blank(),legend.justification=c(0,0), legend.position=c(0, 0))+
  scale_color_gradient(low="#E1FA72", high="#F46FEE")

h2 = p + geom_point(data = apartments, aes(x =longitude, y = latitude , color = sqf? ),alpha = 0.1,inherit.aes = FALSE)+
  labs(title = "Apartments Size by Region")+theme(plot.title=element_text(hjust=0.5,size = 20))+
  theme(legend.background = element_blank(),legend.justification=c(0,0), legend.position=c(0, 0))+
  scale_color_gradient(?ow="#E1FA72", high="#F46FEE")

grid.arrange(h1,h2,nrow = 1)
h1
##### Equally divide into 10 groups by rent
group_rent = rep(0, nrow(apartments))
apartments = cbind(apartments, group_rent)
breaks = quantile(apartments$price, probs = seq(0, 1, 0.1), name = F?LSE)

for(i in 1:nrow(apartments)){
  for(j in 1:10){
    if(apartments$price[i] >= breaks[j] & apartments$price[i] <= breaks[j+1]){apartments$group[i] = j}
  }
}

rent = apartments[apartments[,"group"] %in% c("10","1"),]

table(rent_top_ten$bedrooms)
tabl?(rent_top_ten$bathrooms)
table(rent_top_ten$parking)
table(rent_top_ten$pets)
table(rent_top_ten$laundry)
table(rent_top_ten$city)
table(rent_top_ten$county)

# define top 10th rent apartments as high rent house, low 10th rent as low rent house

p1 = ggplo?(rent, aes(x = as.factor(pets), fill = as.factor(group))) + 
  geom_bar( position = "dodge")+labs(title = "Pets Allowed by Apartments Rent", x = "") +
  theme(axis.ticks.x = element_blank())+theme(plot.title=element_text(hjust=0.5))+
  theme(legend.backgro?nd = element_blank(),legend.justification=c(1,1), legend.position=c(1, 1))+
  geom_text(stat='count', aes(label=..count..), vjust=0.5, position = position_dodge(width = 1))+
  scale_fill_manual(labels = c("Low", "High"),values = c("khaki","lightskyblue2"))?
  guides(fill = guide_legend(title = "Apartments Rent"))

p2 = ggplot(rent, aes(x = as.factor(laundry), fill = as.factor(group))) + 
  geom_bar( position = "dodge")+labs(title = "Laundry Situation by Apartments Rent", x = "") +
  theme(axis.ticks.x = elem?nt_blank())+theme(plot.title=element_text(hjust=0.5))+
  theme(legend.background = element_blank(),legend.justification=c(0,1), legend.position=c(0, 1))+
  geom_text(stat='count', aes(label=..count..), vjust=0.5, position = position_dodge(width = 1))+
  sc?le_fill_manual(labels = c("Low", "High"),values = c("khaki","lightskyblue2"))+
  guides(fill = guide_legend(title = "Apartments Rent"))

p3 = ggplot(rent, aes(x = as.factor(parking), fill = as.factor(group))) + 
  geom_bar( position = "dodge")+labs(title =?"Parking Policy by Apartments Rent", x = "") +
  theme(axis.ticks.x = element_blank())+theme(plot.title=element_text(hjust=0.5))+
  theme(legend.background = element_blank(),legend.justification=c(1,1), legend.position=c(1, 1))+
  geom_text(stat='count', a?s(label=..count..), vjust=0.5, position = position_dodge(width = 1))+
  scale_fill_manual(labels = c("Low", "High"),values = c("khaki","lightskyblue2"))+
  guides(fill = guide_legend(title = "Apartments Rent"))

library(plyr)
mu = ddply(rent, "group", summ?rise, grp.mean=mean(sqft))  # calculate the mean of different group
head(mu)

p4 = ggplot(rent, aes(x = sqft, fill = as.factor(group))) + 
  geom_density(alpha = 0.5, size = 1)+labs(title = "Apartments Size by Apartments Rent", x = "") +
  theme(axis.ticks?x = element_blank())+theme(plot.title=element_text(hjust=0.5))+
  theme(legend.background = element_blank(),legend.justification=c(1,1), legend.position=c(1, 1))+
  scale_fill_manual(labels = c("Low", "High"),values = c("khaki","lightskyblue2"))+
  guides(?ill = guide_legend(title = "Apartments Rent"))+
  geom_vline(data=mu, aes(xintercept=grp.mean),linetype="dashed") + xlim(0,3500)

grid.arrange(p1,p2,p3,p4,nrow=2)

# correlation of size and rent
ggplot(apartments,aes(y = sqft)) + geom_boxplot() + ylim(0,30?0)

# remove outliers
library(data.table)
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}
outlierReplace(apartments, "sqft", 
               which(apartments$sqft > 5000), ?A)
apart = na.omit(apartments)

ggplot(apart, aes(sqft,price)) + geom_point() + geom_smooth(method='loess') +
  labs(title="Relationship of Apartments Size and Rent",x="Apartments Size",y="Apartments Rent")+ 
  theme(plot.title=element_text(hjust=0.5,size ? 18)) 
cor(apart$sqft,apart$price,use = "complete.obs") 

# split size in quartile
group = rep(0, nrow(apart))
apart = cbind(apart, group)

breaks = quantile(apart$sqft, probs = seq(0, 1, 0.2), na.rm = TRUE)

for(i in 1:nrow(apart)){
  for(j in 1:5){
    i?(apart$sqft[i] >= breaks[j] & apart$sqft[i] <= breaks[j+1]){apart$group[i] = j}
  }
}
ggplot(apart) +geom_jitter(aes(sqft,price)) + geom_smooth(aes(sqft,price), method='loess', se=FALSE) +
  facet_wrap(~group, scales="free_x") + 
  labs(title="Apartments S?ze VS Rent in Different Size Quantiles",x="Apartments Size",y="Apartments Rent")+
  theme(plot.title=element_text(hjust=0.5,size = 18))
####
ap = na.omit(apartments)
apart5 = apart[apart[,"group"] == '5',]
cor(apart5$sqft,apart5$price,use = "complete.obs")?
# Make a report-worthy plot
apartments = readRDS("cl_apartments.rds")
apartments[which(apartments$price == max(apartments$price, na.rm = TRUE)), ]
apartments$price[apartments$price >= 30000000] = 3408
apartments[which(apartments$price == max(apartments$pr?ce, na.rm = TRUE)), ]
apartments$price[apartments$price >= 9900000] = 995
apartments$sqft[apartments$sqft >= 5000] = NA
big_cities = c('San Francisco', 'Oakland', 'San Jose', 'Sacramento', 'Los Angeles', 'San Diego')
# Use the %in% operator
apt_big6 = apar?ments[apartments$city %in% big_cities, ]

apt_big6$city = factor(apt_big6$city)
ggplot(apt_big6, aes(x = sqft, y = price)) + geom_point() + geom_smooth(method='lm')+ facet_wrap(.~city, nrow = 2)+
  labs(title="Relation of Apartments Rent and Size in Big Ci?ies",x="Apartments Size",y="Apartments Rent")+
  theme(plot.title=element_text(hjust=0.5,size = 18))

#### by craigslist
ggplot(apartments, aes(x=craigslist,y = price,color = craigslist)) + geom_boxplot(width=0.2, fill="white",size = 0.8) +
  geom_violin(a?pha = 0.4,size = 0.8) + ylim(0,17700)+
  theme(legend.position="none")+ theme(plot.title=element_text(hjust=0.5,size = 18))+
  labs(title="Apartments Rent by Craigslist Branch",x="",y="Apartments Rent")

## MId Apartments rent
data1 = apartments[apartments?,'bedrooms']%in%c('1','2'),]
data2 = data1[data1[,'bathrooms']%in%c('1','1.5'),]
data3 = data2[data2[,'parking']%in%c('covered','street','paid'),]
data4 = data3[data3[,'laundry']%in%c('hookup','shared'),]
data5 = data4[data4$city %in% big_cities, ]

g1 = g?plot(data5, aes(x=city,y = price,color = city)) + geom_boxplot(width=0.2, fill="white",size = 0.8) +
  theme(legend.position="none")+ theme(plot.title=element_text(hjust=0.5,size = 18))+
  labs(title="Mid-Apartments Rent in Big Cities",x="",y="Apartments R?nt")+
  stat_summary(geom="text", fun.y=median,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.3), size=5.5)

## superior quality Apartments rent
data1 = apartments[apartments[,'bedrooms']%in%c('3','4','5'),]
?ata2 = data1[data1[,'bathrooms']%in%c('2.5','3.5','3','4'),]
data3 = data2[data2[,'parking']%in%c('valet','garage'),]
data4 = data3[data3[,'laundry']%in%c('in-unit'),]
data5 = data4[data4$city %in% c('San Francisco', 'San Jose', 'Los Angeles', 'San Diego')? ]

g2 = ggplot(data5, aes(x=city,y = price,color = city)) + geom_boxplot(width=0.2, fill="white",size = 0.8) +
  theme(legend.position="none")+ theme(plot.title=element_text(hjust=0.5,size = 18))+
  labs(title="Superior Quality Apartments Rent in Big Citi?s",x="",y="Apartments Rent")+
  stat_summary(geom="text", fun.y=median,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.23), size=5.5)
grid.arrange(g1,g2)

# the number of missing values in each col
colSums(is.?a(apartments))
