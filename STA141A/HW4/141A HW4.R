apartments = readRDS("cl_apartments.rds")
Davis = subset(apartments, city == "Davis")
census = read.csv('2010_census_data/DEC_10_SF1_SF1DP1_with_ann.csv',header = T)
# get Davis census


# correct errors 
apartments$price[apartments$price >= 30000000] = 34?8
apartments$price[apartments$price >= 9900000] = 995

# remove outliers
library(data.table)
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}
outlierReplace(apartments, "sqf?", 
               which(apartments$sqft > 5000), NA)
outlierReplace(apartments, "sqft", 
               which(apartments$sqft < 100), NA)
outlierReplace(apartments, "price", 
               which(apartments$price < 200), NA)

library(ggplot2)
library(ggma?)
library(RColorBrewer)
library(gridExtra)
qmplot(longitude, latitude, data = Davis, colour = I('red'), size = I(3), darken = .1)
#  ggmap davis
bbox = c(
  -121.772613, 38.532675, # bottom left
  -121.695120, 38.571458 # top right
)
m = get_stamenmap(bbox? zoom = 15,maptype = "toner-lite") # zoom default = 10

#  # distribution of pre price
D = Davis[,c('longitude','latitude','price','sqft')]
D$pre = D$price / D$sqft
D = D[,c('longitude','latitude','pre')]

D$quartile = as.integer(cut(D$pre, quantile(D$pre,?probs=0:4/4,na.rm = T), include.lowest=TRUE))
D$quartile = as.factor(D$quartile)
D = na.omit(D)

# get pre-price quartile
quantile(Davis_per_price$pre, c(0,0.25, 0.5, 0.75,1), type = 1) 
# 1.309689 1.444328 2.025140 2.189286 5.482517

ggmap(m) + geom_jitte?(aes(x = longitude, y = latitude, colour = quartile),data = D,height = 0.001, width = 0.001,size = 3,alpha = 0.85) +
  labs(title = "Distribution of Apartments Unit Price in Quartile : Davis", x='',y='')+ guides(colour=guide_legend(title="Unit Price Quarti?e")) +
  scale_colour_manual(labels = c("1.31 - 1.44", "1.45 - 2.03","2.04 - 2.19", "2.20 - 5.48"),
                      values=c('darkorchid2','#E69F00','#56B4E9','red3'))+
  theme(legend.position="top",plot.title = element_text(hjust = 0.5,size = 20),
 ?      legend.text = element_text(size=15),legend.title = element_text(size=15),
        axis.text = element_blank(),axis.ticks = element_blank())

########################################################## no use
#  distribution of price
Davis_price = Davi?[,c('longitude','latitude','price')]
Davis_price$quartile = as.integer(cut(Davis_price$price, quantile(Davis_price$price, probs=0:4/4), include.lowest=TRUE))
Davis_price$quartile = as.factor(Davis_price$quartile)

# get price quartile
quantile(Davis_price$?rice, c(0,0.25, 0.5, 0.75,1), type = 1) 

p1 = ggmap(m) + geom_jitter(aes(x = longitude, y = latitude, colour = quartile),data = Davis_price,height = 0.001, width = 0.001,size = 3,alpha = 0.85) +
  labs(title = "Distribution of Apartments Price in Quartile?: Davis", x='',y='')+ guides(colour=guide_legend(title="Price Quartile")) +
  scale_colour_manual(labels = c("721 - 1375", "1376 - 1514","1515 - 1839", "1840 - 2700"),
                      values=c('darkorchid2','#E69F00','#56B4E9','red3'))+
  theme(legen?.position="top",plot.title = element_text(hjust = 0.5,size = 20),legend.text = element_text(size=15),legend.title = element_text(size=15))

#  distribution of sqft
Davis_sqft = Davis[,c('longitude','latitude','sqft')]
Davis_sqft = na.omit(Davis_sqft)
Davis?sqft$quartile = as.integer(cut(Davis_sqft$sqft, quantile(Davis_sqft$sqft, probs=0:4/4), include.lowest=TRUE, na.rm = TRUE))
Davis_sqft$quartile = as.factor(Davis_sqft$quartile)

p2 = ggmap(m) + geom_jitter(aes(x = longitude, y = latitude, colour = quartile?,data = Davis_sqft,height = 0.001, width = 0.001,size = 3,alpha = 0.85) +
  labs(title = "Distribution of Apartments Size in Quartile : Davis", x='',y='')+ guides(colour=guide_legend(title="Size Quartile")) +
  scale_colour_manual(labels = c("143 - 716", "?17 - 840","841 - 953", "954 - 1350"),
                      values=c('darkorchid2','#E69F00','#56B4E9','red3'))+
  theme(legend.position="top",plot.title = element_text(hjust = 0.5,size = 20),legend.text = element_text(size=15),legend.title = element_text(?ize=15))
# get size quartile
quantile(Davis_sqft$sqft, c(0,0.25, 0.5, 0.75,1), type = 1) 

grid.arrange(p1,p2,nrow=1)
######################################################################################### no use

Davis_bed = Davis[,c('longitude','latitu?e','bedrooms')]
Davis_bed$bedrooms = as.factor(Davis_bed$bedrooms)
Davis_bed = na.omit(Davis_bed)
p3 = ggmap(m) + geom_jitter(aes(x = longitude, y = latitude, colour = bedrooms),data = Davis_bed,
                       height = 0.001, width = 0.001,size = ?,alpha = 0.85) +
  labs(title = "Distribution of Apartments Bedrooms Number in Davis", x='',y='')+ 
  theme(legend.position="top",plot.title = element_text(hjust = 0.5,size = 16),
        legend.text = element_text(size=15),legend.title = element_text(size?14))+ 
  scale_colour_manual(values=c('darkorchid2','#E69F00','#56B4E9','red3'))
             

Davis_bath = Davis[,c('longitude','latitude','bathrooms')]
Davis_bath$bathrooms = as.factor(Davis_bath$bathrooms)
Davis_bath = na.omit(Davis_bath)
p4 = ggmap(m)?+ geom_jitter(aes(x = longitude, y = latitude, colour = bathrooms),data = Davis_bath,
                       height = 0.001, width = 0.001,size = 3,alpha = 0.85) +
  labs(title = "Distribution of Apartments Bathrooms Number in Davis", x='',y='')+ 
  theme(?egend.position="top",plot.title = element_text(hjust = 0.5,size = 16),
        legend.text = element_text(size=15),legend.title = element_text(size=14))+ 
  scale_colour_manual(values=c('darkorchid2','#E69F00','#56B4E9','red3'))


Davis_parking = Davis[,c(?longitude','latitude','parking')]
Davis_parking$parking = as.factor(Davis_parking$parking)
Davis_parking = na.omit(Davis_parking)
p5  = ggmap(m) + geom_jitter(aes(x = longitude, y = latitude, colour = parking),data = Davis_parking,
                       h?ight = 0.001, width = 0.001,size = 3,alpha = 0.85) +
  labs(title = "Distribution of Apartments Parking Policy in Davis", x='',y='')+ 
  theme(legend.position="top",plot.title = element_text(hjust = 0.5,size = 20),
        legend.text = element_text(size=1?),legend.title = element_text(size=15))

Davis_laundry = Davis[,c('longitude','latitude','laundry')]
Davis_laundry$laundry = as.factor(Davis_laundry$laundry)
Davis_laundry = na.omit(Davis_laundry)
p6 = ggmap(m) + geom_jitter(aes(x = longitude, y = latitude? colour = laundry),data = Davis_laundry,
                       height = 0.001, width = 0.001,size = 3,alpha = 0.85) +
  labs(title = "Distribution of Apartments Laundry in Davis", x='',y='')+ 
  theme(legend.position="top",plot.title = element_text(hjust ? 0.5,size = 20),
        legend.text = element_text(size=15),legend.title = element_text(size=15))

Davis_pets = Davis[,c('longitude','latitude','pets')]
Davis_pets$pets = as.factor(Davis_pets$pets)
Davis_pets = na.omit(Davis_pets)
p7 = ggmap(m) + geom_jit?er(aes(x = longitude, y = latitude, colour = pets),data = Davis_pets,
                       height = 0.001, width = 0.001,size = 3,alpha = 0.85) +
  labs(title = "Distribution of Apartments Pets Policy in Davis", x='',y='')+ 
  theme(legend.position="top"?plot.title = element_text(hjust = 0.5,size = 20),
        legend.text = element_text(size=15),legend.title = element_text(size=15))

grid.arrange(p4,p3,nrow = 1)
grid.arrange(p6,p5,nrow = 1)
p7

# high quality Apartments
D_suit = Davis[which(Davis$bathroom? >= Davis$bedrooms),]
D_suit = D_suit[D_suit[,'parking']%in%c('covered','street','garage','paid'),]
D_suit = D_suit[D_suit[,'laundry']%in%c('in-unit','shared','hookup'),]
D_suit$pre = D_suit$price / D_suit$sqft
  
ggmap(m) + geom_jitter(aes(x = longitude, ? = latitude),data = D_suit,
                       height = 0.001, width = 0.001,size = 3,alpha = 0.85,color = 'red') +
  labs(title = "Distribution of High Quality Apartments in Davis", x='',y='')+ 
  theme(legend.position="top",plot.title = element_text(?just = 0.5,size = 20),
        legend.text = element_text(size=15),legend.title = element_text(size=15),
        axis.text = element_blank(),axis.ticks = element_blank())

########################################################### Southern Bay Area
range(?BA$latitude)
range(SBA$longitude)

bbox = c(
 -122.60, 36.90, # bottom left
 -121.45, 38.16) # top right

p = get_stamenmap(bbox) 
ggmap(p)
SBA = apartments[apartments[,'county']%in%
                  c('San Francisco', 'San Mateo', 'Santa Clara', 'Alameda?,'Contra Costa'),]
SBA = na.omit(SBA)
SBA$pre = SBA$price / SBA$sqft
SBA$quartile = as.integer(cut(SBA$pre, quantile(SBA$pre, probs=0:4/4), include.lowest=TRUE))
SBA$quartile = as.factor(SBA$quartile)

# family friendly 
S_ff = SBA[SBA[,'bedrooms']%in%c('3?,'4','5','6'),]
S_ff = S_ff[S_ff[,'bathrooms']%in%c('2.5','3','3.5','4','4.5','5','5.5'),]
S_ff = S_ff[S_ff[,'pets']%in%c('both','cats','dogs','negotiable'),]
S_ff = S_ff[S_ff[,'parking']%in%c('covered','garage','paid'),]
S_ff = S_ff[S_ff[,'laundry']%in%c(?in-unit','hookup'),]
S_ff = S_ff[S_ff[,'quartile']%in%c('1','2'),]
S_ff = na.omit(S_ff)

g2 = ggmap(p, extent = "panel", maprange=FALSE) +
  geom_density2d(data = S_ff, aes(x = longitude, y = latitude)) +
  stat_density2d(data = S_ff, aes(x = longitude, y ? latitude, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 30, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.?itle = element_blank(),plot.title = element_text(hjust = 0.5,size = 13),
        axis.text = element_blank(),axis.ticks = element_blank()) +
  labs(title = 'Distribution of Family Friendly Apartments in Southern Bay Area')


# not friendly
S_ff_no = SBA[SB?[,'bedrooms']%in%c('0','1','2'),]
S_ff_no = S_ff_no[S_ff_no[,'bathrooms']%in%c('0','1','1.5','2'),]
S_ff_no = S_ff_no[S_ff_no[,'pets']%in%c('none'),]
S_ff_no = S_ff_no[S_ff_no[,'parking']%in%c('none','off-street','street'),]
S_ff_no = S_ff_no[S_ff_no[,'lau?dry']%in%c('none','shared'),]
S_ff_no = S_ff_no[S_ff_no[,'quartile']%in%c('1','2'),]

S_ff_no = na.omit(S_ff_no)

g1 = ggmap(p, extent = "panel", maprange=FALSE) +
  geom_density2d(data = S_ff_no, aes(x = longitude, y = latitude)) +
  stat_density2d(data =?S_ff_no, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 30, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(?egend.position = "none", axis.title = element_blank(),plot.title = element_text(hjust = 0.5,size = 13),
        axis.text = element_blank(),axis.ticks = element_blank()) +
  labs(title = 'Distribution of Not Family Friendly Apartments in Southern Bay Area'?

grid.arrange(g1,g2,nrow = 1)

# southern bay   price/size
SBA_sqft = SBA[,c('longitude','latitude','sqft','price','county')]
SBA_sqft = na.omit(SBA_sqft)
SBA_sqft$pre = SBA_sqft$price / SBA_sqft$sqft
SBA_sqft$quartile = as.integer(cut(SBA_sqft$pre, quant?le(SBA_sqft$pre, probs=0:4/4), include.lowest=TRUE))
SBA_sqft$quartile = as.factor(SBA_sqft$quartile)

#ggmap(p) + geom_point(aes(x = longitude, y = latitude, colour = quartile),data = SBA_sqft,size = 2) +
#  labs(title = "Distribution of Apartments Size i? Quartile : Southern Bay Area", x='',y='')+ guides(colour=guide_legend(title="Size Quartile")) +
#  scale_colour_manual(labels = c("80 - 680", "681 - 819","820 - 1013", "1014 - 5000"),
#                      values=c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78A?3"))+
#  theme(legend.position="top",plot.title = element_text(hjust = 0.5,size = 18))
# get price/size quartile
quantile(SBA_sqft$pre, c(0,0.25, 0.5, 0.75,1), type = 1) 
# 0.2320205  2.8853914  3.3913043  4.1176471 19.9500000 

ggmap(p, extent = "panel", ?aprange=FALSE) +
   geom_density2d(data = SBA_sqft, aes(x = longitude, y = latitude)) +
   stat_density2d(data = SBA_sqft, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
                                       size = 0.01, bins = 30,?geom = 'polygon') +
   scale_fill_gradient(low = "green", high = "red") +
   scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
   theme(legend.position = "none", axis.title = element_blank(),plot.title = element_text(hjust = 0.5,size = 12),
         axis?text = element_blank(),axis.ticks = element_blank()) +
  labs(title = 'Distribution of Apartments Unit Price by Quartile in Southern Bay Area') + facet_wrap(~quartile)

# southern bay area   price
#SBA_price = SBA[,c('longitude','latitude','price','county'?]
#outlierReplace(SBA_price, "price", 
#                              which(SBA_price$price < 200), NA)
#SBA_price = na.omit(SBA_price)
#SBA_price$quartile = as.integer(cut(SBA_price$price, quantile(SBA_price$price, probs=0:4/4), include.lowest=TRUE))
#SBA?price$quartile = as.factor(SBA_price$quartile)

#ggmap(p, extent = "panel", maprange=FALSE) +
#  geom_density2d(data = SBA_price, aes(x = longitude, y = latitude)) +
#  stat_density2d(data = SBA_price, aes(x = longitude, y = latitude, fill = ..level.., alp?a = ..level..),
#                 size = 0.01, bins = 30, geom = 'polygon') +
#  scale_fill_gradient(low = "green", high = "red") +
#  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
#  theme(legend.position = "none", axis.title = element_blank(),plot.?itle = element_text(hjust = 0.5,size = 15)) +
#  labs(title = 'Distribution of Apartments Price by Quartile in Southern Bay Area') + facet_wrap(~quartile)
# get price quartile
#quantile(SBA_price$price, c(0,0.25, 0.5, 0.75,1), type = 1) 
# 200 - 2275  2276?- 2750 2751 - 3450 3451 - 17700

by_group = split(SBA, factor(SBA$county))

SBA_parking = SBA[,c('longitude','latitude','parking','county')]
SBA_parking = na.omit(SBA_parking)
ggmap(p, extent = "panel", maprange=FALSE) +
  geom_density2d(data = SBA_parking? aes(x = longitude, y = latitude)) +
  stat_density2d(data = SBA_parking, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 30, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red")?+
  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5,size = 15)) +
  theme(axis.text = element_blank(),axis.ticks = element_blank() )+
  labs(title = 'Distribution of Apartments Park?ng in Southern Bay Area',x='',y='') + facet_wrap(~parking)

SBA_laundry = SBA[,c('longitude','latitude','laundry','county')]
SBA_laundry = na.omit(SBA_laundry)
ggmap(p, extent = "panel", maprange=FALSE) +
  geom_density2d(data = SBA_laundry, aes(x = longit?de, y = latitude)) +
  stat_density2d(data = SBA_laundry, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 30, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha?range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5,size = 15)) +
  theme(axis.text = element_blank(),axis.ticks = element_blank() )+
  labs(title = 'Distribution of Apartments Laundry in Southern ?ay Area',x='',y='') + facet_wrap(~laundry)

SBA_pets = SBA[,c('longitude','latitude','pets','county')]
SBA_pets = na.omit(SBA_pets)
ggmap(p, extent = "panel", maprange=FALSE) +
  geom_density2d(data = SBA_pets, aes(x = longitude, y = latitude)) +
  stat_de?sity2d(data = SBA_pets, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 30, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25), guide = FA?SE) +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5,size = 15)) +
  theme(axis.text = element_blank(),axis.ticks = element_blank() )+
  labs(title = 'Distribution of Apartments Pets Policy in Southern Bay Area',x='',y='') + facet_w?ap(~pets)

SBA_bedrooms = SBA[,c('longitude','latitude','bedrooms','county')]
SBA_bedrooms = na.omit(SBA_bedrooms)
ggmap(p, extent = "panel", maprange=FALSE) +
  geom_density2d(data = SBA_bedrooms, aes(x = longitude, y = latitude)) +
  stat_density2d(data ? SBA_bedrooms, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 30, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  ?heme(legend.position = "none",plot.title = element_text(hjust = 0.5,size = 15)) +
  theme(axis.text = element_blank(),axis.ticks = element_blank() )+
  labs(title = 'Distribution of Apartments Bedrooms Number in Southern Bay Area',x='',y='') + facet_wrap(~?edrooms)


SBA_bathrooms = SBA[,c('longitude','latitude','bathrooms','county')]
SBA_bathrooms = na.omit(SBA_bathrooms)
ggmap(p, extent = "panel", maprange=FALSE) +
  geom_density2d(data = SBA_bathrooms, aes(x = longitude, y = latitude)) +
  stat_density2d(?ata = SBA_bathrooms, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 30, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25), guide = FALSE? +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5,size = 15)) +
  theme(axis.text = element_blank(),axis.ticks = element_blank() )+
  labs(title = 'Distribution of Apartments Bathrooms Number in Southern Bay Area',x='',y='') + facet?wrap(~bathrooms)

# heat map
lon = c(-122.90,-121.25)
lat = c(36.90,38.16)

bbox = c(
  -122.60, 36.90, # bottom left
  -121.45, 38.16)

YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
MyMap <- get_map(location = c(lon = mean(lon), lat =?mean(lat)), 
                 source = "google", maptype = "roadmap", crop = FALSE)
ggmap(MyMap) %+% SBA_price +
  aes(x = longitude, y = latitude, z = price) +
  stat_summary_2d(fun = median, alpha = 0.5) +
  scale_fill_gradientn(name = "Price",colours = ?lOrBr, space = "Lab") +
  labs(title ='Distribution of Apartments Price in Southern Bay Area',x = "", y = "") +
  coord_map()+ theme(axis.text = element_blank(),axis.ticks = element_blank(),
                     plot.title = element_text(hjust = 0.5,size =?15))

ggmap(MyMap) %+% SBA_sqft +
  aes(x = longitude, y = latitude, z = pre) +
  stat_summary_2d(fun = median, alpha = 0.5) +
  scale_fill_gradientn(name = "Unit Price",colours = YlOrBr, space = "Lab") +
  labs(title ='Distribution of Apartments Unit Pric? in Southern Bay Area',x = "", y = "") +
  coord_map()+ theme(axis.text = element_blank(),axis.ticks = element_blank(),
                     plot.title = element_text(hjust = 0.5,size = 15))

ggmap(MyMap) %+% SBA_sqft +
  aes(x = longitude, y = latitude, z?= sqft) +
  stat_summary_2d(fun = median, alpha = 0.5) +
  scale_fill_gradientn(name = "Size",colours = YlOrBr, space = "Lab") +
  labs(title ='Distribution of Apartments Size in Southern Bay Area',x = "", y = "") +
  coord_map()+ theme(axis.text = element?blank(),axis.ticks = element_blank(),
                     plot.title = element_text(hjust = 0.5,size = 15))

# high quality apartments in S Bay area
SBA$pre = SBA$price / SBA$sqft
SBA$quartile = as.integer(cut(SBA$pre, quantile(SBA$pre, probs=0:4/4,na.rm ?T), include.lowest=TRUE))
SBA$quartile = as.factor(SBA$quartile)
quantile(SBA$pre, c(0,0.25, 0.5, 0.75,1), type = 1,na.rm = T) 
# 0.2320205  2.8853914  3.3913043  4.1176471 19.9500000  

S_suit = SBA[which(SBA$bathrooms >= SBA$bedrooms),]
S_suit = S_suit[S?suit[,'parking']%in%c('covered','street','garage','paid'),]
S_suit = S_suit[S_suit[,'laundry']%in%c('in-unit','shared','hookup'),]
S_suit = na.omit(S_suit)

ggmap(p) + geom_jitter(aes(x = longitude, y = latitude, colour = quartile),data = S_suit,
         ?             height = 0.001, width = 0.001,size = 1) +
  labs(title = "Distribution of High Quality Apartments in Southern Bay Area", x='',y='')+ 
  theme(legend.position="right",plot.title = element_text(hjust = 0.5,size = 14),
        legend.text = eleme?t_text(size=10),legend.title = element_text(size=12),
        axis.text = element_blank(),axis.ticks = element_blank())+ guides(colour=guide_legend(title="Unit Price Quartile"))+
  scale_colour_manual(labels = c("0.23 - 2.88", "2.89 - 3.39","3.40 - 4.12", ?4.13 - 19.95"),
                  values=c('darkorchid2','#E69F00','#56B4E9','red3'))

table(factor(S_suit$county))
table(factor(SBA$county))

################################### oldest population
levels(SBA$city)
census = read.csv('2010_census_data/DEC_10?SF1_SF1DP1_with_ann.csv',header = T)
census = census[-1,]

#str = census$GEO.display.label
#data_str = data.frame(str)
#head(data_str)
#data_str<-mutate(data_str,str=as.character(str))
#data_str<-mutate(data_str,str=sapply(strsplit(data_str$str, split=',',?fixed=TRUE),function(x) (x[1])))
#data_str2<-mutate(data_str,str=sapply(strsplit(data_str$str, split=' ', fixed=TRUE),function(x) (x[-length(x)])))
#head(data_str2)
library(stringr)
census[, 3] = str_remove_all(census[, 3], ' (town|city|CDP|CDP \\(.+ Count?\\)), California')

old = census[,c('GEO.display.label','HD02_S025','HD01_S020')]
colnames(old)[colnames(old)=="GEO.display.label"] <- "city"
colnames(old)[colnames(old)=="HD01_S020"] <- "Total population percent median age"
colnames(old)[colnames(old)=="H?02_S025"] <- "Total population Percent 65+"

SBA_2 = SBA[,c('latitude','longitude','price','sqft','bedrooms','bathrooms','pets','laundry','parking','city','county','pre','quartile')]
M = merge(old,SBA_2,by = 'city')

M_1 = M[,c(1,2,3)]
M_1 = unique(M_1)
M_? = M[,c(1,3)]
M_2 = unique(M_2)
M_2$`Total population percent median age` = as.character(M_2$`Total population percent median age` )
M_2$`Total population percent median age` = as.numeric(M_2$`Total population percent median age` )
M_2$quartile = as.intege?(cut(M_2$`Total population percent median age`, quantile(M_2$`Total population percent median age`, probs=0:4/4,na.rm =T), include.lowest=TRUE))
M_2$quartile = as.factor(M_2$quartile)
quantile(M_2$`Total population percent median age` , c(0, 0.5, 1), type ? 1,na.rm = T) 
# 31.0 36.1 38.7 42.6 51.3 

M$`Total population percent median age` = as.character(M$`Total population percent median age` )
M$`Total population percent median age` = as.numeric(M$`Total population percent median age` )


group = rep(0, nro?(M))
M = cbind(M, group)
M = na.omit(M)
breaks = quantile(M$`Total population percent median age`, probs = seq(0, 1, 0.2), name = FALSE)

for(i in 1:nrow(M)){
  for(j in 1:5){
    if(M$`Total population percent median age`[i] >= breaks[j] & M$`Total popula?ion percent median age`[i] <= breaks[j+1]){M$group[i] = j}
  }
}

M$group = as.factor(M$group)
h1 = ggplot(M,aes(x=price, color = group)) + geom_density(alpha = 0.3,size = 1.5) +
  labs(title = "Distribution of Apartments Price in Southern Bay Area by Age ?roup") +
  theme(plot.title = element_text(hjust = 0.5,size = 15))+
  labs(y="Density")+
  labs(x=" ") + scale_color_manual(labels = c("31.0 - 36.5", "36.6 - 51.3"),
                                  values=c("#E69F00", "#56B4E9"))+
  theme(legend.backgrou?d = element_blank(),legend.justification=c(1,1), legend.position=c(1, 1),
        axis.ticks.y = element_blank()) + 
  guides(color=guide_legend(title="Age Group")) + xlim(0,10000)


h2 = ggplot(M,aes(x=sqft, color = group)) + geom_density(alpha = 0.3,size?= 1.5) +
  labs(title = "Distribution of Apartments Size in Southern Bay Area by Age Group") +
  theme(plot.title = element_text(hjust = 0.5,size = 15))+
  labs(y="Density")+
  labs(x=" ") + scale_color_manual(labels = c("31.0 - 36.5", "36.6 - 51.3"),
    ?                              values=c("#E69F00", "#56B4E9"))+
  theme(legend.background = element_blank(),legend.justification=c(1,1), legend.position=c(1, 1),
        axis.ticks.y = element_blank()) + 
  guides(color=guide_legend(title="Age Group")) + xl?m(0,3000)

grid.arrange(h1,h2,nrow = 1)

h3 = ggplot(M, aes(x = as.factor(bedrooms) , fill = group)) + 
  geom_bar(position = "dodge")+labs(title = "Number of Bedrooms by Age Group", x = "Number of Bedrooms") +
  theme(axis.ticks.x = element_blank())+theme?plot.title=element_text(hjust=0.5))+
  scale_fill_manual(labels = c("31.0 - 35.2", "35.3 - 36.2",'36.3 - 38.3','38.4 - 38.8','38.9  -51.3'),
                    values=c("#E69F00", "#56B4E9",'lightslateblue','tomato1','black'))+
  theme(legend.background =?element_blank(),legend.justification=c(1,1), legend.position=c(1, 1))+
  geom_text(stat='count', aes(label=..count..), vjust=-0.2,position = position_dodge(width = 1))

h4 = ggplot(M, aes(x = as.factor(pets) , fill = group)) + 
  geom_bar(position = "dodge?)+labs(title = "Pets Allowed by Age Group", x = "Pets Allowed") +
  theme(axis.ticks.x = element_blank())+theme(plot.title=element_text(hjust=0.5))+
  scale_fill_manual(labels = c("31.0 - 35.2", "35.3 - 36.2",'36.3 - 38.3','38.4 - 38.8','38.9  -51.3'),
   ?                values=c("#E69F00", "#56B4E9",'lightslateblue','tomato1','black'))+
  theme(legend.background = element_blank(),legend.justification=c(1,1), legend.position=c(1, 1))+
  geom_text(stat='count', aes(label=..count..), vjust=-0.2,position = pos?tion_dodge(width = 1))

h5 = ggplot(M, aes(x = as.factor(parking) , fill = group)) + 
  geom_bar(position = "dodge")+labs(title = "Parking Policy by Age Group", x = "Parking Policy") +
  theme(axis.ticks.x = element_blank())+theme(plot.title=element_text(h?ust=0.5))+
  scale_fill_manual(labels = c("31.0 - 35.2", "35.3 - 36.2",'36.3 - 38.3','38.4 - 38.8','38.9  -51.3'),
                    values=c("#E69F00", "#56B4E9",'lightslateblue','tomato1','black'))+
  theme(legend.background = element_blank(),legend.ju?tification=c(1,1), legend.position=c(1, 1))+
  geom_text(stat='count', aes(label=..count..), vjust=-0.2, position = position_dodge(width = 1))


h6 = ggplot(M, aes(x = as.factor(laundry) , fill = group)) + 
  geom_bar(position = "dodge")+labs(title = "Laun?ry Situation by Age Group", x = "Laundry Situation") +
  theme(axis.ticks.x = element_blank())+theme(plot.title=element_text(hjust=0.5))+
  scale_fill_manual(labels = c("31.0 - 35.2", "35.3 - 36.2",'36.3 - 38.3','38.4 - 38.8','38.9  -51.3'),
              ?     values=c("#E69F00", "#56B4E9",'lightslateblue','tomato1','black'))+
  theme(legend.background = element_blank(),legend.justification=c(1,1), legend.position=c(1, 1))+
  geom_text(stat='count', aes(label=..count..), vjust=-0.2, position = position_dodg?(width = 1))

grid.arrange(h5,h6,h3,h4,nrow = 2)

by_group = split(M,M$group)
table(by_group[[1]]$pets)
table(by_group[[1]]$laundry)
table(by_group[[1]]$parking)
table(by_group[[2]]$pets)
table(by_group[[2]]$laundry)
table(by_group[[2]]$parking)
table(by_g?oup[[3]]$pets)
table(by_group[[3]]$laundry)
table(by_group[[3]]$parking)
table(by_group[[4]]$pets)
table(by_group[[4]]$laundry)
table(by_group[[4]]$parking)
table(by_group[[5]]$pets)
table(by_group[[5]]$laundry)
table(by_group[[5]]$parking)
