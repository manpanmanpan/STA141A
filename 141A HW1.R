# 2
data = readRDS("college_scorecard.rds")
nrow(data)
head(data)

# 3
ncol(data)
colnames(data)

# 4
data$academic_year = factor(data$academic_year)
levels(data$academic_year)

nrow(data[data[,'academic_year'] == '2012',])
nrow(data[data[,'academic_year'] == '2013',])
nrow(data[data[,'academic_year'] == '2014',])
nrow(data[data[,'academic_year'] == '2015',])
nrow(data[data[,'academic_year'] == '2016',])

# or
table(data$academic_year)

# 5
data$state = factor(data$state)
a=levels(data$state)
list = rep(0,length(a))
for (i in 1:length(list)) {
  list[i] = nrow(data[data[,'state'] == a[i],])
}
sort(list)
a[which(list == 5)]

a[which(list == 2022)]
a[which(list == 2176)]
a[which(list == 2317)]
a[which(list == 2381)]
a[which(list == 3881)]

# 6
data_2014 = data[data[,'academic_year'] == '2014',]
data$ownership = factor(data$ownership)
levels(data$ownership)
data_public_2014 = data_2014[data_2014[,'ownership'] == 'Public',]

library(ggplot2)
# Basic scatter plot
ggplot(data_public_2014, aes(x = data_public_2014$avg_net_price.public, y = data_public_2014$earn_10_yrs_after_entry.median
)) + geom_point()+ theme(plot.title=element_text(hjust=0.5))+
  labs(title = "scatter plot of average net price versus median student earnings after 10 years for Public School", x= "Avg_Net_Price_Public", y = "Earn_10_Yrs_After_Entry_Median")

# 7
data_profit_2014 = data_2014[data_2014[,'ownership'] == 'Private for-profit',]
ggplot(data_profit_2014, aes(x = data_profit_2014$avg_net_price.private, y = data_profit_2014$earn_10_yrs_after_entry.median)) +
  geom_point()+ theme(plot.title=element_text(hjust=0.5))+
  labs(title = "scatter plot of average net price versus median student earnings after 10 years for Private Profit School", x= "Avg_Net_Price_Profit", y = "Earn_10_Yrs_After_Entry_Median")

# 8
data_nonprofit_2014 = data_2014[data_2014[,'ownership'] == 'Private nonprofit',]
ggplot(data_nonprofit_2014, aes(x = data_nonprofit_2014$avg_net_price.private, y = data_nonprofit_2014$earn_10_yrs_after_entry.median)) +
  geom_point()+ theme(plot.title=element_text(hjust=0.5))+
  labs(title = "scatter plot of average net price versus median student earnings after 10 years for Private NonProfit Schools", x= "Avg_Net_Price_Profit", y = "Earn_10_Yrs_After_Entry_Median")

# combine the three plots in one plot
a = data$avg_net_price.public
b = data$avg_net_price.private
a[is.na(a)] = 0
b[is.na(b)] = 0

avg_net_price = a+b
avg_net_price[avg_net_price == 0] = NA

data$avg_net_price = avg_net_price

ggplot(data, aes(x = data$avg_net_price, y = data$earn_10_yrs_after_entry.median, color = ownership)) +
  geom_point()+ theme(plot.title=element_text(hjust=0.5))+
  labs(title = "scatter plot of average net price versus median student earnings after 10 years", x= "Average Net Price", y = "Median Student Earnings after 10 Years")+
  scale_color_manual(values = c("Private for-profit"="darkseagreen2", "Private nonprofit"="gold1","Public"="lightskyblue1"))+
  theme(legend.position="top")+ ylim(0, 150000) + xlim(0,80000)
  

# 9
Academic_year = data$academic_year
ggplot(data,aes(x = data$ownership, fill = Academic_year)) + geom_bar(position = "dodge")+
  scale_fill_manual(values = c("2012"="lightcyan2", "2013"="lightsteelblue1","2014"="lightskyblue1","2015"="lightskyblue2","2016"="lightblue3"))+
  theme(legend.position="top")+
  labs(title = "The Number of Schools for Different Ownership: 2012 - 2016", x= "")+ theme(plot.title=element_text(hjust=0.5))

# 10
data_UCD = data[data[,'id'] == '110644',]
ggplot(data=data_UCD, aes(x=academic_year, y=admission_rate.overall, group=1)) +
  geom_line(color="red", size=1.2)+geom_point(size=3)+
  labs(title = "The Admission Rates for UC Davis: 2012 - 2016")+ theme(plot.title=element_text(hjust=0.5))

# 11
for(a in names(data)){
  print(c(a, typeof(data[, a])))
}
typeof(data$state)
typeof(data$degrees_awarded.highest)
typeof(data$name)
typeof(data$academic_year)

