data = readRDS("college_scorecard.rds")
####### 1
# calculate missing value of each column and add the features' name
res = NULL
for (i in 1:ncol(data)){
    temp<-sum(is.na (data[,i]))
    temp<-as.data.frame(temp)
    temp$Features<-colnames(data)[i]
   
    res<-rbind(res,temp)
  }
names(res)[names(res) == "temp"] = "Number_of_missing_value"
res[order(res$Number_of_missing_value),]   # sorting by increasing
res[order(-res$Number_of_missing_value),]  # sorting by decreasing

######### 2
options("scipen"=100, "digits"=5)
# undergraduate population
a = data[,c("id","size")]
a$size = as.numeric(a$size)
# graduate population
b = data[,c("id","grad_students")]
b$grad_students = as.numeric(b$grad_students)

# combine the two population in one dataset

group = rep?0, nrow(a))  # group equals to 0 means undergraduate
a = cbind(a, group)
group = rep(1, nrow(b))  #  group equals to 1 means graduate
b = cbind(b, group)
names(a)[names(a) == "size"] = "Population"
names(b)[names(b) == "grad_students"] = "Population"
ab = ?bind(a,b)
ab$group = as.factor(ab$group)
ab$id = as.factor(ab$id)

levels(ab$group)[which(levels(ab$group)==0)] = "Undergraduate"
levels(ab$group)[which(levels(ab$group)==1)] = "Graduate"
ab$group = as.factor(ab$group)

library(ggplot2)
# Basic scatter plot of undergraduate and graduate population
ggplot(ab, aes(x = id, y = Population, color = group)) + geom_point()+
  scale_color_manual(values = c("Graduate"="lightskyblue2", "Undergraduate"="darkorange"))+
  theme(axis.ticks.x = element_blank() )+theme(plot.title=element_text(hjust=0.5))+
  labs(title="Student Populations for the Universities: 2012 - 2016",x="Universities",y="Popultaion")+
  theme(axis.text.x = element_blank() ) +
  theme(legend.position=c(0,1), legend.justification=c(0,1))

# undergraduate u?usual populations 
data[data[,"id"] == '372213',]$name  #University of Phoenix-Online Campus
ab[ab[,"id"] == '372213',]$Population #  205286 166816  47660  41900
data[data[,"id"] == '484613',]$name  #University of Phoenix-Arizona
ab[ab[,"id"] == '484613',]?Population  # 151558 129615 100011  39187  32746  27918
data[data[,"id"] == '150987',]$name  #Ivy Tech Community College
ab[ab[,"id"] == '150987',]$Population # 91112 87017 77657 70074 65092


# graduate unusual populations
data[data[,"id"] == '125231',]$n?me #Walden University
ab[ab[,"id"] == '125231',]$Population #8558  8064  8658  7815  7329 41513 42811 43228 44560 44530
data[data[,"id"] == '372213',]$name #University of Phoenix-Online Campus
ab[ab[,"id"] == '372213',]$Population #205286 166816  47660  41?00
data[data[,"id"] == '484613',]$name #University of Phoenix-Arizona
ab[ab[,"id"] == '484613',]$Population #151558 129615 100011  39187  32746  27918
data[data[,"id"] == '413413',]$name #Capella University
ab[ab[,"id"] == '413413',]$Population #8066  7935? 8738  9098  9385 27667 26060 26311 27969 28176


##### population grouped by different academic year
c = data[,c("id","size","grad_students","academic_year")]
c$size[is.na(c$size)] = 0
c$grad_students[is.na(c$grad_students)] = 0

c$total_population = c$si?e + c$grad_students
c$total_population[c$total_population == 0] = NA
c$id = as.factor(c$id)
c$academic_year = as.factor(c$academic_year)
levels(c$academic_year)

hist6 = ggplot(c, aes(x = id, y = total_population, color = academic_year)) + geom_point()+
  ?cale_color_manual(values = c("2012"="springgreen3", "2013"="darkorange", "2014"="lightcoral", "2015"="violetred2", "2016"="mediumpurple3"))+
  theme(axis.ticks.x = element_blank() )+ theme(plot.title=element_text(hjust=0.5))+
  labs(title="Student Populati?ns for the Universities of Different Years",x="Universities",y="Total Popultaion")+
  theme(axis.text.x = element_blank() ) +theme(plot.title = element_text(size = 10))+
  theme(legend.position=c(1,1), legend.justification=c(1,1))

c_2012 = c[c[,'academic_?ear'] == "2012",]
c_2013 = c[c[,'academic_year'] == "2013",]
c_2014 = c[c[,'academic_year'] == "2014",]
c_2015 = c[c[,'academic_year'] == "2015",]
c_2016 = c[c[,'academic_year'] == "2016",]
hist1 = ggplot(c_2012, aes(x = id, y = total_population, color = a?ademic_year )) + geom_point()+
  scale_color_manual(values = c("2012"="palevioletred1"))+
  theme(axis.ticks.x = element_blank() )+ theme(plot.title=element_text(hjust=0.5))+
  labs(title="Student Populations for the Universities: 2012",x="Universities",y=?Total Popultaion")+
  theme(axis.text.x = element_blank() )+theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")
hist2 = ggplot(c_2013, aes(x = id, y = total_population, color = academic_year )) + geom_point()+
  scale_color_manua?(values = c("2013"="violet"))+
  theme(axis.ticks.x = element_blank() )+ theme(plot.title=element_text(hjust=0.5))+
  labs(title="Student Populations for the Universities: 2013",x="Universities",y="Total Popultaion")+
  theme(axis.text.x = element_blank() ? +theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")
hist3 = ggplot(c_2014, aes(x = id, y = total_population, color = academic_year )) + geom_point()+
  scale_color_manual(values = c("2014"="steelblue1"))+
  theme(axis.ticks.x =?element_blank() )+ theme(plot.title=element_text(hjust=0.5))+
  labs(title="Student Populations for the Universities: 2014",x="Universities",y="Total Popultaion")+
  theme(axis.text.x = element_blank() ) +theme(plot.title = element_text(size = 10))+
  them?(legend.position = "none")
hist4 = ggplot(c_2015, aes(x = id, y = total_population, color = academic_year )) + geom_point()+
  scale_color_manual(values = c("2015"="tan1"))+
  theme(axis.ticks.x = element_blank() )+ theme(plot.title=element_text(hjust=0.5)?+
  labs(title="Student Populations for the Universities: 2015",x="Universities",y="Total Popultaion")+
  theme(axis.text.x = element_blank() ) +theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")
hist5 = ggplot(c_2016, aes(x = i?, y = total_population, color = academic_year )) + geom_point()+
  scale_color_manual(values = c("2016"="paleturquoise3"))+
  theme(axis.ticks.x = element_blank() )+ theme(plot.title=element_text(hjust=0.5))+
  labs(title="Student Populations for the Unive?sities: 2016",x="Universities",y="Total Popultaion")+
  theme(axis.text.x = element_blank() ) +theme(plot.title = element_text(size = 10))+
  theme(legend.position = "none")

library(gridExtra)
grid.arrange(hist1,hist2,hist3,hist4,hist5,hist6,nrow=2)

# ge? the unusual population each year
c_2012[c_2012[,"id"] == '372213',]$total_population
c_2013[c_2013[,"id"] == '372213',]$total_population
c_2014[c_2014[,"id"] == '484613',]$total_population
c_2015[c_2015[,"id"] == '484613',]$total_population
c_2016[c_2016[?"id"] == '484613',]$total_population

# relation of undergraduate and graduate population in 2016
# undergraduate population
data_2016 = data[data[,"academic_year"] == '2016',]
cor(data_2016$size,data_2016$grad_students,use = "complete.obs")  # 0.6960052

?ata_2015 = data[data[,"academic_year"] == '2015',]
cor(data_2015$size,data_2015$grad_students,use = "complete.obs")  #0.6979115

data_2014 = data[data[,"academic_year"] == '2014',]
cor(data_2014$size,data_2014$grad_students,use = "complete.obs")  # 0.71212?

data_2013 = data[data[,"academic_year"] == '2013',]
cor(data_2013$size,data_2013$grad_students,use = "complete.obs")  # 0.7105582

data_2012 = data[data[,"academic_year"] == '2012',]
cor(data_2012$size,data_2012$grad_students,use = "complete.obs")  # 0.7?15982



Undergraduate_Population = data_2016$size
Graduate_Population = data_2016$grad_students
hist1 = ggplot(data_2016, aes(Undergraduate_Population ,Graduate_Population)) + geom_point() + geom_smooth(method='lm') +
  labs(title="Relationship between Un?ergraduate and Graduate Populations in 2016")+ 
  theme(plot.title=element_text(hjust=0.5,size = 10)) + annotate("text",x=60000,y=20000,label ="Cor = 0.6960052")

hist2 = ggplot(data_2015, aes(data_2015$size,data_2015$grad_students)) + geom_point() + geom_?mooth(method='lm') +
  labs(title="Relationship between Undergraduate and Graduate Populations in 2015", x="Undergraduate_Population", y="Graduate_Population")+ 
  theme(plot.title=element_text(hjust=0.5,size = 10)) + annotate("text",x=100000,y=18000,label?="Cor = 0.6979115")

hist3 = ggplot(data_2014, aes(data_2014$size,data_2014$grad_students)) + geom_point() + geom_smooth(method='lm') +
  labs(title="Relationship between Undergraduate and Graduate Populations in 2014", x="Undergraduate_Population", y="Gra?uate_Population")+ 
  theme(plot.title=element_text(hjust=0.5,size = 10)) + annotate("text",x=100000,y=18000,label ="Cor = 0.712125")

hist4 = ggplot(data_2013, aes(data_2013$size,data_2013$grad_students)) + geom_point() + geom_smooth(method='lm') +
  labs?title="Relationship between Undergraduate and Graduate Populations in 2013", x="Undergraduate_Population", y="Graduate_Population")+ 
  theme(plot.title=element_text(hjust=0.5,size = 10)) + annotate("text",x=100000,y=18000,label ="Cor = 0.7105582")

hist5 ? ggplot(data_2012, aes(data_2012$size,data_2012$grad_students)) + geom_point() + geom_smooth(method='lm') +
  labs(title="Relationship between Undergraduate and Graduate Populations in 2012", x="Undergraduate_Population", y="Graduate_Population")+ 
  theme?plot.title=element_text(hjust=0.5,size = 10)) + annotate("text",x=100000,y=18000,label ="Cor = 0.7115982")

grid.arrange(hist1,hist2,hist3,hist4,hist5,nrow=2)

##### 3
colnames(data)

data_year = list()
for (i in (1:5)){
  data_year[[i]] = data[data[,'acad?mic_year'] == 2011+i,]
}

Programe = NULL
for (j in (47:84)){
  temp<-mean(data_year[[1]][,j], na.rm = TRUE)
  temp<-as.data.frame(temp)
  temp$Features<-colnames(data_year[[1]])[j]
  Programe<-rbind(Program,temp)
}

# add two loops into one 
data_year = l?st()
Program = NULL
for (i in (1:5)){
  data_year[[i]] = data[data[,'academic_year'] == 2011+i,]
  for (j in (47:84)){
    temp<-mean(data_year[[i]][,j], na.rm = TRUE)
    temp<-as.data.frame(temp)
    temp$Features<-colnames(data_year[[i]])[j]
    temp$Ye?r <- 2011+i
    Program<-rbind(Program,temp)
  }
}

Program$temp = round(Program$temp,5)
names(Program)[names(Program) == "temp"] = "Mean_of_program_percentages"

######### Are there any program percentages that show patterns di???erent from the others?
data_year = list()
for (i in (1:5)){
  data_year[[i]] = Program[Program[,'Year'] == 2011+i,]
}
Pro = list()
for (i in (1:5)){
  Pro[[i]] = ggplot(data_year[[i]]) +
    geom_point( aes(x=Features, y=Mean_of_program_percentages)) +
   ?labs(title = paste("The Mean of Program Percentages in", 2011 + i), x="Features", y="Mean of Program Percentages")+ 
    theme(plot.title=element_text(hjust=0.5,size = 10)) + theme(axis.text.x = element_blank()) 
}
grid.arrange(Pro[[1]],Pro[[2]],Pro[[3]],P?o[[4]],Pro[[5]],nrow=2)


library(dplyr)
# the most/least popular programes
#    least_popular = Program[order(Program$Year,Program$Mean_of_program_percentages),]
#    most_popular = Program[order(Program$Year,-Program$Mean_of_program_percentages),]

# the?most/least 5 popular programes
most_popular_2 = Program %>% group_by(Year) %>% top_n(5, Mean_of_program_percentages)
least_popular_2 = Program %>% group_by(Year) %>% top_n(-5, Mean_of_program_percentages)



#################### loop to plot 5 graphs of th? most popular program


data_year = list()
for (i in (1:5)){
  data_year[[i]] = most_popular_2[most_popular_2[,'Year'] == 2011+i,]
}
Pro = list()
for (i in (1:5)){
  Pro[[i]] = ggplot(data_year[[i]]) +
    geom_bar( aes(x=Features, y=Mean_of_program_percen?ages,fill = Features),stat="identity") + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","paleturquoise3","rosybrown2")) +
    guides(fill = "none") +
    labs(title = paste("The Most Popular Program in", 2011 + i), x="", y="Mean of Program Perc?ntages")+ 
    theme(plot.title=element_text(hjust=0.5,size = 10)) + theme(axis.text.x = element_text(angle = 30))+
    geom_text(aes(x=Features, y=Mean_of_program_percentages,label = Mean_of_program_percentages , vjust = -0.1, hjust = 0.5),size = 4)+
    ?cale_x_discrete(breaks=c("program_percentage.personal_culinary", "program_percentage.humanities", "program_percentage.visual_performing","program_percentage.health","program_percentage.business_marketing"),
                     labels=c("personal culinary"? "humanities", "visual performing","health","marketing")) 
}
grid.arrange(Pro[[1]],Pro[[2]],Pro[[3]],Pro[[4]],Pro[[5]],nrow=2)


#################### loop to plot 5 graphs of the least popular program

data_year = list()
for (i in (1:5)){
  data_year[[i]] ? least_popular_2[least_popular_2[,'Year'] == 2011+i,]
}
Pro = list()
for (i in (1:5)){
  Pro[[i]] = ggplot(data_year[[i]]) +
    geom_bar(aes(Features, Mean_of_program_percentages,fill = Features),stat="identity") + scale_fill_manual(values=c("#999999", "#?69F00", "#56B4E9","paleturquoise3","rosybrown2")) +
    guides(fill = "none") + theme(axis.text.x = element_text(angle = 30))+
    labs(title = paste("The least Popular Program in", 2011 + i), x="", y="Mean of Program Percentages")+ 
    theme(plot.title=e?ement_text(hjust=0.5,size = 10))+ 
    geom_text(aes(Features, Mean_of_program_percentages,label = Mean_of_program_percentages , vjust = -0.1, hjust = 0.5),size = 4)+
    scale_x_discrete(breaks=c("program_percentage.architecture", "program_percentage.ethn?c_cultural_gender", "program_percentage.library","program_percentage.military","program_percentage.science_technology"),
                     labels=c("architecture", "ethnic cultural gender", "library","military","science technology"))
}
grid.arrange(Pro[?1]],Pro[[2]],Pro[[3]],Pro[[4]],Pro[[5]],nrow=2)


#### try one plot

least_popular_2014 =  least_popular_2[least_popular_2[,'Year'] == "2014",]

ggplot(least_popular_2014, aes(x=Features, y=Mean_of_program_percentages,fill = Features)) +guides(fill = "none?) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","paleturquoise3","rosybrown2")) +
   geom_text(aes(label = Mean_of_program_percentages , vjust = -0.4, hjust = 0.5),size = 4)+
  scale_x_discrete(breaks=c("program?percentage.architecture", "program_percentage.ethnic_cultural_gender", "program_percentage.library","program_percentage.military","program_percentage.science_technology"),
                   labels=c("architecture", "ethnic_cultural_gender", "library","military","science_technology"))

##################################  4 
tuition = data[,c("id","state","tuition.in_state","tuition.out_of_state","academic_year")]

# add "tuition.in_state","tuition.out_of_state" into "total_tuition"

tuition$tuition.in_state[?s.na(tuition$tuition.in_state)] = 0
tuition$tuition.out_of_state[is.na(tuition$tuition.out_of_state)] = 0

tuition$total_tuition = tuition$tuition.in_state + tuition$tuition.out_of_state
tuition$total_tuition[tuition$total_tuition == 0] = NA

var_tuition =?tuition %>% group_by(state) %>%
  mutate(Var_tuition = var(total_tuition,na.rm = TRUE))
var_tuition = var_tuition[,c("state","Var_tuition","academic_year")]
var_tuition = unique(var_tuition)


var_tuition_most = var_tuition %>% group_by(academic_year) %>% top_n(5, Var_tuition)
var_tuition_least = var_tuition %>% group_by(academic_year) %>% top_n(-5, Var_tuition)


tuition = data[,c("id","state","tuition.in_state","tuition.out_of_state","academic_year")]
# var of tuition.in_state
var_tuition.in_state = tuitio? %>% group_by(state) %>%
  mutate(Var_instate = var(tuition.in_state,na.rm = TRUE))
var_tuition.in_state = var_tuition.in_state[,c("state","Var_instate","academic_year")]
var_tuition.in_state = unique(var_tuition.in_state)


# var of tuition.out_of_state
v?r_tuition.out_of_state = tuition %>% group_by(state) %>%
  mutate(Var_outstate = var(tuition.out_of_state,na.rm = TRUE))
var_tuition.out_of_state = var_tuition.out_of_state[,c("state","Var_outstate","academic_year")]
var_tuition.out_of_state = unique(var_t?ition.out_of_state)

# the most/least 5 var tuition.in_state
var_tuition.in_state_most = var_tuition.in_state %>% group_by(academic_year) %>% top_n(5, Var_instate)
var_tuition.in_state_least = var_tuition.in_state %>% group_by(academic_year) %>% top_n(-5, ?ar_instate)

var_tuition.out_of_state_most = var_tuition.out_of_state %>% group_by(academic_year) %>% top_n(5, Var_outstate)
var_tuition.out_of_state_least = var_tuition.out_of_state %>% group_by(academic_year) %>% top_n(-5, Var_outstate)


#relationship b?tween the number of universities in a state and total tuition
tuition
mean_tuition = tuition %>% group_by(state) %>%
  mutate( mean_total_tuition = mean(total_tuition,na.rm = TRUE))
mean_tuition = mean_tuition[,c("state","mean_total_tuition")]
mean_tuition?= unique(mean_tuition)
mean_tuition = mean_tuition[order(mean_tuition$mean_total_tuition),]
table(tuition$state)

##### 5
race = data[,c("id","name","demographics.race_ethnicity.white","demographics.race_ethnicity.black",
               "demographics.race_?thnicity.hispanic","demographics.race_ethnicity.asian","demographics.race_ethnicity.aian",
               "demographics.race_ethnicity.nhpi","demographics.race_ethnicity.unknown")]
#Find the college which has the most variance of the percentage of race
app?y(race[,3:9],1,var)

race$name[which.max(apply(race[,3:9],1,var))]  # "Marsha Kay Beauty College"

# replace NA to 0
# race[is.na(race)] = 0

# use men/women to get the ratio to measure the diversity of demographics
wm =  data[,c("id","name","demographics.?en","demographics.women")]
wm$ratio = wm$demographics.men/wm$demographics.women
wm = wm[order(wm$ratio),] 
# if the ratio are close to 1, the school would have more diversity.

### 6 (a)
data_ca = data[data[,"state"] == 'CA',]
data_year = list()
Program = ?ULL
for (i in (1:5)){
  data_year[[i]] = data_ca[data_ca[,'academic_year'] == 2011+i,]
  for (j in (47:84)){
    temp<-mean(data_year[[i]][,j], na.rm = TRUE)
    temp<-as.data.frame(temp)
    temp$Features<-colnames(data_year[[i]])[j]
    temp$Year <- 2011?i
    Program<-rbind(Program,temp)
  }
}

Program$temp = round(Program$temp,5)
names(Program)[names(Program) == "temp"] = "Mean_of_program_percentages"

# the most/least 5 popular programes
most_popular_ca = Program %>% group_by(Year) %>% top_n(5, Mean_of_?rogram_percentages)
least_popular_ca = Program %>% group_by(Year) %>% top_n(-5, Mean_of_program_percentages)


#################### loop to plot 5 graphs of the most popular program  
data_year = list()
for (i in (1:5)){
  data_year[[i]] = most_popular_ca[?ost_popular_ca[,'Year'] == 2011+i,]
}
Pro = list()
for (i in (1:5)){
  Pro[[i]] = ggplot(data_year[[i]]) +
    geom_bar( aes(x=Features, y=Mean_of_program_percentages,fill = Features),stat="identity") + scale_fill_manual(values=c("#999999", "#E69F00", "#56?4E9","paleturquoise3","rosybrown2")) +
    guides(fill = "none") +
    labs(title = paste("The Most Popular Program in California, in", 2011 + i), x="", y="Mean of Program Percentages")+ 
    theme(plot.title=element_text(hjust=0.5,size = 10)) + theme(axis?text.x = element_text(angle = 30))+
    geom_text(aes(x=Features, y=Mean_of_program_percentages,label = Mean_of_program_percentages , vjust = 0, hjust = 0.5),size = 4)
}
grid.arrange(Pro[[1]],Pro[[2]],Pro[[3]],Pro[[4]],Pro[[5]],nrow=2)

###################? loop to plot 5 graphs of the least popular program
data_year = list()
for (i in (1:5)){
  data_year[[i]] = least_popular_ca[least_popular_ca[,'Year'] == 2011+i,]
}
Pro = list()
for (i in (1:5)){
  Pro[[i]] = ggplot(data_year[[i]]) +
    geom_bar(aes(Featu?es, Mean_of_program_percentages,fill = Features),stat="identity") + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","paleturquoise3","rosybrown2")) +
    guides(fill = "none") + theme(axis.text.x = element_text(angle = 30))+
    labs(title = pas?e("The least Popular Program in", 2011 + i), x="", y="Mean of Program Percentages")+ 
    theme(plot.title=element_text(hjust=0.5,size = 10))+ 
    geom_text(aes(Features, Mean_of_program_percentages,label = Mean_of_program_percentages , vjust = 0, hjust =?0.5),size = 4)
}
grid.arrange(Pro[[1]],Pro[[2]],Pro[[3]],Pro[[4]],Pro[[5]],nrow=2)

# 6 (b)
ca_2016 = data_ca[data_ca[,'academic_year'] == "2016",]
data_ca$tuition.in_state[which(data_ca$name=="University of California-Davis")]  # 14046
data_ca$tuition.out?of_state[which(data_ca$name=="University of California-Davis")] # 40728

# get UCD row
which(ca_2016$name=="University of California-Davis")
ca_2016[50,]

hist1 = ggplot(ca_2016, aes(x = as.factor(id), y = tuition.in_state )) + geom_point()+
  theme(axis.t?cks.x = element_blank() )+ theme(plot.title=element_text(hjust=0.5,size =20))+
  labs(title="In State Tuition for the California Universities: 2016",x="Universities",y="In State Tuition")+
  theme(axis.text.x = element_blank() )+
  geom_point(data = ca_201?[50,], aes(x = as.factor(id), y = tuition.in_state ), colour = "#E69F00", size = 5) +
  annotate("text",x=90,y=17000,label ="University of California-Davis 
           in state tuition : 14046 ",size = 6)

hist2 = ggplot(ca_2016, aes(x = as.factor(id), y =?tuition.out_of_state )) + geom_point()+
  theme(axis.ticks.x = element_blank() )+ theme(plot.title=element_text(hjust=0.5,size =20))+
  labs(title="Out of State Tuition for the California Universities: 2016",x="Universities",y="Out of State Tuition")+
  th?me(axis.text.x = element_blank() ) +
  geom_point(data = ca_2016[50,], aes(x = as.factor(id), y = tuition.out_of_state ), colour = "#E69F00", size = 5)+
  annotate("text",x=90,y=45000,label ="University of California-Davis 
           out of state tuition ? 40728 ",size = 6)

mean_instate_tuition = mean(ca_2016$tuition.in_state,na.rm = TRUE)   # 14625
mean_outofstate_tuition = mean(ca_2016$tuition.out_of_state,na.rm = TRUE) # 18376


# 8 (a)
data_Q8 = data[,c("id","name","tuition.out_of_state","demographics.?ace_ethnicity.white","demographics.race_ethnicity.black",
                   "demographics.race_ethnicity.hispanic","demographics.race_ethnicity.asian","demographics.race_ethnicity.aian",
                   "demographics.race_ethnicity.nhpi","demographics.?ace_ethnicity.unknown","academic_year")]

data_Q8$diversity_race = apply(data_Q8[,4:10],1,var)
data_Q = data_Q8[,c("id","name","tuition.out_of_state","diversity_race",'academic_year')]

data_year = list()
for (i in (1:5)){
  data_year[[i]] = data_Q[data_Q[?'academic_year'] == 2011+i,]
}
Pro = list()
for (i in (1:5)){
  Pro[[i]] = ggplot(data_year[[i]]) +
    geom_point( aes(x=diversity_race, y=tuition.out_of_state))  + geom_smooth(aes(x=diversity_race, y=tuition.out_of_state),method='lm')+
    labs(title = p?ste("Relationship between Out of State Tuition and Race Diversity, in", 2011 + i), x="Race Diversity", y="Out of State Tuition")+ 
    theme(plot.title=element_text(hjust=0.5,size = 10)) + theme(axis.text.x = element_blank())
}
grid.arrange(Pro[[1]],Pro[[2?],Pro[[3]],Pro[[4]],Pro[[5]],nrow=2)


# calculate the cor of race diversity and out of state tuition
cor(data_year[[1]]$diversity_race, data_year[[1]]$tuition.out_of_state,use = "complete.obs") # -0.16720
cor(data_year[[2]]$diversity_race, data_year[[2]]$?uition.out_of_state,use = "complete.obs") # -0.17135
cor(data_year[[3]]$diversity_race, data_year[[3]]$tuition.out_of_state,use = "complete.obs") # -0.18209
cor(data_year[[4]]$diversity_race, data_year[[4]]$tuition.out_of_state,use = "complete.obs") # -0.1?415
cor(data_year[[5]]$diversity_race, data_year[[5]]$tuition.out_of_state,use = "complete.obs") # -0.20903


# (b)
data_ownership = data[,c("id","name","state","tuition.in_state","ownership",'academic_year')]
ownership_2016 = data_ownership[data_ownership?,"academic_year"] == '2016',]
ownership_ca_2016 = ownership_2016[ownership_2016[,"state"] == 'CA',]
ownership_ca_2016 = na.omit(ownership_ca_2016)
id = as.factor(ownership_ca_2016$id)
ownership = as.factor(ownership_ca_2016$ownership)

ggplot(ownership_ca_?016,aes(x=reorder(id,-tuition.in_state),y=tuition.in_state,fill=ownership))+
  geom_bar(stat="identity")+labs(title="In State Tuition Distribution in California Schools by Ownership in 2016",x="Schools",y="In State Tuition")+
  theme(plot.title=element_tex?(hjust=0.5,size=20))+theme(axis.text.x = element_blank() )+
  scale_fill_manual(values = c("#8FBC94", "lightskyblue","darkorange"))+theme(axis.ticks.x = element_blank())+
  geom_hline(yintercept=14625,color="black",size=1.5)+ 
  annotate('text',x=230,y=170?0,label="California Satewide Average 14625",size=5)+
  theme(legend.position = c(1, 1),legend.justification = c(1,1))

# calculate the California statewide average tuition-in-state
mean(ownership_ca_2016$tuition.in_state,na.rm = TRUE)





