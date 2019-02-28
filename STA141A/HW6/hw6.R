# Q1
read_digits = function(file_name){ 
  # INPUTS: file name
  # OUTPUTS: data frame
  path = 'digits/'
  temp = paste('digits/',file_name,sep="")    
  data = read.table(paste(temp,'.txt',sep=""))
}
train = read_digits('train')
test = read_digits('test'?
# Q2
# Display graphically what each digit (0 through 9) looks like on average
by_group = split(train,train$V1) # each digit 0 through 9

result = NULL
get_column_mean = function(group) {
  # INPUTS: each digit group
  # OUTPUTS: each column mean of each ?roup
  for (i in (2:257)){
    temp1 = mean(group[,i])
    result = cbind(result,temp1)
  }
  return(result)
}
result = sapply(by_group,get_column_mean) #  column mean of all 10 groups
# change to 16*16 matrix and rotate
group = list()
rotate_clockwise = f?nction(x) { t(apply(x, 2, rev))} # #rotated 90 degrees
for (i in (1:10)){
  group[[i]] = rotate_clockwise(t(matrix(result[,i],16,16)))
}

par(mfrow=c(2,5))
plot = list()
for (i in (1:10)){plot[[i]] = image(group[[i]],axes = F, col = grey(seq(1,0,length=512?))}

#Which pixels seem the most/least likely to be useful for classification?

#get_column_var = function(group) {
# INPUTS: each digit group
# OUTPUTS: each column var of within group
# for (i in (2:257)){
#  temp1 = var(group[,i])
#    result = rbi?d(r??ult,temp1)
#  }
#  return(result)
#}
#result_var = sapply(by_group,get_column_var)

# var_in_group
var_in_group = aggregate(train,list(train$V1),var)
var_in_group = var_in_group[,3:258]
var_in_group = apply(var_in_group,2,mean)

# var_between_group
temp1 =?aggregate(train,list(train$V1),mean)
grand_mean = apply(temp1,2,mean)

var_between_group = function(by_group,grand_mean){
  # INPUTS: group , grand_mean
  # OUTPUTS: 256 variance between groups
  between_group_var = rep(0,256)
  for (j in (2:257)){
    nto?al = 0
    for (i in (1:10)){
      temp1 = mean(by_group[[i]][,j])
      temp2 = (temp1-grand_mean[j])^2
      temp3 = temp2*nrow(by_group[[i]])
      ntotal = ntotal + temp3
    }
    temp4 = ntotal/9
    between_group_var[j-1] = temp4
  }
  return (betw?en_group_var)
}
var_between_group = var_between_group(by_group, grand_mean)

a = var_in_group/var_between_group # V66 is the most useful, V2 is the least useful

# Q3
label = train[,1]
dataset  = train[,2:257]
test_label = test[,1]
test_dataset  = test[,2:?57]

predict_KNN = function(test_dataset,k,dataset,label, method){  
  # INPUTS: predict points, k parameter,train dataset, train dataset's label, euclidean/manhattan distance
  # OUTPUTS: predict label
  temp1 = as.matrix(dist(rbind(test_dataset, dataset)?method = method, upper = TRUE, diag = TRUE))
  distance = temp1[(nrow(test_dataset)+1):nrow(temp1),1:nrow(test_dataset)]
  sorteddisindex = apply(distance,2,order)
  temp2 = sorteddisindex[1:k,]
  
  temp4 = rep(0,nrow(test_dataset))
  for (i in (1:ncol(te?p2))){
    temp3 = table(label[temp2[,i]])[order(table(label[temp2[,i]]))]
    temp4[i] = as.numeric(names(temp3)[length(temp3)])
  }
  return(temp4)
}

# Q4 
library(caret)
subsets = createFolds(1:7291, k = 10, list = TRUE, returnTrain = FALSE)


cv_error?knn = function(subsets,dataset,label,k,method){
  # INPUTS: m-fold subsets, all train dataset, all train dataset's label, k, distance method
  # OUTPUTS: mean error rate
  err_rate = rep(0,10)
  for (i in (1:10)){
    test_temp = dataset[subsets[[i]],]
   ?train_temp = dataset[-subsets[[i]],]
    get_label = predict_KNN(test_temp,k,train_temp,label[-subsets[[i]]],method)
    ori_label = label[subsets[[i]]]
    labels = data.frame(cbind(get_label, ori_label))
    not_equal = labels[!labels$get_label==labels$o?i_label,]   
    temp = nrow(not_equal)/nrow(test_temp)*100
    err_rate[i] = as.numeric(format(round(temp, 2), nsmall = 2))
  }
  return(mean(err_rate))
}
cv_error_knn(subsets,dataset,label,2,"euclidean")  # 3.567

# euclidean distance
err_rate_cv_E = rep?0,15)
for (i in (2:16)){
  err_rate_cv_E[i] = cv_error_knn(subsets,dataset,label,i,"euclidean")
}
index = c(1:15)
errrate_CV_E = data.frame(index,err_rate_cv_E[2:16])
err_rate_cv_E
# 0.000 3.567 3.057 3.388 3.483 3.552 3.551 3.758 3.868 3.964 4.197 4.308 4?528 4.746 4.870 4.951


library(ggplot2)
ggplot(errrate_CV_E,aes(x = index, y=err_rate_cv_E[2:16])) + geom_point(size = 3,color = 'tomato1')+labs(title = "10-Fold Cross-validation Test Set Error Rates of Euclidean Distance by K Values ", x= "K Values", y =?"Error Rates")+
  theme(plot.title=element_text(hjust=0.5))

# manhattan distance
err_rate_cv_M = rep(0,15)
for (i in (2:16)){
  err_rate_cv_M[i] = cv_error_knn(subsets,dataset,label,i,"manhattan")
}
index = c(1:15)
errrate_cv_M = data.frame(index,err_rate?cv_M[2:16])

ggplot(errrate_cv_M,aes(x = index, y=err_rate_cv_M[2:16])) + geom_point(size = 3,color = 'tomato1')+  labs(title = "10-Fold Cross-validation Test Set Error Rates of Manhattan Distance by K Values ", x= "K Values", y = "Error Rates")+
  theme(p?ot.title=element_text(hjust=0.5)) 

# Q6
predict_KNN = function(test_dataset,k,dataset,label, method){  
  # INPUTS: predict points, k parameter,train dataset, train dataset's label, euclidean/manhattan distance
  # OUTPUTS: predict label
  temp1 = as.matr?x(dist(rbind(test_dataset, dataset),method = method, upper = TRUE, diag = TRUE))
  distance = temp1[(nrow(test_dataset)+1):nrow(temp1),1:nrow(test_dataset)]
  sorteddisindex = apply(distance,2,order)
  temp2 = sorteddisindex[1:k,]
  
  temp4 = rep(0,nrow(t?st_dataset))
  for (i in (1:ncol(temp2))){
    temp3 = table(label[temp2[,i]])[order(table(label[temp2[,i]]))]
    temp4[i] = as.numeric(names(temp3)[length(temp3)])
  }
  return(temp4)
}

err3 = function(test_dataset,k,dataset,label,test_label,method){
  ? INPUTS: predict points, k, train dataset, train label, test label, distance method
  # OUTPUTS: error rate estimator
  get_label = predict_KNN(test_dataset,k,dataset,label,method)
  ori_label = test_label
  labels = data.frame(cbind(get_label, ori_label))?  not_equal = labels[!labels$get_label == labels$ori_label,]
  temp = nrow(not_equal)/nrow(test_dataset)*100
  err = as.numeric(format(round(temp, 2), nsmall = 2))
  return(err)
} 
#####  euclidean distance
err_rate3 = rep(0,15)
for (i in (2:16)){
  err_ra?e3[i] = err3(test_dataset,i,dataset,label,test_label,"euclidean")
}
err_rate3
# 6.48 5.43 5.63 5.68 5.68 5.88?6.03 6.18 6.18 6.33 6.73 6.98 7.08 7.32 7.27

index = c(1:15)
err_rate_3 = data.frame(index,err_rate3[2:16])
ggplot(err_rate_3,aes(x = index, y=er?_rate3[2:16])) + geom_point(size = 3,color = 'tomato1')+  labs(title = "Test Set Error Rates of Euclidean Dis?ance by K Values ", x= "K Values", y = "Error Rates")+
  theme(plot.title=element_text(hjust=0.5)) 

########## manhattan distance
err_rate4 = rep?0,15)
for (i in (2:16)){
  err_rate4[i] = err3(test_dataset,i,dataset,label,test_label,"manhattan")
}
err_rate4
# 7.03 5.83 6.08 6.23 6.33 6.38 6.83 7.27 7.82 7.92 8.07 8.02 7.82 8.22 8.27
index = c(1:15)
err_rate_4 = data.frame(index,err_rate4[2:16])
ggpl?t(err_rate_4,aes(x = index, y=err_rate4[2:16])) + geom_point(size = 3,color = 'tomato1')+
  labs(title = "Test Set Error Rates of Manhattan Distance by K Values ", x= "K Values", y = "Error Rates")+
  theme(plot.title=element_text(hjust=0.5))












