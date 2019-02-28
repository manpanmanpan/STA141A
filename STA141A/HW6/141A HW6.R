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
#    result = rbi?d(result,temp1)
#  }
#  return(result)
#}
#result_var = sapply(by_group,get_column_var)

# var_in_group
var_in_group = aggregate(train,list(train$V1),var)
var_in_group = var_in_group[,3:258]
var_in_group = apply(var_in_group,2,mean)

# var_between_group
te?p1 = aggregate(train,list(train$V1),mean)
grand_mean = apply(temp1,2,mean)

var_between_group = function(by_group,grand_mean){
  # INPUTS: group , grand_mean
  # OUTPUTS: 256 variance between groups
  between_group_var = rep(0,256)
  for (j in (2:257)){
  ? ntotal = 0
    for (i in (1:10)){
      temp1 = mean(by_group[[i]][,j])
      temp2 = (temp1-grand_mean[j])^2
      temp3 = temp2*nrow(by_group[[i]])
      ntotal = ntotal + temp3
    }
    temp4 = ntotal/9
    between_group_var[j-1] = temp4
  }
  return ?between_group_var)
}
var_between_group = var_between_group(by_group, grand_mean)

a = var_in_group/var_between_group # V66 is the most useful, V2 is the least useful

# Q3
label = train[,1]
dataset  = train[,2:257]
n = nrow(train)
x = matrix(dataset[101,],?ncol=256)
x = unlist(x)
temp1 = matrix(rep(x,n), ncol=256,byrow = T)
temp2 = matrix(data.matrix(dataset),ncol=256)
diffmat = (temp1 - temp2)^2
distance = sqrt(rowSums(diffmat))
sorteddisindex = order(distance)

k = 10
count = rep(0,k)
for (i in (1:k)){
  c?unt[i] = label[sorteddisindex[k]]
}
table(count)

predict_KNN = function(x,k){  # Euclidean Distance
  # INPUTS: predict points, k parameter 
  # OUTPUTS: predict label
  label = train[,1]
  dataset  = train[,2:257]
  n = nrow(train)
  temp1 = matrix(rep(x?n), ncol=256,byrow = T)
  temp2 = matrix(data.matrix(dataset),ncol=256)
  diffmat = (temp1 - temp2)^2
  distance = sqrt(rowSums(diffmat))
  sorteddisindex = order(distance)
  
  count = rep(0,k)
  for (i in (1:k)){
    count[i] = label[sorteddisindex[k]]
 ?}
  temp3 = table(count)[order(table(count))]
  temp4 = names(temp3)[length(temp3)]
  return(temp4)
}

test_label = test[,1]
test_dataset  = test[,2:257]

get_label = rep(0,nrow(test_dataset))
for (i in (1:nrow(test_dataset))){
  x =  unlist(matrix(test_da?aset[i,], ncol=256))
  get_label[i] = as.numeric(predict_KNN(x,5))
}

test_label = data.frame(cbind(test_label, get_label))
not_equal = test_label[!test_label$test_label==test_label$get_label,] # 216(k = 10)   187 (k = 5)
nrow(not_equal)/nrow(test_label)

?   
predict_KNN2 = function(x,k){  # Manhattan Distance
  label = train[,1]
  dataset  = train[,2:257]
  n = nrow(train)
  temp1 = matrix(rep(x,n), ncol=256,byrow = T)
  temp2 = matrix(data.matrix(dataset),ncol=256)
  diffmat = abs(temp1 - temp2)
  distanc? = rowSums(diffmat)
  sorteddisindex = order(distance)
  
  count = rep(0,k)
  for (i in (1:k)){
    count[i] = label[sorteddisindex[k]]
  }
  temp3 = table(count)[order(table(count))]
  temp4 = names(temp3)[length(temp3)]
  return(temp4)
}

get_label2 = r?p(0,nrow(test_dataset))
for (i in (1:nrow(test_dataset))){
  x =  unlist(matrix(test_dataset[i,], ncol=256))
  get_label2[i] = as.numeric(predict_KNN(x,10))
}

test_label2 = data.frame(cbind(test_label, get_label2))
not_equal2 = test_label2[!test_label2$te?t_label==test_label2$get_label2,] # 216

# Q4 Q3
library(caret)
subsets = createFolds(1:7291, k = 10, list = TRUE, returnTrain = FALSE)
err_rate = rep(0,10)

for (i in (1:10)){
  test_temp = dataset[subsets[[i]],]
  train_temp = dataset[-subsets[[i]],]
  g?t_label = rep(0,nrow(test_temp))   ##
  for (j in (1:nrow(test_temp))){
  x =  unlist(matrix(test_temp[j,], ncol=256))
  get_label[j] = as.numeric(predict_KNN(x,10,train_temp,label[-subsets[[i]]]))
  }
  ori_label = label[subsets[[i]]]
  labels = data.fram?(cbind(get_label, ori_label))
  not_equal = labels[!labels$get_label==labels$ori_label,]   
  temp = nrow(not_equal)/nrow(get_label)*100
  err_rate[i] = as.numeric(format(round(temp, 2), nsmall = 2))
}
# 4.58 3.69 3.49 3.99 2.64 4.24 3.44 3.14 3.09 3.89
me?n(err_rate) # 3.619

# label = train[,1]
# dataset  = train[,2:257]
predict_KNN = function(x,k,dataset,label){  # Euclidean Distance
  # INPUTS: predict points(row), k parameter,train dataset, train dataset's label
  # OUTPUTS: predict label
  n = nrow(dat?set)
  temp1 = matrix(rep(x,n), ncol=256,byrow = T)
  temp2 = matrix(data.matrix(dataset),ncol=256)
  diffmat = (temp1 - temp2)^2
  distance = sqrt(rowSums(diffmat))
  sorteddisindex = order(distance)
  
  count = rep(0,k)
  for (i in (1:k)){
    count[i] ? label[sorteddisindex[k]]
  }
  temp3 = table(count)[order(table(count))]
  temp4 = names(temp3)[length(temp3)]
  return(temp4)
}

predict_KNN2 = function(x,k,dataset,label){  # Manhattan distance
  # INPUTS: predict points(row), k parameter,train dataset,?train dataset's label
  # OUTPUTS: predict label
  n = nrow(dataset)
  temp1 = matrix(rep(x,n), ncol=256,byrow = T)
  temp2 = matrix(data.matrix(dataset),ncol=256)
  diffmat = abs(temp1 - temp2)
  distance = rowSums(diffmat)
  sorteddisindex = order(distan?e)
  
  count = rep(0,k)
  for (i in (1:k)){
    count[i] = label[sorteddisindex[k]]
  }
  temp3 = table(count)[order(table(count))]
  temp4 = names(temp3)[length(temp3)]
  return(temp4)
}

cv_error_knn = function(subsets,label,k){
  # INPUTS: m-fold subse?s, test dataset's label, k
  # OUTPUTS: mean error rate
  err_rate = rep(0,10)
  for (i in (1:10)){
    test_temp = dataset[subsets[[i]],]
    train_temp = dataset[-subsets[[i]],]
    get_label = rep(0,nrow(test_temp))   ##
    for (j in (1:nrow(test_temp)?){
      x =  unlist(matrix(test_temp[j,], ncol=256))
      get_label[j] = as.numeric(predict_KNN(x,k,train_temp,label[-subsets[[i]]]))
    }
    ori_label = label[subsets[[i]]]
    labels = data.frame(cbind(get_label, ori_label))
    not_equal = labels[!l?bels$get_label==labels$ori_label,]   
    temp = nrow(not_equal)/nrow(test_temp)*100
    err_rate[i] = as.numeric(format(round(temp, 2), nsmall = 2))
  }
  return(mean(err_rate))
}

# Q5
result = rep(0,15)
for (i in (1:15)){
  result[i] = cv_error_knn(subs?ts, label, i)
}


# k=1, 2.948 k=2 4.073 k=3 5.431 k=4  6.145 k=5 7.118 k=6 7.543  k=7 8.173

library(ggplot2)
ggplot(result) + geom_point(size = 3,color = 'tomato1')+
  labs(title = "Test Set Error Rates of Euclidean Distance by K Values ", x= "K Values",?y = "Error Rates")+
  theme(plot.title=element_text(hjust=0.5))

ggplot(result2) + geom_point(size = 3,color = 'tomato1')+
  labs(title = "Test Set Error Rates of Manhattan Distance by K Values ", x= "K Values", y = "Error Rates")+
  theme(plot.title=eleme?t_text(hjust=0.5))


# Q6
# ??????????????????????????????????????????????????????
test_dataset = test[,2:257]
test_label = test[,1]

predict_KNN3 = function(test_dataset,k,dataset,label){  # euclidean distance
  # INPUTS: predict points, k parameter,train?dataset, train dataset's label
  # OUTPUTS: predict label
  temp1 = as.matrix(dist(rbind(test_dataset, dataset),method = 'euclidean', upper = TRUE, diag = TRUE))
  distance = temp1[(nrow(test_dataset)+1):nrow(temp1),1:nrow(test_dataset)]
  sorteddisindex =?apply(distance,2,order)
  temp2 = sorteddisindex[1:k,]
  
  temp4 = rep(0,nrow(test_dataset))
  for (i in (1:ncol(temp2))){
    temp3 = table(label[temp2[,i]])[order(table(label[temp2[,i]]))]
    temp4[i] = as.numeric(names(temp3)[length(temp3)])
  }
  ret?rn(temp4)
}

err3 = function(test_dataset,k,dataset,label,test_label,func){
  get_label = func(test_dataset,k,dataset,label)
  ori_label = test_label
  labels = data.frame(cbind(get_label, ori_label))
  not_equal = labels[!labels$get_label == labels$ori_la?el,]
  temp = nrow(not_equal)/nrow(test_dataset)*100
  err = as.numeric(format(round(temp, 2), nsmall = 2))
  return(err)
} 

err_rate3 = rep(0,15)
for (i in (2:16)){
  err_rate3[i] = err3(test_dataset,i,dataset,label,test_label,predict_KNN3)
}
err_rate3
#?0.00 6.48 5.43 5.63 5.68 5.68 5.88 6.03 6.18 6.18 6.33 6.73 6.98 7.08 7.32 7.27

index = c(1:15)
err_rate_3 = data.frame(index,err_rate3[2:16])

library(ggplot2)
g1 = ggplot(err_rate_3,aes(x = index, y=err_rate3[2:16])) + geom_point(size = 3,color = 'tomat?1')+
  labs(title = "Test Set Error Rates of Euclidean Distance by K Values ", x= "K Values", y = "Error Rates")+
  theme(plot.title=element_text(hjust=0.5)) 

predict_KNN4 = function(test_dataset,k,dataset,label){  # manhattan distance
  # INPUTS: predict?points, k parameter,train dataset, train dataset's label
  # OUTPUTS: predict label
  temp1 = as.matrix(dist(rbind(test_dataset, dataset),method = 'manhattan', upper = TRUE, diag = TRUE))
  distance = temp1[(nrow(test_dataset)+1):nrow(temp1),1:nrow(test_da?aset)]
  sorteddisindex = apply(distance,2,order)
  temp2 = sorteddisindex[1:k,]
  
  temp4 = rep(0,nrow(test_dataset))
  for (i in (1:ncol(temp2))){
    temp3 = table(label[temp2[,i]])[order(table(label[temp2[,i]]))]
    temp4[i] = as.numeric(names(temp3)?length(temp3)])
  }
  return(temp4)
}

err_rate4 = rep(0,15)
for (i in (2:16)){
  err_rate4[i] = err3(test_dataset,i,dataset,label,test_label,predict_KNN4)
}
#  0.00 7.03 5.83 6.08 6.23 6.33 6.38 6.83 7.27 7.82 7.92 8.07 8.02 7.82 8.22 8.27
index = c(1:15)?err_rate_4 = data.frame(index,err_rate4[2:16])

library(ggplot2)
g2 = ggplot(err_rate_4,aes(x = index, y=err_rate4[2:16])) + geom_point(size = 3,color = 'tomato1')+
  labs(title = "Test Set Error Rates of Manhattan Distance by K Values ", x= "K Values", y ? "Error Rates")+
  theme(plot.title=element_text(hjust=0.5))

library(gridExtra)
grid.arrange(g1,g2,nrow = 1)



# ???????????????????????????????????????????????????????????????

# Q6
# Euclidean Distance
predict_KNN = function(x,k){
  # INPUTS: predict p?ints, k parameter 
  # OUTPUTS: predict label
  label = train[,1]
  dataset  = train[,2:257]
  n = nrow(train)
  temp1 = matrix(rep(x,n), ncol=256,byrow = T)
  temp2 = matrix(data.matrix(dataset),ncol=256)
  diffmat = (temp1 - temp2)^2
  distance = sqrt(ro?Sums(diffmat))
  sorteddisindex = order(distance)
  
  count = rep(0,k)
  for (i in (1:k)){
    count[i] = label[sorteddisindex[i]]
  }
  temp3 = table(count)[order(table(count))]
  temp4 = names(temp3)[length(temp3)]
  return(temp4)
}

input = test[,2:257?
error_rate = function(input,k,func){
  # INPUTS: test data , k , KNN function
  # OUTPUTS: error rate estimator
  get_label = rep(0,nrow(input))   ##
  for (j in (1:nrow(input))){
    x =  unlist(matrix(input[j,], ncol=256))
    get_label[j] = as.numeric(?unc(x,k))
  }
  ori_label = test[,1]
  labels = data.frame(cbind(get_label, ori_label))
  not_equal = labels[!labels$get_label==labels$ori_label,]   
  temp = nrow(not_equal)/nrow(input)*100
  err_rate = as.numeric(format(round(temp, 2), nsmall = 2))
  
  ?eturn(err_rate)
}

err_rate_E = rep(0,15)
for (i in (1:15)){
  err_rate_E[i] = error_rate(input,i,predict_KNN)
}
err_rate_E 
# 5.63  6.73  7.57  8.87  9.32 10.41 10.56 11.56 11.01 10.76 12.26 11.76 13.85 13.05 15.10
index = c(1:15)
err_rate_E = data.frame(?ndex,err_rate)

library(ggplot2)
ggplot(err_rate_E,aes(x = index, y=err_rate)) + geom_point(size = 3,color = 'tomato1')+
  labs(title = "Test Set Error Rates of Euclidean Distance by K Values ", x= "K Values", y = "Error Rates")+
  theme(plot.title=element?text(hjust=0.5))


# Manhattan distance
predict_KNN_M = function(x,k){
  # INPUTS: predict points, k parameter 
  # OUTPUTS: predict label
  label = train[,1]
  dataset  = train[,2:257]
  n = nrow(train)
  temp1 = matrix(rep(x,n), ncol=256,byrow = T)
  tem?2 = matrix(data.matrix(dataset),ncol=256)
  diffmat = abs(temp1 - temp2)
  distance = rowSums(diffmat)
  sorteddisindex = order(distance)
  
  count = rep(0,k)
  for (i in (1:k)){
    count[i] = label[sorteddisindex[i]]
  }
  temp3 = table(count)[order(tab?e(count))]
  temp4 = names(temp3)[length(temp3)]
  return(temp4)
}
input = test[,2:257]
err_rate_M = rep(0,15)
for (i in (1:15)){
  err_rate_M[i] = error_rate(input,i,predict_KNN_M)
}
err_rate_M
#  6.23  7.67  8.62  9.82 11.36 11.26 11.86 12.90 13.20 14.10?13.45 13.70 14.55 14.75 15.35
index = c(1:15)
err_rate_M = data.frame(index,err_rate_M)
ggplot(err_rate_M,aes(x = index, y=err_rate_M)) + geom_point(size = 3,color = 'tomato1')+
  labs(title = "Test Set Error Rates of Manhattan Distance by K Values ", x= "? Values", y = "Error Rates")+
  theme(plot.title=element_text(hjust=0.5))
