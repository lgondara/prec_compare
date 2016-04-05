# install and load required packages
install.packages("e1071")
require(e1071)
install.packages("geepack")
require(geepack)
install.packages("randomForest")
require(randomForest)
install.packages("DTComPair")
require(DTComPair)
install.packages("CombinePValue")
require(CombinePValue)
install.packages("caret")
require(caret)
install.packages("foreign")
require(foreign)

# read in diabetic retinopathy data
diab.data=read.arff(url("https://archive.ics.uci.edu/ml/machine-learning-databases/00329/messidor_features.arff"))

# downloaded data does not have column names, append 'X' to all column names
colnames(diab.data) = paste("x", colnames(diab.data), sep="")

# divide data into test and train (30% and 70%)
smp_size <- floor(0.70 * nrow(diab.data))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(diab.data)), size = smp_size)
diab.train <- diab.data[train_ind, ]
diab.test <- diab.data[-train_ind, ]

## naive bayes
# train the model
model.dr.train=naiveBayes(xClass~., data=diab.train)
# use it on test data
model.dr.pred=predict(model.dr.train,diab.test)
# save results and append columns to test accuracy
dr.final.data=as.data.frame(cbind(diab.test,model.dr.pred))

## random forest
model.rf.train=randomForest(xClass~., data=diab.train, ntree=1000)
model.rf.pred=predict(model.rf.train,diab.test)
rf.final.data=as.data.frame(cbind(diab.test,model.rf.pred))

##prep data for precision comparison
data.use=as.data.frame(cbind(dr.final.data$xClass, dr.final.data$model.dr.pred, rf.final.data$model.rf.pred))

##n=1, w=2
##first class, recode data
data.use$dis=ifelse(data.use$V1==1,c(1), c(0))
data.use$y1=ifelse(data.use$V2==1,c(1), c(0))
data.use$y2=ifelse(data.use$V3==1,c(1), c(0))

# create a paired object
data.paired=tab.paired(d=dis, y1=y1, y2=y2, data=data.use)
# generalized score
pv.gs(data.paired)$ppv$p.value
# relative precision
pv.rpv(data.paired)$ppv$p.value

##secnod class
data.use$dis2=ifelse(data.use$V1==2,c(1), c(0))
data.use$y12=ifelse(data.use$V2==2,c(1), c(0))
data.use$y22=ifelse(data.use$V3==2,c(1), c(0))

data.paired=tab.paired(d=dis2, y1=y12, y2=y22, data=data.use)
pv.gs(data.paired)$ppv$p.value
pv.rpv(data.paired)$ppv$p.value

## To fit a GEE based regression model
## Create id variable

data.use$id=1:nrow(data.use)

## concatenate datasets for GEE and stack them vertically
data.1=as.data.frame(cbind(data.use$dis, data.use$y1, data.use$id))
data.1$test=1
data.2=as.data.frame(cbind(data.use$dis, data.use$y2, data.use$id))
data.2$test=2

data.merge=rbind(data.1,data.2)


### Fit GEE model
### order data
## class 1
data.gee=subset(data.merge,V2==1)
osub=order(as.integer(data.gee$V3))
data.gee=data.gee[osub,]

gee.model=geeglm(V1~test, data=data.gee, id=V3, family=binomial, corstr="independence")
summary(gee.model)


## class 2
## concatenate datasets for GEE
data.1=as.data.frame(cbind(data.use$dis2, data.use$y12, data.use$id))
data.1$test=1
data.2=as.data.frame(cbind(data.use$dis2, data.use$y22, data.use$id))
data.2$test=2

data.merge=rbind(data.1,data.2)


### Fit GEE model
### order data
data.gee=subset(data.merge,V2==1)
osub=order(as.integer(data.gee$V3))
data.gee=data.gee[osub,]

gee.model=geeglm(V1~test, data=data.gee, id=V3, family=binomial, corstr="independence")
summary(gee.model)


