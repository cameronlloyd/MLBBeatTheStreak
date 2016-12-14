library("ggplot2")
library("GGally")
library("lattice")
library('tree')
library('randomForest')
library('caret')
library('gbm')
library('pls')
library('stats')
library('reshape2')

set.seed(1)
x = read.csv("data.csv")
#Preprocessing
#Note that NA rows have already been omitted
x$hit = as.factor(x$hit)
colsToRemove = c(1,27,32)
x.sub = x[,-colsToRemove]

x.sub = subset(x.sub, sampleHandBatterCareer>summary(x.sub$sampleHandBatterCareer)[2]/2 & 
                 sampleHandPitcherCareer>summary(x.sub$sampleHandPitcherCareer)[2]/2 &
                 sampleHandBatter2015>summary(x.sub$sampleHandBatter2015)[2]/2 &
                 sampleHandPitcher2015>summary(x.sub$sampleHandPitcher2015)[2]/2 &
                 sampleHandBatter2014>summary(x.sub$sampleHandBatter2014)[2]/2 &
                 sampleHandPitcher2014>summary(x.sub$sampleHandPitcher2014)[2]/2)
#Save 30 days for testing
uniqueDays = unique(x.sub$Date)
samp = sample(1:length(uniqueDays),30)
days.test = uniqueDays[samp]
days.train = uniqueDays[-samp]
train.dat = subset(x.sub,Date %in% days.train)
test.dat = subset(x.sub,Date %in% days.test)

##Remove the Date column
train.dat = train.dat[,-29]



#Exploration
#Question 1:Are your predictors marginally useful in discriminating observations that get hits, from observations that don’t get hits?
a=ggplot(data=train.dat,aes(x=factor(hit),y=avgHandBatterCareer))+geom_boxplot()+xlab("Hit")
b=ggplot(data=train.dat,aes(x=factor(hit),y=avgHandPitcherCareer))+geom_boxplot()+xlab("Hit")
c=ggplot(data=train.dat,aes(x=factor(hit),y=parkFactor))+geom_boxplot()+xlab("Hit")
d=ggplot(data=train.dat,aes(x=factor(hit),y=batterAvgInMonth))+geom_boxplot()+xlab("Hit")
multiplot(a,b,c,d,cols=2)

#Investgating Interaction
ggplot(data=train.dat,aes(x=avgHandBatterCareer,y=avgHandPitcherCareer,color=hit))+geom_point()

#investigating Correlational Structure in the data
#Just grab the career data
careerX = train.dat[,1:8]
M=cor(careerX)
levelplot(M)


#Model selection
#Create the 7 folds
folds = createFolds(1:nrow(train.dat),k=7)

#DECISION TREE ANALYSIS
tree.cv <- function(folds,train.dat) {
  
  acc = c()
  for (i in 1:length(folds)) {
    trainI = unlist(folds[-i])
    tr.dat = train.dat[trainI,]
    te.dat = train.dat[-trainI,]
    rf.mod = tree(hit~.,data=tr.dat,control=tree.control(mindev = 0,nobs=nrow(tr.dat)))
    print(rf.mod)
    mod.pred = predict(rf.mod, newdata = te.dat)
    mod.pred = ifelse(mod.pred[,2]>.5,1,0)
    new.tpr = sum(mod.pred[which(mod.pred==1)]==te.dat$hit[which(mod.pred==1)])/length(te.dat$hit[which(mod.pred==1)])
    dat.pred = data.frame(model.pred = as.factor(mod.pred), true = te.dat$hit)
    print(table(dat.pred$model.pred,dat.pred$true))
    acc = c(acc,new.tpr)
    
  }
  
  return (mean(acc))
}

tree.tpr=tree.cv(folds,train.dat)


#RANDOM FOREST [We cross validate on the number of predictors considered at each split]
rf.cv <- function(folds,train.dat,mtry) {
  
  acc = c()
  for (i in 1:length(folds)) {
    trainI = unlist(folds[-i])
    tr.dat = train.dat[trainI,]
    te.dat = train.dat[-trainI,]
    rf.mod = randomForest(hit~.,data=tr.dat, mtry = mtry,n.tree=2000)
    mod.pred = predict(rf.mod, newdata = te.dat,type='response')
    new.tpr = sum(mod.pred[which(mod.pred==1)]==te.dat$hit[which(mod.pred==1)])/length(te.dat$hit[which(mod.pred==1)])
    dat.pred = data.frame(model.pred = as.factor(mod.pred), true = te.dat$hit)
    print(table(dat.pred$model.pred,dat.pred$true))
    acc = c(acc,new.tpr)
    
  }
  
  return (mean(acc))
}


#Try 5 different values for p
ps = c(5,10,15,20,27)
bestP = NA
maxAcc = 0
accs=c()
for (p in ps) {
  print(p)
  newAcc = rf.cv(folds,train.dat,p)
  accs = c(accs,newAcc)
  if (newAcc>maxAcc) {
    maxAcc = newAcc
    bestP = p
  }
}


#Boosting
#Attempting boosting
#We need to CV on interaction depth [interaction.depth]. In the boosting guide, it says to set lambda small [shrinkage], and cross validate on T [n.tree](the number of iterations)
#The smaller the shrinkage parameter, the better predictive performance, but the worse the computaitonal peofrmance
#Go with bernoulli loss for classification
#There is also the issue of randomely subsampling the data for each weak learner. This could improve predictive performance, (and also computataional performance)
#This is the bag.fraction variable. We will go with .5 (which is recommended by the author.)

#So the strategy is:
#bag.fraction=.5
#shrinkage = .01 (potentially make it smaller if algorithm is fast)
#Cross validate on a grid of interaction depth and number of iterations.
#For interaction depth, we will go with 1,2,4,6
#For number of iterations, we will go with 3,000, 5,000, 7,000, 9,000 iterations


boost.cv <- function(folds,train.dat,lambda,bag.frac,inter.depth,n.tr) {
  
  acc = c()
  for (i in 1:length(folds)) {
    trainI = unlist(folds[-i])
    tr.dat = train.dat[trainI,]
    te.dat = train.dat[-trainI,]
    rf.mod = gbm(hit~.,data=tr.dat,distribution="bernoulli",n.trees=n.tr,shrinkage=lambda,interaction.depth=inter.depth,bag.fraction=bag.frac)
    mod.pred = predict.gbm(rf.mod, newdata = te.dat,n.trees=n.tr,type="response")
    mod.pred = ifelse(mod.pred>.5,1,0)
    new.tpr = sum(mod.pred[which(mod.pred==1)]==te.dat$hit[which(mod.pred==1)])/length(te.dat$hit[which(mod.pred==1)])
    dat.pred = data.frame(model.pred = as.factor(mod.pred), true = te.dat$hit)
    acc = c(acc,new.tpr)
  }
  return (mean(acc))
}

#Run this before I go to bed
lambda=.01
bag.frac = .5
ids = c(1,2,4,6)
n.trees = c(3000,5000,7000,9000)
accs=c()

for (n.tr in n.trees) {
  for (id in ids) {
    print(paste("The interaction depth is",id,sep=""))
    print(paste("The number of trees is ",n.tr,sep=""))
    newAcc = boost.cv(folds,train.dat,lambda,bag.frac,id,n.tr)
    print(newAcc)
    accs = c(accs,newAcc)
  }
}

#Logistic Regression
#make hit a factor again
train.dat$hit = as.factor(train.dat$hit)
#We'll try out two models
formula1 = as.formula(hit~(avgHandBatterCareer+pitcherAvgInMonth+batterAvgInMonth)^2)
formula2 = as.formula(hit~.)
#Lets cross validate on the training set to select the number of predictors to sample from at each split
logit.cv <- function(folds,train.dat,formula) {
  
  acc = c()
  for (i in 1:length(folds)) {
    trainI = unlist(folds[-i])
    tr.dat = train.dat[trainI,]
    te.dat = train.dat[-trainI,]
    logit.mod = glm(formula,data=tr.dat,family=binomial)
    #print(summary(logit.mod))
    mod.pred = predict(logit.mod, newdata = te.dat,type="response")
    mod.pred = as.factor(ifelse(mod.pred>.5,1,0))
    new.tpr = sum(mod.pred[which(mod.pred==1)]==te.dat$hit[which(mod.pred==1)])/length(te.dat$hit[which(mod.pred==1)])
    #print(new.acc)
    dat.pred = data.frame(model.pred = as.factor(mod.pred), true = te.dat$hit)
    print(table(dat.pred$model.pred,dat.pred$true))
    acc = c(acc,new.tpr)
    
  }
  
  return (mean(acc))
}

logit.cv(folds,train.dat,formula1) #This is very slightly better
logit.cv(folds,train.dat,formula2)


#Streak Building with our best model
#Our best model based on 7-fold cross validation is a random forest with p=10 predictors

#Fit this model on the training set
final.mod.rf = randomForest(hit~.,data=train.dat, mtry = 10,n.tree=2000)
#Iterate through the days in the testing set
days = unique(test.dat$Date)



Evaluate <- function(thresh,predictions,subTest.hits){
  winningIndices = which(predictions[,2]>thresh)
  total = length(winningIndices)
  correct = sum(subTest.hits[winningIndices]==1)
  
  return (c(total,correct))
}

total = c(0,0,0,0,0,0) #50, 60, 70, 80, 90, 95
correct = c(0,0,0,0,0,0) #50, 60, 70, 80, 90, 95

streakPicksCorrectVector = c()
streakPicksCorrect = 0
for (day in days) {
  
  subTest = subset(test.dat,Date==day)
  predictions = predict(final.mod.rf, newdata = subTest,type='prob')
  print(predictions)
  eval50 = Evaluate(.50,predictions,subTest$hit)
  total[1] = total[1]+eval50[1]
  correct[1] = correct[1]+eval50[2]
  eval60 = Evaluate(.60,predictions,subTest$hit)
  total[2] = total[2]+eval60[1]
  correct[2] = correct[2]+eval60[2]
  eval70 = Evaluate(.70,predictions,subTest$hit)
  total[3] = total[3]+eval70[1]
  correct[3] = correct[3]+eval70[2]
  eval80 = Evaluate(.80,predictions,subTest$hit)
  total[4] = total[4]+eval80[1]
  correct[4] = correct[4]+eval80[2]
  eval90 = Evaluate(.90,predictions,subTest$hit)
  total[5] = total[5]+eval90[1]
  correct[5] = correct[5]+eval90[2]
  eval95 = Evaluate(.95,predictions,subTest$hit)
  total[6] = total[6]+eval95[1]
  correct[6] = correct[6]+eval95[2]
  
  
  
  winningIndexStreak = which.max(predictions[,2])
  print(winningIndexStreak)
  
  if (subTest$hit[winningIndexStreak]==1){
    streakPicksCorrect=streakPicksCorrect+1
    streakPicksCorrectVector = c(streakPicksCorrectVector,1)
  }
  else {
    streakPicksCorrectVector = c(streakPicksCorrectVector,0)
  }
  
  
}

#Analyzing the results
ResultFrame = data.frame(threshold = c(50,60,70,80,90,95),PredictedHit=total,NumCorrectHits=correct,TPR = correct/total)
ggplot(data=ResultFrame,
       aes(x=threshold,y=TPR,size=PredictedHit))+
  geom_point()+xlab("'hit' votes (%)")+ylab("True Positive Rate")+ggtitle("TPR vs Prediction Confidence")+scale_size_continuous(guide = guide_legend(title="Number of Observations"),range=c(5,15))
streakPicksCorrectVector




