train=read.table('/Users/chenrui/desktop/sta3011/data/train.txt',header=T)
test=read.table('/Users/chenrui/desktop/sta3011/data/test.txt',header=T)
pre.data=read.table('/Users/chenrui/desktop/sta3011/data/predict.txt',header=T)
library(caret)
library(alr3)
library(glmnet)
library(leaps)

faclist=c(5,6,9,15,17)
for(i in faclist){
  train[,i]=as.factor(train[,i])
  test[,i]=as.factor(test[,i])
  pre.data[,i]=as.factor(pre.data[,i])
}

### full model
m1=glm(shot_made_flag~.,family='binomial',data=train)
mmps(m1)
summary(m1)
m1p=predict(m1,newdata=test,type='response')
m1p=as.factor(as.numeric(m1p>=.5))
confusionMatrix(m1p,as.factor(test$shot_made_flag))


###### m2 with log transformation for time remain
m2=glm(shot_made_flag~.+log(time_remain+1),family='binomial',data=train)
mmps(m2)
summary(m2)
m2p=predict(m2,newdata=test,type='response')
m2p=as.factor(as.numeric(m2p>=.5))
confusionMatrix(m2p,as.factor(test$shot_made_flag))

##stepwise
backwardAIC=step(m2,direction="backward", data=train,family='binomial')
backwardBIC=step(m2,direction="backward", data=train,family='binomial',k=log(nrow(train)))

##AIC
mAIC=glm(shot_made_flag ~ action_type + loc_x + period + season + shot_type + 
           shot_zone_area + shot_zone_basic + shot_zone_range + time_remain + 
           last + log(time_remain + 1),data=train,family='binomial')
summary(mAIC)
mAIC.p=predict(mAIC,newdata=test,type='response')
mAIC.p=as.factor(as.numeric(mAIC.p>=.5))
confusionMatrix(mAIC.p,as.factor(test$shot_made_flag))


###BIC
mBIC=glm(shot_made_flag ~ action_type + loc_y + shot_zone_range + time_remain + 
           log(time_remain + 1),data=train,family='binomial')
mBIC.p=predict(mBIC,newdata=test,type='response')
mBIC.p=as.factor(as.numeric(mBIC.p>=.5))
confusionMatrix(mBIC.p,as.factor(test$shot_made_flag))



#
best_sub_all=regsubsets(train[,-9],train$shot_made_flag,method='exhaustive')
a=summary(best_sub_all)
a



aa=as.data.frame(cbind(pre.data$shot_id,as.numeric(mBIC.p)))
colnames(aa)=c('shot_id','shot_made_flag')
write.csv(aa,file='/Users/chenrui/downloads/mbic.csv')




##### make k-fold cross validation
library(boot)
accu=c()
ex=c(rep(0,3),rep(1,7))
for(thre in seq(0.5,0.9,0.05)){
  ac=rep(0,10)
  for(i in 1:10){
    folds=seq(2055*(i-1)+1+sum(ex[1:i]),2055*i+ex[i])
    m1.fold=glm(shot_made_flag ~ action_type + shot_distance + shot_type + shot_zone_area + 
                  time_remain + log(time_remain + 1),family='binomial',data=train[-folds,])
    m1p.fold=predict(m1.fold,newdata=train[folds,],type='response')
    m1p.fold=as.factor(as.numeric(m1p.fold>=thre))
    ac[i]=as.numeric(confusionMatrix(m1p.fold,as.factor(train$shot_made_flag[folds]))$overall[1])
  }
  accu=c(accu,mean(ac))
}
cat('Best accuracy for m1',max(accu),'threshold:',.5+.05*(which.max(accu)-1))

accu=c()
ex=c(rep(0,3),rep(1,7))
for(thre in seq(0.5,0.9,0.05)){
  ac=rep(0,10)
  for(i in 1:10){
    folds=seq(2055*(i-1)+1+sum(ex[1:i]),2055*i+ex[i])
    m2.fold=glm(shot_made_flag ~ shot_made_flag~.+log(time_remain+1),family='binomial',data=train[-folds,])
    m2p.fold=predict(m2.fold,newdata=train[folds,],type='response')
    m2p.fold=as.factor(as.numeric(m2p.fold>=thre))
    ac[i]=as.numeric(confusionMatrix(m2p.fold,as.factor(train$shot_made_flag[folds]))$overall[1])
  }
  accu=c(accu,mean(ac))
}
cat('Best accuracy for m2',max(accu),'threshold:',.5+.05*(which.max(accu)-1))

accu=c()
ex=c(rep(0,3),rep(1,7))
for(thre in seq(0.5,0.9,0.05)){
  ac=rep(0,10)
  for(i in 1:10){
    folds=seq(2055*(i-1)+1+sum(ex[1:i]),2055*i+ex[i])
    m3.fold=glm(shot_made_flag ~ shot_zone_range + combined_shot_type,family='binomial',data=train[-folds,])
    m3p.fold=predict(m3.fold,newdata=train[folds,],type='response')
    m3p.fold=as.factor(as.numeric(m2p.fold>=thre))
    ac[i]=as.numeric(confusionMatrix(m3p.fold,as.factor(train$shot_made_flag[folds]))$overall[1])
  }
  accu=c(accu,mean(ac))
}
cat('Best accuracy for m3',max(accu),'threshold:',.5+.05*(which.max(accu)-1))


############# other model
library(e1071)
library(pROC)
sv=svm(shot_made_flag~shot_zone_area+shot_zone_basic+shot_type
       +combined_shot_type+shot_distance+shot_type+train$time_remain,type='C-classification',data=train)
sv.p=predict(sv,newdata=test,type='response')
confusionMatrix(sv.p,as.factor(test$shot_made_flag))

m3=glm(shot_made_flag~shot_zone_area+shot_zone_basic+shot_type
       +combined_shot_type+shot_distance+shot_type+time_remain+loc_x+loc_y,family='binomial')
mmps(m3)
