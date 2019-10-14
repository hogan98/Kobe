train=read.table('/Users/chenrui/desktop/sta3011/data/train.txt',header=T)
test=read.table('/Users/chenrui/desktop/sta3011/data/test.txt',header=T)
pre.data=read.table('/Users/chenrui/desktop/sta3011/data/predict.txt',header=T)
pre.data=pre.data[,c(1,2,6,7,10:12,14,16:19,24,26,27)]
library(caret)


dmy <- dummyVars(" ~ .", data = train)
newtrain <- data.frame(predict(dmy, newdata = train))
dmy <- dummyVars(" ~ .", data = test)
newtest <- data.frame(predict(dmy, newdata = test))
pre.data=pre.data[,-15]
dmy <- dummyVars(" ~ .", data = pre.data)
newpre<-data.frame(predict(dmy, newdata = pre.data))
shot_made_flag=rep(NA,nrow(newpre))
newpre=cbind(newpre,shot_made_flag)

action_type.Turnaround.Finger.Roll.Shot=rep(0,nrow(newpre))
action_type.Driving.Hook.Shot=rep(0,nrow(newtest))
action_type.Turnaround.Finger.Roll.Shott=rep(0,nrow(newtest))
action_type.Turnaround.Hook.Shot=rep(0,nrow(newtest))
newpre=cbind(newpre,action_type.Turnaround.Finger.Roll.Shot)
newtest=cbind(newtest,action_type.Driving.Hook.Shot,action_type.Turnaround.Finger.Roll.Shott,action_type.Turnaround.Hook.Shot)



inactive=c()
for(i in 1:length(unique(train$action_type))){
  if(table(train$action_type)[[i]]<=5){
    inactive=c(inactive,names(table(train$action_type))[i])
  }
}
train$action_type=as.character(train$action_type)
test$action_type=as.character(test$action_type)
made$action_type=as.character(made$action_type)
for(i in 1:nrow(train)){
  if(train$action_type[i] %in% inactive){
    train$action_type[i]='other'
  }
}
for(i in 1:nrow(test)){
  if(test$action_type[i] %in% inactive){
    test$action_type[i]='other'
  }
}
for(i in 1:nrow(made)){
  if(made$action_type[i] %in% inactive){
    made$action_type[i]='other'
  }
}
train$action_type=as.factor(train$action_type)
test$action_type=as.factor(test$action_type)
made$action_type=as.factor(made$action_type)