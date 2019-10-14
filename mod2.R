ais=read.csv('/Users/chenrui/desktop/sta3011/data/ais.txt',sep=' ')
attach(ais)
m1=glm(Sex~RCC+WCC+BMI,family='binomial')
summary(m1)

library(alr3)
mmps(m1)

m2=glm(Sex~RCC+WCC+log(BMI)+BMI,family='binomial')
summary(m2)
mmps(m2)



m3=glm(Sex~RCC+log(WCC)+BMI+log(BMI),family='binomial')
summary(m3)
mmps(m3)
