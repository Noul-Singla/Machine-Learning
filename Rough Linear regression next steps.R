
sgditerations <- vector()
sgdlearning_rate <- seq(0.002,0.009,0.00005)
sgdb0 <- (sgdlearning_rate*0)
sgdb1 <- (sgdlearning_rate*0)
brk <- 0
for(j in 1:length(sgdlearning_rate)){
  
  for(k in 1 :1000){
    
    for( i in 1:length(ud100)){
      if( (2*sgdlearning_rate[j]* (y100[i]-(sgdb0[j]+sgdb1[j]*ud100[i]))) < 0.00001 & (2*sgdlearning_rate[j]* ((y100[i]-(sgdb0[j]+sgdb1[j]*ud100[i]))*ud100[i])) <  0.00001)
      {
        brk <- 1
        break
      }
      else{
        sgdb0[j] <- sgdb0[j] + (2*sgdlearning_rate[j]* (y100[i]-(sgdb0[j]+sgdb1[j]*ud100[i])))
        sgdb1[j] <- sgdb1[j] + (2*sgdlearning_rate[j]* ((y100[i]-(sgdb0[j]+sgdb1[j]*ud100[i]))*ud100[i]))
      }
    } 
    if(brk ==1){
      brk <- 0
      break} 
  }
  sgditerations <- rbind(sgditerations,k*i)
}

sgd <- as.data.frame(cbind(sgdlearning_rate,sgdb0,sgdb1,sgditerations))
rownames(sgd) <- seq(1,nrow(sgd),1)
sgd

par(mfrow=c(1,2))
with(bgd,plot(bgdlearning_rate,bgditerations))
with(sgd,plot(sgdlearning_rate,sgditerations))


par(mfrow=c(1,2))
with(bgd,plot(bgdlearning_rate,bgdb1))
with(sgd,plot(sgdlearning_rate,sgdb1))



print(c(b0,b1))

bgdalpha <- bgd[round(bgd$bgdb0,digits=3) == round(b0,digits=3) & round(bgd$bgdb1,digits=3) == round(b1,digits=3) ,]
bgdalpha <- bgdalpha[order(bgdalpha$V4),]

sgdalpha <- sgd[round(sgd$sgdb0,digits=2) == round(b0,digits=2) & round(sgd$sgdb1,digits=2) == round(b1,digits=2) ,]
sgdalpha <- sgdalpha[order(sgdalpha$V4),]

bgdalpha[1,1]
sgdalpha[1,1]

# 2.d

b1 <- vector()
b0 <- vector()
bgdb0 <- vector()
bgdb1 <- vector()
sgdb0 <- vector()
sgdb1 <- vector()
sgditerations <- vector()
bgditerations <- vector()

for(j in 1:200)
{
  
  bgdb0[j] <- 0
  bgdb1[j] <- 0
  sgdb0[j] <- 0
  sgdb1[j] <- 0
  
  ud100 <- runif(100,-2,2)
  y100 <- 2+(3*ud100)
  y100 <- y100 + rnorm(1,y100,2)
  
  b1numerator <- sum(y100 * (ud100-mean(ud100)))
  b1denom <- sum((ud100-mean(ud100))^2)
  b1[j] <- b1numerator/b1denom
  b0[j] <- mean(y100) - (b1[j]*mean(ud100))
  
  
  
  for( i in 1:1000){
    
    if( (2*bgdalpha[1,1]* sum(y100-(bgdb0[j]+bgdb1[j]*ud100))) < 0.0001 & (2*bgdalpha[1,1]* sum((y100-(bgdb0[j]+bgdb1[j]*ud100))*ud100)) <  0.0001){
      break;
    }
    else{
      bgdb0[j] <- bgdb0[j] + (2*bgdalpha[1,1]* sum(y100-(bgdb0[j]+bgdb1[j]*ud100)))
      bgdb1[j] <- bgdb1[j] + (2*bgdalpha[1,1]* sum((y100-(bgdb0[j]+bgdb1[j]*ud100))*ud100))
    }
    
  }
  bgditerations[j] <- i*length(ud100)
  
  for(k in 1 :1000){
    
    for( i in 1:length(ud100)){
      if( (2*sgdalpha[1,1]* (y100[i]-(sgdb0[j]+sgdb1[j]*ud100[i]))) < 0.00001 & (2*sgdalpha[1,1]* ((y100[i]-(sgdb0[j]+sgdb1[j]*ud100[i]))*ud100[i])) <  0.00001)
      {
        brk <- 1
        break
      }
      else{
        sgdb0[j] <- sgdb0[j] + (2*sgdalpha[1,1]* (y100[i]-(sgdb0[j]+sgdb1[j]*ud100[i])))
        sgdb1[j] <- sgdb1[j] + (2*sgdalpha[1,1]* ((y100[i]-(sgdb0[j]+sgdb1[j]*ud100[i]))*ud100[i]))
      }
    } 
    if(brk ==1){
      brk <- 0
      break} 
  }
  sgditerations[j] <- k*i
  
  
}

result200 <- cbind.data.frame(b0,b1,bgdb0,bgdb1,bgditerations,sgdb0,sgdb1,sgditerations)
# result200
par(mfrow = c(1,1))

true <- rep(3,200)

hist(true,xlim=c(1,5),ylim=c(0,200),breaks = 5,col = "red",main="Distribution for slope estimation by least sq. method")
hist(result200$b1,xlim=c(1,5),ylim=c(0,200),breaks = 5,col = "yellow",add=T)

hist(true,xlim=c(1,5),ylim=c(0,200),breaks = 5,col = "red",main="Distribution for slope estimation by batch gradient")
hist(result200$bgdb1,xlim=c(1,5),ylim=c(0,200),breaks = 50,add=T,col="yellow")

hist(true,xlim=c(1,5),ylim=c(0,200),breaks = 5,col = "red",,main="Distribution for slope estimation by stochastic gradient")
hist(result200$sgdb1,xlim=c(1,5),ylim=c(0,200),breaks = 100,add=T,col="yellow")

hist(result200$bgditerations,breaks = 100, col = "grey", main="Distribution of batch iterations for coefficient estimation")
hist(result200$sgditerations,breaks = 100,col = "grey", main="Distribution of stochastic iterations for coefficient estimation")


# 2.E

b1 <- vector()
b0 <- vector()
bgdb0 <- vector()
bgdb1 <- vector()
sgdb0 <- vector()
sgdb1 <- vector()
sgditerations <- vector()
bgditerations <- vector()

for(j in 1:200)
{
  
  bgdb0[j] <- 0
  bgdb1[j] <- 0
  sgdb0[j] <- 0
  sgdb1[j] <- 0
  
  ud300 <- runif(300,-2,2)
  y300 <- 2+(3*ud300)
  y300 <- y300 + rnorm(1,y300,2)
  
  b1numerator <- sum(y300 * (ud300-mean(ud300)))
  b1denom <- sum((ud300-mean(ud300))^2)
  b1[j] <- b1numerator/b1denom
  b0[j] <- mean(y300) - (b1[j]*mean(ud300))
  
  
  for( i in 1:1000){
    
    if( (2*bgdalpha[1,1]* sum(y300-(bgdb0[j]+bgdb1[j]*ud300))) < 0.0001 & (2*bgdalpha[1,1]* sum((y300-(bgdb0[j]+bgdb1[j]*ud300))*ud300)) < 0.0001){
      break;
    }
    else{
      bgdb0[j] <- bgdb0[j] + (2*bgdalpha[1,1]* sum(y300-(bgdb0[j]+bgdb1[j]*ud300)))
      bgdb1[j] <- bgdb1[j] + (2*bgdalpha[1,1]* sum((y300-(bgdb0[j]+bgdb1[j]*ud300))*ud300))
    }
    
  }
  bgditerations[j] <- i*length(ud300)
  
  for(k in 1 :1000){
    
    for( i in 1:length(ud300)){
      if( (2*sgdalpha[1,1]* (y300[i]-(sgdb0[j]+sgdb1[j]*ud300[i]))) < 0.00001 & (2*sgdalpha[1,1]* ((y300[i]-(sgdb0[j]+sgdb1[j]*ud300[i]))*ud300[i])) <  0.00001)
      {
        brk <- 1
        break
      }
      else{
        sgdb0[j] <- sgdb0[j] + (2*sgdalpha[1,1]* (y300[i]-(sgdb0[j]+sgdb1[j]*ud300[i])))
        sgdb1[j] <- sgdb1[j] + (2*sgdalpha[1,1]* ((y300[i]-(sgdb0[j]+sgdb1[j]*ud300[i]))*ud300[i]))
      }
    } 
    if(brk ==1){
      brk <- 0
      break} 
  }
  sgditerations[j] <- k*i
  
  
}

result300 <- cbind.data.frame(b0,b1,bgdb0,bgdb1,bgditerations,sgdb0,sgdb1,sgditerations)
# result200
par(mfrow = c(1,1))

true <- rep(3,200)

hist(true,xlim=c(1,5),ylim=c(0,200),breaks = 5,col = "red",main="Distribution for slope estimation by least sq. method")
hist(result300$b1,xlim=c(1,5),ylim=c(0,200),breaks = 5,col = "yellow",add=T)

hist(true,xlim=c(1,5),ylim=c(0,200),breaks = 5,col = "red",main="Distribution for slope estimation by batch gradient")
hist(result300$bgdb1,xlim=c(1,5),ylim=c(0,200),breaks = 50,add=T,col="yellow")

hist(true,xlim=c(1,5),ylim=c(0,200),breaks = 5,col = "red",,main="Distribution for slope estimation by stochastic gradient")
hist(result300$sgdb1,xlim=c(1,5),ylim=c(0,200),breaks = 100,add=T,col="yellow")

hist(result300$bgditerations,breaks = 100, col = "grey", main="Distribution of batch iterations for coefficient estimation")
hist(result300$sgditerations,breaks = 100,col = "grey", main="Distribution of stochastic iterations for coefficient estimation")


# 3.a

library(ggplot2)
dd <- as.data.frame(diamonds)
dd
with(dd,plot(color,price))
dim(dd)

# 3. b
carat1 <- with(dd,dd[carat > 0 & carat < quantile(carat,0.2),])
carat2 <- with(dd,dd[carat >= quantile(carat,0.2) & carat < quantile(carat,0.4),])
carat3 <- with(dd,dd[carat >= quantile(carat,0.4) & carat < quantile(carat,0.6),])
carat4 <- with(dd,dd[carat >= quantile(carat,0.6) & carat < quantile(carat,0.8),])
carat5 <- with(dd,dd[carat >= quantile(carat,0.8),])
#check that the number of rows are equal
#dim(rbind(carat1,carat2,carat3,carat4,carat5))
#plot the 6 boxplot to see the range and outliers. 
par(mfrow=c(1,6))
with(dd,plot(color,price))
with(carat1,plot(color,price))
with(carat2,plot(color,price))
with(carat3,plot(color,price))
with(carat4,plot(color,price))
with(carat5,plot(color,price))


# 3.c
with(dd,plot(carat,price))
with(dd,plot(color,price))

ggplot(dd,aes(carat,price))+geom_point()+geom_line(aes(dd$carat,predict(lmttt,data.frame(carat=dd$carat))))

ggplot(dd,aes(carat,price))+geom_point()

ggplot(dd,aes(carat,price))+geom_point(aes(colour=color))


with(dd,plot(price,carat))
with(dd,plot(carat,price))
with(dd,plot(log(carat),log(price)))
with(dd,plot(carat,log(price)))

plot.new()


lm1 <- lm(price~carat,data=dd)
predict1 <- predict(lm1,data=dd)
mean((predict1-dd$price)^2)
cor(predict1,dd$price)
qqnorm(lm1$residuals)
qqline(lm1$residuals)

lm2 <- lm(price~carat+color,data=dd)
predict2 <- predict(lm2,data=dd)
mean((predict2-dd$price)^2)
cor(predict2,dd$price)
qqnorm(lm2$residuals)
qqline(lm2$residuals)

lm3 <- lm(price~carat+I(carat^2)+(color),data=dd)
predict3 <- predict(lm3,data=dd)
mean((predict3-dd$price)^2)
cor(predict3,dd$price)
qqnorm(lm3$residuals)
qqline(lm3$residuals)

lm4 <- lm(log(price)~log(carat)+(color),data=dd)
predict4 <- predict(lm4,data=dd)
mean((predict4-log(dd$price))^2)
cor(predict4,log(dd$price))
qqnorm(lm4$residuals)
qqline(lm4$residuals)

lm5 <- lm(log(price)~log(carat),data=dd)
predict5 <- predict(lm5,data=dd)
mean((predict5-log(dd$price))^2)
cor(predict5,log(dd$price))
qqnorm(lm5$residuals,main="lm5")
qqline(lm5$residuals)





# 4.a

#credit <- read.csv("credit.csv")
set.seed(123)

str(credit)
#creditalt <- credit[,c(1:7,12:17)]
#str(creditalt)
sample_credit <- credit[credit$X %in% sample(credit$X,200),]
valid_credit <- credit[!credit$X %in% sample_credit$X,]
dim(sample_credit)
dim(valid_credit)

# 4.b

anyNA(sample_credit)

summary(sample_credit)
str(sample_credit)
cor(sample_credit[,c(2:7)])
pairs(sample_credit)
#Rating and limit are highly linearlycorrelated so hence excludint them
#rating is also moderately linear related to income, so removing that as well
#X is a sequence and hence should not be considered

lm1 <- with(sample_credit,lm(Balance~Rating))
summary(lm1)

#4.c
lmall<- lm(Balance~.,data=sample_credit)
lmall
qqnorm(lmall$residuals)
qqline(lmall$residuals)
sample_credit[order(lmall$residuals),]


# 4.d

# install.packages("leaps")
library("leaps")

varsel <- regsubsets(Balance~.,data=sample_credit)
summary(varsel)
str(summary(varsel))
str(varsel)
par(mfrow = c(1,5))
plot(summary(varsel)$rss,type="b")
plot(summary(varsel)$rsq,type="b")
plot(summary(varsel)$adjr2,type="b")
plot(summary(varsel)$bic,type="b")
plot(summary(varsel)$cp,type="b")

# we must go with4 variable model
summary(varsel)
# the four variable are income limit cards student
lm4 <- lm(Balance~Income+Limit+Cards+Student,data=sample_credit)
lm4

sample_credit_matrix <- model.matrix(Balance~.,data=sample_credit)


# using ridge model to select coeff for all variables

# install.packages("glmnet")
library("glmnet")
par(mfrow=c(1,1))
lbda <- 10^seq(-10,10,length = 100)

ridgedata <- glmnet(sample_credit_matrix,sample_credit$Balance,alpha=0,lambda=lbda)
plot(ridgedata,label=TRUE)

ridgecvout <- cv.glmnet(sample_credit_matrix,sample_credit$Balance, alpha = 0,nfolds=15)
plot(ridgecvout)

str(ridgecvout)

ridgecvout$lambda.min
bestridgedata <- glmnet(sample_credit_matrix,sample_credit$Balance,alpha=0,lambda=ridgecvout$lambda.min)
# plot(bestridgedata,label=TRUE)
bestridgedata$beta

log(ridgecvout$lambda.min)

# using lasso model to select variables and coeff

lassodata <- glmnet(sample_credit_matrix,sample_credit$Balance,alpha=1,lambda=lbda)
plot(lassodata,label=TRUE)

lassocvout <- cv.glmnet(sample_credit_matrix,sample_credit$Balance, alpha = 1,nfolds=15)
plot(lassocvout)

lassocvout$lambda.min
bestlassodata <- glmnet(sample_credit_matrix,sample_credit$Balance,alpha=1,lambda = lassocvout$lambda.min)
bestlassodata$beta
# plot(bestlassodata,label=TRUE)

log(lassocvout$lambda.min)

valid_credit_matrix <- model.matrix(Balance~.,valid_credit)

lm1predict <- predict(lm1,valid_credit)
lmallpredict <- predict(lmall,valid_credit)
ridgepredict <- predict(bestridgedata,valid_credit_matrix)
lassopredict <- predict(bestlassodata,valid_credit_matrix)

lm1SSE <-  sum((lm1predict-valid_credit$Balance)^2) 
lmallSSE <- sum((lmallpredict-valid_credit$Balance)^2) 
ridgeSSE <- sum((ridgepredict-valid_credit$Balance)^2)
lassoSSE <- sum((lassopredict-valid_credit$Balance)^2)
lm1MSE <-  mean(((lm1predict-valid_credit$Balance)^2) )
lmallMSE <- mean(((lmallpredict-valid_credit$Balance)^2) )
ridgeMSE <- mean(((ridgepredict-valid_credit$Balance)^2))
lassoMSE <- mean(((lassopredict-valid_credit$Balance)^2))

print(c(cor(lm1predict,valid_credit$Balance),cor(lmallpredict,valid_credit$Balance),cor(ridgepredict,valid_credit$Balance),cor(lassopredict,valid_credit$Balance)))
print(c(format(lm1SSE,big.mark=","),format(lmallSSE,big.mark=","),format(ridgeSSE,big.mark=","),format(lassoSSE,big.mark=",")))
print(c(lm1MSE,lmallMSE,ridgeMSE,lassoMSE))

plot(valid_credit$Balance,lmallpredict,ylab="Predicted value",main="all variable linear prediction")
plot(valid_credit$Balance,lassopredict,ylab="Predicted value",main="Lasso prediction")
par(mfrow=c(2,2))

# install.packages("gridExtra")
library("gridExtra")
grid1 <- ggplot(valid_credit,aes(valid_credit$Balance,lmallpredict,main="asd")) +geom_point(col="red")+xlab("Balance")+ylab("Predicted Balance")+ggtitle("precited via linear regression of variables")
grid2 <- ggplot(valid_credit,aes(valid_credit$Balance,lassopredict)) +geom_point(col="green")+xlab("Balance")+ylab("Predicted Balance")+ggtitle("precited via lasso model for selected variables")
grid3 <- ggplot(valid_credit,aes(valid_credit$Balance,ridgepredict)) +geom_point(col="green")+xlab("Balance")+ylab("Predicted Balance")+ggtitle("precited via ridge model for variables")
grid.arrange(grid1,grid2,grid3)


# 4.e
grid2 <- ggplot(valid_credit,aes(valid_credit$Balance,lassopredict)) +geom_point(col="green")+xlab("Balance")+ylab("Predicted Balance")+ggtitle("precited via lasso model for selected variables")
print(cor(lassopredict,valid_credit$Balance))
print(format(lassoSSE,big.mark=","))
print(lassoMSE)

