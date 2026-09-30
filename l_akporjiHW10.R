rm(list=ls())
#1
library(multtest)
data(golub)
grep('CDH11 Cadherin 11',golub.gnames [,2])
CDH11 <- golub[123,]
cor.coefficients <- apply(golub, 1, function(x) cor(x, CDH11))
cor.coefficients[123] <- NA
top2 <- order(cor.coefficients, decreasing = TRUE) [1:2]
golub.gnames[top2,2]
cor.coefficients[top2]
#2
options(timeout = 600)
install.packages("Sleuth2")
library(Sleuth2)
data(ex1605)
Age4IQ <- ex1605$Age4IQ
Age13IQ <- ex1605$Age13IQ
#2a
cor(Age4IQ, Age13IQ)
#2b
cor.test(Age4IQ, Age13IQ)
#2c
nboot <- 2000

boot.cor<-rep(NA,nboot)

data<- cbind(Age4IQ,Age13IQ) #Data set

for (i in 1:nboot){
  
  dat.star<-data[sample(1:nrow(data), replace=TRUE), ] # Resample the pairs together
  
  boot.cor[i] <- cor(dat.star[,1], dat.star[,2]) #Correlation on resampled data
  
}

quantile(boot.cor, c(0.025,0.975))
#3A
BMIQ <- ex1605$BMIQ
reg.fit <- lm(Age13IQ ~ BMIQ) 
reg.fit
#B
summary(reg.fit)
#C
75.0453 + 0.3654 * 95
#3D
predict(reg.fit, newdata = data.frame(BMIQ = 95), interval = "prediction", level = 0.90)
#3e
plot(reg.fit, which=2)
plot(reg.fit, which=1)
#4
install.packages("AER")
library(AER)
data(CASchools)
#A
reg.fit2 <- lm(math ~ expenditure + income, data = CASchools)
reg.fit2
summary(reg.fit2)
#D
predict(reg.fit2,
        newdata = data.frame(expenditure = 5000, income=20),
        interval = "confidence",
        level = 0.99)
