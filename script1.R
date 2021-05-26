install.packages("rstudioapi", dependencies = TRUE)
library(rstudioapi)
currentDir <- dirname(getActiveDocumentContext()$path)
currentDir
setwd(currentDir)
studentnumber =787524
  fulldata = read.csv("HousePrices.txt", sep = " ", header = TRUE)
digitsum = function(x) sum(floor(x/10^(0:(nchar(x)-1)))%%10)

set.seed(studentnumber)
mysum = digitsum(studentnumber)

if((mysum %% 2) == 0) { # number is even
  rownumbers = sample(1:327,150,replace=F)
} else { # number is odd
  rownumbers = sample(309:585,150,replace=F)
}
mydata = fulldata[rownumbers,]

#1
#model for the median house price
#Use semiparametric flexible modelling 
#not treat covariates as random effects.

# 1.A Construct at least three different semiparametric models (the models are required to have at
# least one nonparametric component) and use AIC as a method to select a final semiparametric
# model. Describe which models were part of the search.
library(SemiPar)
priceHouse = mydata$PriceHouse
meanIncome = mydata$MeanIncome
healthSocial = mydata$HealthSocial
industries = mydata$Industries
hotelRestaurant = mydata$HotelRestaurant
region = mydata$Region
province = mydata$Province
municipality = mydata$Municipality
shops = mydata$Shops
bankruptcies = mydata$Bankruptcies
taxForms = mydata$TaxForms


plot(meanIncome,priceHouse)
plot(healthSocial,priceHouse)#not really significant
plot(industries,priceHouse)
plot(hotelRestaurant,priceHouse)
hist(priceHouse)
#fit a normal parametric additiv model linear regression

fit0 = lm(median(priceHouse) ~ meanIncome + healthSocial)
summary(fit0)
AIC0= as.numeric(-2*logLik(fit0) + 2*length(coef(fit0)))
AIC0

#NON Parametric model
fitsp0 = spm(priceHouse~ f(meanIncome) +f(healthSocial)) #sum of two univariate function, not a bivariate function
summary(fitsp0)
AICfitsp0 = -2*fitsp0$fit$logLik+2*fitsp0$aux$df.fit
AICfitsp0
fitsp0$info$pen$knots 
#knots are the x-axis values for non continuous data fitting in order to have a continuous one
par(mfrow = c(1,2))
plot(fitsp0)


#Semi Parametric Model
interact = meanIncome*healthSocial
fitsp1 = spm(priceHouse~ f(meanIncome) +f(healthSocial) +interact, spar.method = "ML")
# I parametrically add the interaction. the plot is linear because is parametric
summary(fitsp1)#pvalue is significant
par(mfrow = c(1,3))
plot(fitsp1, se = FALSE)
AICsp1 = -2*fitpar1$fit$logLik+2*fitpar1$aux$df.fit
AICsp1 #choose AIC più basso, interaction effect is not needed


fitsp2 = spm(priceHouse~ f(meanIncome)+f(healthSocial), spar.method = "ML")
#spar estimation method ML o compare the two AIC
AICfitsp2 = -2*fitsp2$fit$logLik+2*fitsp2$aux$df.fit
AICfitsp2

#all semiparametric models
fitsp3 = spm(priceHouse~ f(meanIncome) + healthSocial , spar.method = "ML")
plot(fitsp3)
fitsp4 = spm(priceHouse~ meanIncome+f(healthSocial) ,spar.method = "ML") 
#sum of two univariate function, not a bivariate function
plot(fitsp4)

AICfitsp3 = -2*fitsp3$fit$logLik+2*fitsp3$aux$df.fit
AICfitsp3 #BEST AIC
AICfitsp4 = -2*fitsp4$fit$logLik+2*fitsp4$aux$df.fit
AICfitsp4

summary(fitsp3) #1df, pvalues super low, it's a good linear model so I perfrom a normal linear regression
summary(fitsp4)

fit5 = lm(priceHouse~ meanIncome + healthSocial + I(healthSocial^2))
summary(fit5)
AIC5 = as.numeric(-2*logLik(fit5) + 2*length(coef(fit5)))
AIC5 #same AIC as fitsp3, same model
#a second order effect gives an higher AIC

#Since the non-parametric assumption sees no clear information about f,
#we want to estimate this smooth function using a penalized spline estimators.

library(splines)
knots.vec =c(fitsp3$info$pen$knots)
knots.vec = array(unlist(knots.vec), dim = c(nrow(knots.vec[[1]]), ncol(knots.vec[[1]]), length(knots.vec)))

fitsp3$fit$coef
cc

x = seq(from = 0 , to = 1 , length = 36)
n = length(x)
Xpoly = cbind(rep(1,n),x)
Xspl = outer(x,knots,"-")
Xspl = Xspl*(Xspl>0)
X = cbind(Xpoly,Xspl)
D = diag(c(0,0,rep(1,num.knots)))
lambda = 100
fit = X%*%solve(t(X)%*%X+lambda*D)%*%t(X)%*%y
plot(fit)

#B = bs(x, knots = knots.vec, degree = 1)
plot(x,B[,1],type = "l",lty = 1, ylim = c(-0.5,0.7),ylab = "",xlim = c(0,1.1))
for(j in 2:ncol(B)) lines(x,B[,j],lty = j)
points(knots.vec,rep(0,length(knots.vec)),pch = 2)

#EX 30bbb

fit9 = spm(priceHouse ~ f(meanIncome , healthSocial) )
plot(fit9)
AICfit9 = -2*fit9$fit$logLik+2*fit9$aux$df.fit
AICfit9#choose AIC più basso, interaction effect is not needed





#4 a
# alpha = 1 LASSO , alpha = 0 RIDGE , in between is the general version of elastic net
xmatrix <- model.matrix(priceHouse ~ 
                          meanIncome+
                          healthSocial+                       
                          industries +
                          hotelRestaurant +
                          region +
                          province +
                          shops +
                          bankruptcies +
                          taxForms ,
                        data=mydata)[,-1]

alpha = c(0,1,.5)
for (a in alpha){
  glmnet_fit  <- glmnet(xmatrix,priceHouse,alpha = a)
  cv_fit <- cv.glmnet(xmatrix,priceHouse,alpha = a, folds=5)
  
  coefficients <-coef(cv_fit,s = cv_fit$lambda.min)
  print(coefficients)
}
s <- cv_fit$lambda.min #minimum lambda value(the best, cross validation is included)
point <- as.numeric(coef(cv_fit , s = s))[-1]
#plotting the estimated coefficient as function of lambda
plot.tmp <- plot(glmnet_fit, xvar = "lambda" , label = TRUE)


#municipality leftover because variable for the "curious"
mydata$Province = as.factor(mydata$Province)
mydata$Region = as.factor(mydata$Region )
priceHouse = mydata$PriceHouse
meanIncome = mydata$MeanIncome
healthSocial = mydata$HealthSocial
industries = mydata$Industries
hotelRestaurant = mydata$HotelRestaurant
region = mydata$Region
province = mydata$Province
shops = mydata$Shops
bankruptcies = mydata$Bankruptcies
taxForms = mydata$TaxForms


logitRegression = lm(priceHouse ~ 
                        meanIncome+
                        healthSocial+                       
                        industries +
                        hotelRestaurant +
                        region +
                        province +
                        shops +
                        bankruptcies +
                        taxForms ,
                      data=mydata, 
                    family = binomial)
print(coef(logitRegression))
#I oder the data in a decreasing number of industries order and then I take the median value
library(glmnet)
mydata_grouped_1 <- mydata[order(mydata$Industries),]
newX_1 <- mydata[15,]
newX_1 <- as.matrix(newX_1)
for (a in alpha){
glm <-cv.glmnet(xmatrix,priceHouse,alpha = a, folds=5)#Coefficient NA --> Error
print(coef(glm))
pred1 <- predict(glm, newX_1, s = glm$lambda.min,type = "class")
print(pred1)
}


mydata_grouped_2 <- mydata[order(mydata$HotelRestaurant),]
newX_2 <- mydata[130,]
newX_2 <- as.matrix(newX_2)
for (a in alpha){
  glm <-cv.glmnet(xmatrix,priceHouse,alpha = a, folds=5)
  pred1 <- predict(glm,  newX_2, type = "response", s = glm$lambda.min)
  print(pred1)
}

pred <-predict(logitRegression, newx_1, interval = "confidence")





#2
y = mydata$PriceHouse
x6 = mydata$TaxForms
x9 = mydata$HealthSocial











