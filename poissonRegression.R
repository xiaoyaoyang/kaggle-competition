dat_train <- read.table('BikeSharing_train.csv',sep=',',stringsAsFactors = FALSE,header=T)
dat_test <-read.table('BikeSharing_test.csv',sep=',',stringsAsFactors = FALSE,header=T)


require(stringr)
require(lubridate)
require(useful)
require(glmnet)

Month <- month(dat_train[,1])
Hour <- hour(dat_train[,1])
wDay <- wday(dat_train[,1])


datn_train <- data.frame(Hour=Hour,Month=Month,wDay=wDay,dat_train[,-1])
datn_train$Hour <- as.factor(datn_train$Hour)
datn_train$Month <- as.factor(datn_train$Month)
datn_train$wDay <- as.factor(datn_train$wDay)
datn_train$season <- as.factor(datn_train$season)
datn_train$holiday <- as.factor(datn_train$holiday)
datn_train$workingday <- as.factor(datn_train$workingday)
datn_train$weather <- as.factor(datn_train$weather)

# 
# X <- build.x(formula = count~ Hour + Month + mDay + holiday + workingday + weather + temp + 
#             atemp + humidity + windspeed + holiday:temp,data=datn_train)
# Y <- build.y(formula = count~ Hour + Month + mDay + holiday + workingday + weather + temp + 
#                  atemp + humidity + windspeed + holiday:temp,data=datn_train)


X <- build.x(formula = count~ Hour + Month + wDay + holiday + workingday 
             + weather + temp + atemp + humidity +windspeed +,data=datn_train)
Y <- build.y(formula = count~ Hour + Month + wDay + holiday + workingday
             + weather + temp + atemp + humidity +windspeed,data=datn_train)

fit <- glm(data=datn_train,
           formula = count~ Hour + Month + wDay + holiday + 
               workingday + weather + temp + windspeed,family = 'poisson')

fit1 <- glmnet(x = X,y = Y,family = 'poisson')
pred <- predict(fit1,newx=X,s=0,type = 'response')


##weighted for unequal variance
resi <- (Y-pred)
(sum(resi^2))
vfit = lm(abs(resi)~X)
w <- abs(resi)^-2

plot(pred,Y,xlim=c(0,600),ylim=c(0,600))
