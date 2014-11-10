dat_train <- read.table('BikeSharing_train.csv',sep=',',stringsAsFactors = FALSE,header=T)
dat_test <-read.table('BikeSharing_test.csv',sep=',',stringsAsFactors = FALSE,header=T)


require(stringr)
require(lubridate)
require(useful)
require(glmnet)
require(plyr)

dat <- join(dat_train,dat_test,by ='datetime',type = 'full')
dat <- dat[order(dat$datetime),]
Hour <- hour(dat$datetime)
yDay <- yday(dat$datetime)
dat <- data.frame(dat,Hour=Hour,yDay=yDay)

known_index <- !is.na(dat$count)
unknown_index <- is.na(dat$count)
dat_unknown <- subset(dat,unknown_index)
dat_known <- subset(dat,known_index)
Y <- dat_known$count


## input data and se
local_linear <- function(dat_train,s_range=3,s_hour,y_day)
{
    #s_hour, y_day is from test data
    if (dim(subset(dat_train,(Hour==s_hour)&(abs(yDay-y_day)<s_range)))[1]!=0){
        return(mean(subset(dat_train,(Hour==s_hour)&(abs(yDay-y_day)<s_range))$registered))#response casual
    }
   else{
       return(local_linear(dat_train,s_range+1,s_hour,y_day))
   }
}

## 
local_model <- function(dat=dat_known,
                        s_range=5,
                        train_index={set.seed(1);sample(dim(dat)[1],size = dim(dat)[1]*0.8)},
                        train=NULL){
    #train model with dat[train,] test on dat[!train,]
    if(is.null(train)){
        train <- rep(FALSE,dim(dat)[1])
        train[train_index] <- TRUE
    }
    res<-vector()
    for (i in 1:NROW(dat[!train,])){
        res[i] <- local_linear(dat_train=dat[train,],s_range=s_range,
                               s_hour=dat[!train,]$Hour[i],y_day=dat[!train,]$yDay[i])
    }
    return(res)
}

##cv for range
cv.local <- function (fun, k = 5, data, cost, response = "y", ...) 
{
    folds <- data.frame(Fold = sample(rep(1:k, length.out = NROW(data))), 
                        Row = 1:NROW(data))
    error <- 0
    for (f in 1:max(folds$Fold)) {
        theRows <- folds$Row[folds$Fold == f]
        train <- rep(TRUE,dim(data)[1])
        train[theRows] <- FALSE 
        pred <- fun(data, train=train,...)
        theCost <- cost(data[theRows, response], pred)
        error <- error + theCost * (length(theRows)/NROW(data))
    }
    return(error)
}


error <- vector()
for (i in 1:8)
{
    error[i] <- cv.local(local_model,5,dat_known[1:1000,],
                         cost=cost.mse,response='count',s_range=i+5)
}

##train model
res_local <- local_model(dat=dat_known,s_range = 5,train = known_index)

