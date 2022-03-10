### test of lags



test$sample_one <- NA
test$sample_one_tw<- NA
length(test$AT)
x1<-rep(c(2,2,2,2,5,2,3,3,3))
hist(x1)
x2<-rep(c(1,1,6,8,5,5,5,5,5))
hist(x2)
## x2 lags x1
ccfvalues = ccf(x2,x1, type = "correlation")
ccfvalues

## here, k is the lag. k=-2, -7 is significantly negative correlate dwith y
# so, x, at 
set.seed(123)
x = arima.sim(model=list(0.2, 0, 0.5), n = 100)
hist(x)
y = arima.sim(model=list(0.4, 0, 0.4), n = 100)
hist(y)
ccf(x,y, type="correlation")
