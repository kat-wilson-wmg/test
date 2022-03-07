### time series modeling

df <- read_tsv('/cloud/project/raw/weekly_ghosttown.tsv')
small <- df %>%
  filter(COUNTRY_CODE == "US")
small_sep <- separate(small, "DATE_KEY", c("Year", "Month", "Day"), sep = "-")
# ts way 1
attach(small)
small <- small %>%
  arrange(DATE_KEY)
pass.ts <- ts(small[6], start = c(2021,10), freq = 12)
plot(pass.ts, ylab = "streams", main = "weekly streams")
## ggplot way
p <- ggplot(small, aes(x=DATE_KEY, y=TOTAL_STREAMS)) +
  geom_line() + 
  xlab("") +
  labs(title = "Weekly Streams Ghost Town") +
  scale_x_date(date_labels = "%m-%d-%y",
               date_breaks = "month")
p

### on the basic pass.ts

## check residuals
acf(pass.ts)
checkresiduals(pass.ts)
## forecast new points
library(forecast)
rev_forecast <- forecast(pass.ts,7)
plot(forecast(pass.ts,7))
rev_forecast <- forecast(pass.ts,3)
plot(forecast(pass.ts,3))

### additive decomposition model

#build a numeric value
small<- ddply(small, .(COUNTRY_CODE), mutate, id = order(DATE_KEY))
attach(small)
small$time <- as.numeric(small$id)
## we don't want monthly dummies because there is no seasonal structure
# not sig past 5th degree polynomial term
model1<- lm(TOTAL_STREAMS~time+I(time^2)+I(time^3)+I(time^4));summary(model1)

## examine residuals
plot(ts(resid(model1),start=c(2021,1,11),freq=12),xlab="time",
     ylab="residu als",main="Residuals of Model 1")

## normal quantile plot of residuals
qqnorm(resid(model1))
qqline(resid(model1))
## null is normality
shapiro.test(resid(model1))

## estimated seasonal indices (for the month variable...)
## this is not of interest
## can plot the seasonal indices 







