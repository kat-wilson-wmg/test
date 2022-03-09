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

new<- ddply(small, .(COUNTRY_CODE), mutate, id = order(DATE_KEY)) %>%
   select(id, TOTAL_STREAMS)


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
small$time <- as.numeric(small$id)
attach(small)
## we don't want monthly dummies because there is no seasonal structure
# not sig past 5th degree polynomial term
model1<- lm(small[,6]~time+I(time^2)+I(time^3)+I(time^4));summary(model1)

## examine residuals
plot(ts(resid(model1),start=c(2021,1,11),freq=12),xlab="time",
     ylab="residu als",main="Residuals of Model 1")
acf(resid(model1))

head(predict(model1), n = 10)


### spectral density, we might want the seasonality of the music
### industry here, is there a time of the year when it peaks
### add more here
spectrum(resid(model1), span = 2)

## normal quantile plot of residuals
qqnorm(resid(model1))
qqline(resid(model1))
## null is normality
shapiro.test(resid(model1))

## anova model comparison (partial F)
# they are sig different, so choose the more complex model
model2<- lm(small[,6]~time+I(time^2)+I(time^3))
anova(model2,model1)

## seasonal indices are not of interest
### multiplicative decomposition model


#adding in country as an interaction variable
table(df$COUNTRY_CODE)
small <- df %>%
  filter(COUNTRY_CODE %in% c("US", "GB", "NZ", "CH", "RU", "BZ", "CA"))
table(df$COUNTRY_CODE)
small <- small %>%
  arrange(DATE_KEY)
pass.ts <- ts(small[6], start = c(2021,10), freq = 12)
plot(pass.ts, ylab = "streams", main = "weekly streams")
small<- ddply(small, .(COUNTRY_CODE), mutate, id = order(DATE_KEY))
small$time <- as.numeric(small$id)
attach(small)
### you can see which markets are predictive of total streams (going to be the big markets
## like china, GB, US)
model1<- lm(TOTAL_STREAMS~time+I(time^2)+I(time^3)+I(time^4)+
              COUNTRY_CODE);summary(model1)

### distributed lag sample (with countries)
test <- small %>%
  select(TOTAL_STREAMS, COUNTRY_CODE, DATE_KEY) %>%
  group_by_at(vars(-TOTAL_STREAMS)) %>%
  dplyr::mutate(row_id = 1:n()) %>%
  ungroup() %>%
  spread(key = COUNTRY_CODE, value = TOTAL_STREAMS)
test[is.na(test)] = 0
test<- as.data.frame(test)
plot(ts(test[,3]), ylab = "Streams", main = "Ghost Town Weekly Streams", ylim = c(0,2369275),
     lty = 1, lwd = 2, col = "red")
lines(ts(test[4]), lty = 2, lwd = 2, col = "blue")
lines(ts(test[5]), lty = 2, lwd = 2, col = "green")
legend("bottomright", legend = c("GB streams", "NZ streams", "US streams"),
       col = c("red", "blue", "green"), lty = 1, cex = 1,
       pt.cex = 6, pch = 20)
##
#### NZ popularity is predictive of Great Britian
new<- ddply(test, mutate, id = order(DATE_KEY)) %>%
   select(id, TOTAL_STREAMS)
class(new$id)
model1 <- lm(GB ~ US + NZ + RU + CA + CH, data = test)
## US and CH predictive of Canada
model1 <- lm(CA ~ GB + NZ + RU + US + CH, data = test)
summary(model1)
acf(model1)
test$DATE_KEY
head(predict(model1), DATE = "2022-03-03")

### autoregressive model
?arima








