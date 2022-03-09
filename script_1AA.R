### Time Series for Offenbach popularity

charts <- read_tsv('/cloud/project/raw/offenbach.tsv')

charts_total <- charts %>%
  filter(COUNTRY_CODE %in% c("FR", "LU", "LT",
                             "DE", "PL", "BE",
                             "CH", "AT", "WW")) %>%
  filter(ACCOUNT == "Spotify") %>%
  filter(TITLE == "Head Shoulders Knees & Toes (feat. Norma Jean Martine)") %>%
  select(COUNTRY_CODE, CURRENT_POSITION, DATE_KEY)

##reshape this
test <- charts_total %>%
  select(CURRENT_POSITION, COUNTRY_CODE, DATE_KEY) %>%
  group_by_at(vars(-CURRENT_POSITION)) %>%
  dplyr::mutate(row_id = 1:n()) %>%
  ungroup() %>%
  spread(key = COUNTRY_CODE, value = CURRENT_POSITION)
test[is.na(test)] = 200
test<- as.data.frame(test)
plot(ts(test[,3]), ylab = "Streams", main = "Ghost Town Weekly Streams", ylim = c(0,200),
     lty = 1, lwd = 2, col = "red")
lines(ts(test[4]), lty = 2, lwd = 2, col = "blue")
lines(ts(test[5]), lty = 2, lwd = 2, col = "green")
legend("bottomright", legend = c("GB streams", "NZ streams", "US streams"),
       col = c("red", "blue", "green"), lty = 1, cex = 1,
       pt.cex = 6, pch = 20)
##
#### peaks in the DE, nordic countries, before it peaks WW
model1 <- lm(WW ~ FR + LU + LT + BE, data = test)
summary(model1)

### add time variables
test$stime <- c(1:118)

model1 <- lm(WW ~ FR + LU + LT + BE + I(stime) + I(stime^2) + I(stime^3), data = test)
model1 <- lm(WW ~ FR + I(stime) + I(stime^2) + I(stime^3), data = test)
summary(model1)

### arima example with multiple predictor varibles
xreg <- as.matrix(test[,c("FR", "AT", "BE")])
forecast::auto.arima(test$WW, xreg = xreg)
lm(WW ~ FR + AT, data = test) %>%
  resid %>%
  acf
