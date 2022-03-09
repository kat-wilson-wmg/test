
## Step 1: GT data over multiple countries, DV = streams
### GT data over multiple countries
charts <- read_tsv('/cloud/project/raw/weekly_ghosttown.tsv')
table(charts$COUNTRY_CODE)
charts_total <- charts %>%
  filter(COUNTRY_CODE %in% c("FR", "US", "GB", "PT")) %>%
  select(COUNTRY_CODE, TOTAL_STREAMS, DATE_KEY)
## Step 1A: reshape
test <- charts_total %>%
  select(TOTAL_STREAMS, COUNTRY_CODE, DATE_KEY) %>%
  group_by_at(vars(-TOTAL_STREAMS)) %>%
  dplyr::mutate(row_id = 1:n()) %>%
  ungroup() %>%
  spread(key = COUNTRY_CODE, value = TOTAL_STREAMS)
test[is.na(test)] = 0

## Step 2: Cross Covariance Function over time. The covariance for each country X each other country
### ccf function over time
cov(test[,3:6])
## Step 2A: heat map: if there is a covariance, strong at a time lag
## there is a strong time lag in GB and the US, for instance
mat<- data.matrix(cov(test[,3:6]))
nba_heatmap <- heatmap(mat, Rowv=NA, Colv=NA, col = cm.colors(256), 
                       scale="column", margins=c(5,10))

## Step 3: Now that we know that, then modelling will tell us, in concert with the other thing,
# the conditional effect. 


## Step 4: Normal Distribution for country A, and Distribution for B
plot(ts(test[,3]), ylab = "Streams", main = "FR X GB", ylim = c(0,2369275),
     lty = 1, lwd = 2, col = "red")
lines(ts(test[4]), lty = 2, lwd = 2, col = "blue")
plot(ts(test[,3]), ylab = "Streams", main = "FR X US", ylim = c(0,2369275),
     lty = 1, lwd = 2, col = "red")
lines(ts(test[3]), lty = 2, lwd = 2, col = "blue")

## step 5: Model them all together (use the prior ggplot code for this)
plot(ts(test[,3]), ylab = "Streams", main = "All Countries at Once", ylim = c(0,2369275),
     lty = 1, lwd = 2, col = "red")
lines(ts(test[4]), lty = 2, lwd = 2, col = "blue")
lines(ts(test[5]), lty = 2, lwd = 2, col = "green")
lines(ts(test[6]), lty = 2, lwd = 2, col = "pink")


## step 6: Fit models and see if it is sig (this isn't gonna work, 
#have to to the time series with the covariance)
model1 <- lm(GB ~ US, data = test)
model1 <- lm(US ~ PT, data = test)
summary(model1)

## step 7: notes on the DV of charts -> the volume on spotify is so large.. that charts don't matter
## instead, do resident level data (people aged 18-25 in group A, and 18-25 in group B), compute covariance again
## variability will be stronger


### step 8: if you are using streams, then re-scale the data. Either 1. by aggregate demand

## two plots
## ACF and CCF

## step 9: heat map plot, the stream count increase over time
### if you see that the streams increase in this country -> another country, then test that model






