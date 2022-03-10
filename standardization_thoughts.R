
library(tidyverse)
library(reshape)
charts <- read_tsv('/cloud/project/raw/weekly_offennbach.tsv')
charts_total <- charts %>%
  filter(COUNTRY_CODE %in% c("FR", "LU", "LT", "DE", "PL", "BE",
                             "CH",  "AT", "WW", "RO", "NO", "HU", "SE", "FI")) %>%
  filter(PRODUCT_TITLE == "Head Shoulders Knees & Toes (feat. Norma Jean Martine)") %>%
  select(COUNTRY_CODE, TOTAL_STREAMS, DATE_KEY)
## Step 1A: reshape
test <- charts_total %>%
  select(TOTAL_STREAMS, COUNTRY_CODE, DATE_KEY) %>%
  group_by_at(vars(-TOTAL_STREAMS)) %>%
  dplyr::mutate(row_id = 1:n()) %>%
  ungroup() %>%
  spread(key = COUNTRY_CODE, value = TOTAL_STREAMS)
test[is.na(test)] = 0

## describe
describe(test[,3:10], skew=FALSE)
cor(test[,3:10])

## scale each column
test$AT <- scale(test$AT)
test$BE <- scale(test$BE)
test$CH <- scale(test$CH)
test$DE <- scale(test$DE)
test$FI <- scale(test$FI)
test$FR <- scale(test$FR)
test$HU <- scale(test$HU)
test$LT <- scale(test$LT)
test$LU <- scale(test$LU)
test$NO <- scale(test$NO)
test$PL <- scale(test$PL)
test$RO <- scale(test$RO)
test$SE <- scale(test$SE)
cor(test[,3:10])
mean(test$AT_scaled)
describe(test[,3:15])


dat$test1<- pnorm(dat$AT)*100
mean(dat$test1)
sd(dat$test1)

mean(dat$test1)
# this simple scale makes it so that the sclaed values of x and y have the same mean and sd
mean(dat_s[,1])
mean(dat_s[,2])
mean(dat_s[,3])
mean(dat_s[,4])
dat_s <- scale(dat)
cor(scale(test[,3:15]))


