## create a long dataset, with values as the country, then popularity
charts <- read_tsv('/cloud/project/raw/offenbach.tsv')

hsnt <- charts %>%
  filter(COUNTRY_CODE %in% c("FR", "LU", "LT",
                             "DE", "PL", "BE",
                             "CH", "AT", "WW")) %>%
  filter(ACCOUNT == "Spotify") %>%
  filter(TITLE == "Head Shoulders Knees & Toes (feat. Norma Jean Martine)") %>%
  select(COUNTRY_CODE, CURRENT_POSITION, DATE_KEY,
         TITLE) %>%
  mutate(on_WW = ifelse(COUNTRY_CODE == "WW", 1,0)) %>%
  select(COUNTRY_CODE, CURRENT_POSITION, DATE_KEY)
library(tidyverse)
test <- hsnt %>%
  group_by_at(vars(-CURRENT_POSITION)) %>%
  dplyr::mutate(row_id = 1:n()) %>%
  ungroup() %>%
  spread(key = COUNTRY_CODE, value = CURRENT_POSITION)
test[is.na(test)] = 200
class(test$FR)
test<- as.data.frame(test)
plot(test[,8]) 

j <- c(5,10,11) 
sample <- t(test[j,])
plot.ts(sample,         
        main = "Time-series Plot",         
        col = 'blue',         
        type = 'b')
n <- 10
s <- sample(1:100, n)
i <- c(s,100+s, 200+s, 300+s, 400+s, 500+s)
d <- data[i,]
str(d)
pattern <- c(rep('Normal', n),
             rep('Cyclic', n),
             rep('Increasing trend', n),
             rep('Decreasing trend', n),
             rep('Upward shift', n),
             rep('Downward shift', n))
library(dtw)
distance <- dist(d, method = "DTW")
hc <- hclust(distance, method = 'average')
plot(hc,
     labels = pattern,
     cex = 0.7,
     hang = -1,
     col = 'blue')
rect.hclust(hc, k=4)

## classification

pattern100 <- c(rep('Normal', 100),
                rep('Cyclic', 100),
                rep('Increasing trend', 100),
                rep('Decreasing trend', 100),
                rep('Upward shift', 100),
                rep('Downward shift', 100))
newdata <- data.frame(data, pattern100)
str(newdata)
newdata$pattern100<-factor(newdata$pattern100)
library(party)
tree <- ctree(pattern100~., newdata)
