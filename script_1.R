
#### overview
# This replicates the Ghost Town Stream Count using ts object
# in monthly, weekly, daily. Issue with ts() not matching ggplot
# output. Plots only for US. Next step -> compare by countries


### library
library(dplyr)
library(hrbrthemes)

#######
###monthly
#######
library(tidyverse)
df <- read_tsv('/cloud/project/raw/monthly_ghosttown.tsv')
### monthly total streams of ghost town in 5 year period
small <- df %>%
  filter(COUNTRY_CODE == "US")
small_sep <- separate(small, "DATE_KEY", c("Year", "Month", "Day"), sep = "-")
# ts way 1
pass.ts <- ts(TOTAL_STREAMS, start = c(2021,10), freq = 12)
plot(pass.ts, ylab = "streams", main = "monthly streams")
# ts way 2
streams.ts <- ts(small[,6], start =c(2021,1), freq = 12)
plot(streams.ts)
## ggplot way
p <- ggplot(small, aes(x=DATE_KEY, y=TOTAL_STREAMS)) +
  geom_line() + 
  xlab("") +
  scale_x_date(date_labels = "%Y-%m-%d") +
  labs(title = "Monthly Streams Ghost Town") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
p


#######
###weekly
#######
df <- read_tsv('/cloud/project/raw/weekly_ghosttown.tsv')
small <- df %>%
  filter(COUNTRY_CODE == "US")
# small <- separate(small, "DATE_KEY", c("Year", "Month", "Day"), sep = "-")

# # build a numeric indicator
# new<- ddply(small, .(COUNTRY_CODE), mutate, id = order(DATE_KEY)) %>%
#   select(id, TOTAL_STREAMS)
# class(new$id)
# attach(new)
# new$id <- as.numeric(new$id)

# ts way 1
attach(small)
pass.ts <- ts(TOTAL_STREAMS, start = c(2021,10), freq = 12)
plot(pass.ts, ylab = "streams", main = "weekly streams")
## ts way 2
stream.ts <- ts(small[8], start = c(2021,1,1), freq = 5)
plot(stream.ts)
## ggplot way
p <- ggplot(small, aes(x=DATE_KEY, y=TOTAL_STREAMS)) +
  geom_line() + 
  xlab("") +
  scale_x_date(date_labels = "%Y %w %d") +
  labs(title = "Weekly Streams Ghost Town") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
p


#######
###daily
#######
df <- read_tsv('/cloud/project/raw/daily_ghosttown.tsv')
small <- df %>%
  filter(COUNTRY_CODE == "US") %>%
  filter(MAJOR_GENRE_DESC == "Pop")
# small <- separate(small, "DATE_KEY", c("Year", "Month", "Day"), sep = "-")
# ts way 1
attach(small)
pass.ts <- ts(TOTAL_STREAMS, start = c(2021,10), freq = 12)
plot(pass.ts, ylab = "streams", main = "daily streams")
## ts way 2
stream.ts <- ts(small[8], start = c(2021,1,1), freq = 5)
plot(stream.ts)
## ggplot way
p <- ggplot(small, aes(x=DATE_KEY, y=TOTAL_STREAMS)) +
  geom_line() + 
  xlab("") +
  labs(title = "Daily Streams Ghost Town") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
p


