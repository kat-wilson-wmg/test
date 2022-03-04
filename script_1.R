
#### overview
# This replicates the Ghost Town Stream Count using ts object
# in monthly, weekly, daily. Issue with ts() not matching ggplot
# output. Plots only for US. Next step -> compare by countries

## STEP 0: libraries
### library
library(dplyr)
library(hrbrthemes)


## Step 1: Monthly Data
#######
###monthly
#######
library(tidyverse)
df <- read_tsv('/cloud/project/raw/monthly_ghosttown.tsv')
### monthly total streams of ghost town in 5 year period
small <- df %>%
  filter(COUNTRY_CODE == "US") %>%
  filter(MAJOR_GENRE_DESC == "Pop")
small_sep <- separate(small, "DATE_KEY", c("Year", "Month", "Day"), sep = "-")
# ts way 2 incorrect
streams.ts <- ts(small[,6], start = c(2021,1,1), freq = 5)
plot(streams.ts)
## ggplot way
p <- ggplot(small, aes(x=DATE_KEY, y=TOTAL_STREAMS)) +
  geom_line() + 
  xlab("") +
  scale_x_date(date_labels = "%Y-%m-%d") +
  labs(title = "Monthly Streams Ghost Town") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
p

## STEP 2: Weekly Data
#######
###weekly
#######
df <- read_tsv('/cloud/project/raw/weekly_ghosttown.tsv')
small <- df %>%
  filter(COUNTRY_CODE == "US")
small_sep <- separate(small, "DATE_KEY", c("Year", "Month", "Day"), sep = "-")

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
  scale_x_date(date_labels = "%Y-%m-%d") +
  labs(title = "Weekly Streams Ghost Town") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
p
### weekly breaks
p <- ggplot(small, aes(x=DATE_KEY, y=TOTAL_STREAMS)) +
  geom_line() + 
  xlab("") +
  labs(title = "Weekly Streams Ghost Town") +
  scale_x_date(date_labels = "%m-%d-%y",
                     date_breaks = "week")
p
### monthly breaks
p <- ggplot(small, aes(x=DATE_KEY, y=TOTAL_STREAMS)) +
  geom_line() + 
  xlab("") +
  labs(title = "Weekly Streams Ghost Town") +
  scale_x_date(date_labels = "%m-%d-%y",
               date_breaks = "month")
p

## STEP 3: Daily Data
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
stream.ts <- ts(TOTAL_STREAMS , start = c(2021, as.numeric(format(small$DATE_KEY[1], "%j"))),
                frequency = 365)
plot(stream.ts, ylab = "streams", main = "daily streams")
## ts way 2
stream.ts <- ts(small[6], start = c(2021, as.numeric(format(small$DATE_KEY[1], "%j"))),
                frequency = 365)
plot(stream.ts)
## ggplot way
p <- ggplot(small, aes(x=DATE_KEY, y=TOTAL_STREAMS)) +
  geom_line() + 
  xlab("") +
  labs(title = "Daily Streams Ghost Town") +
  scale_x_date(date_labels = "%m-%d-%y",
               date_breaks = "week")+
  theme(axis.text.x=element_text(angle=60, hjust=1))
p


### STEP 4: trends by country
df <- read_tsv('/cloud/project/raw/weekly_ghosttown.tsv')
small <- df %>%
  filter(COUNTRY_CODE %in% c("US", "GB", "RS")) %>%
  filter(MAJOR_GENRE_DESC == "Pop")
small_sep <- separate(small, "DATE_KEY", c("Year", "Month", "Day"), sep = "-")
pass.ts <- ts(small[6])
plot(pass.ts, ylab = "streams", main = "weekly streams", xlim = c(0,20))
## ggplot way
p <- ggplot(small, aes(x=DATE_KEY, y=TOTAL_STREAMS,
                       col = COUNTRY_CODE)) +
  geom_line() + 
  xlab("") +
  scale_x_date(date_labels = "%Y-%m-%d") +
  labs(title = "Weekly Streams Ghost Town") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
p


