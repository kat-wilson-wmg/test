### install packages

### load packages
library(tidyverse)
df <- read_tsv('/cloud/project/raw/monthly_ghosttown.tsv')

### monthly total streams of ghost town in 5 year period
small <- df %>%
  filter(COUNTRY_CODE == "US")
small <- separate(small, "DATE_KEY", c("Year", "Month", "Day"), sep = "-")

### ts
attach(small)
# head(pass)
pass.ts <- ts(TOTAL_STREAMS, start = c(2021, 10), freq = 12)
plot(pass.ts, ylab = "streams", main = "monthly streams")
