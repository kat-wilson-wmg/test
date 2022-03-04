## check the datasets


month <- read_tsv('/cloud/project/raw/monthly_ghosttown.tsv') %>%
  filter(COUNTRY_CODE == "US") %>%
  filter(MAJOR_GENRE_DESC == "Pop")
weekly <- read_tsv('/cloud/project/raw/weekly_ghosttown.tsv') %>%
  filter(COUNTRY_CODE == "US") %>%
  filter(MAJOR_GENRE_DESC == "Pop")
## caclculate weekly in October
1468174 + 1716560
daily <- read_tsv('/cloud/project/raw/daily_ghosttown.tsv')  %>%
  filter(COUNTRY_CODE == "US") %>%
  filter(MAJOR_GENRE_DESC == "Pop")

## calculate daily in October
separate(daily, "DATE_KEY", c("Year", "Month", "Day"), sep = "-") %>%
  filter(Month == '03') %>%
  summarise(n = sum(as.numeric(TOTAL_STREAMS)))

## chart tops
charts <- read_tsv('/cloud/project/raw/test.tsv')
min(charts$DATE_KEY)
max(charts$DATE_KEY)
