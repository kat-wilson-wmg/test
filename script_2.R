
#### overview
# DV of chart toppers -> binary of when songs charted

charts <- read_tsv('/cloud/project/raw/test.tsv')

## ggplot way
charts$DAYS_ON_CHART <- ifelse(is.na(charts$DAYS_ON_CHART),
                               0, charts$DAYS_ON_CHART)


p <- ggplot(charts, aes(x=DATE_KEY, y=DAYS_ON_CHART,
                        col = CUSTOMER_NAME)) +
  geom_line() + 
  xlab("") +
  scale_x_date(date_labels = "%Y-%m-%d") +
  labs(title = "Ghost Town Chart Days") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
p
class(charts$MAJOR_GENRE_DESC)
table(charts$MAJOR_GENRE_DESC, charts$DAYS_ON_CHART)
sum(charts$DAYS_ON_CHART)
charts %>%
  group_by(CUSTOMER_NAME) %>%
  dplyr::summarise(count = n(),
                   sum = sum(DAYS_ON_CHART))
