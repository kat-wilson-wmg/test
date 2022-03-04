
#### DAYS ON CHART

### the DV here is Days on Charts (bad DV)
charts <- read_tsv('/cloud/project/raw/days_on_charts.tsv')
#### days on chart
charts$DAYS_ON_CHART <- ifelse(is.na(charts$DAYS_ON_CHART),
                               0, charts$DAYS_ON_CHART)
charts <- charts %>%
  filter(COUNTRY_CODE %in% c("US", "GB", "RS"))
p <- ggplot(charts, aes(x=DATE_KEY, y=DAYS_ON_CHART,
                        col = CUSTOMER_NAME)) +
  geom_line() + 
  xlab("") +
  scale_x_date(date_labels = "%Y-%m-%d") +
  labs(title = "Ghost Town Chart Days") +
  theme(axis.text.x=element_text(angle=60, hjust=1))  +
  facet_wrap(~COUNTRY_CODE)
p
class(charts$MAJOR_GENRE_DESC)
table(charts$MAJOR_GENRE_DESC, charts$DAYS_ON_CHART)
sum(charts$DAYS_ON_CHART)
charts %>%
  group_by(CUSTOMER_NAME) %>%
  dplyr::summarise(count = n(),
                   sum = sum(DAYS_ON_CHART))
charts %>%
  group_by(COUNTRY_CODE) %>%
  dplyr::summarise(count = n(),
                   sum = sum(DAYS_ON_CHART))



### DV here is Spotify 200 (where lower position is better)
position <- read_tsv('/cloud/project/raw/current_position.tsv')

p <- ggplot(position, aes(x=DATE_KEY, y=as.numeric(CURRENT_POSITION),
                        col = as.factor(REGION))) +
  geom_line() + 
  xlab("") +
  scale_x_date(date_labels = "%Y-%m-%d") +
  labs(title = "Ghost Town Spotify 200 Chart progression") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
p
