
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


### GhostTown
### DV here is Spotify 200 (where lower position is better)
charts <- read_tsv('/cloud/project/raw/test (1).tsv')
charts <- charts %>%
  filter(COUNTRY_CODE %in% c("US", "GB", "RS")) %>%
  filter(ACCOUNT == "Spotify") %>%
  select(COUNTRY_CODE, CURRENT_POSITION, DATE_KEY)

## lower number is higher on the charts
p <- ggplot(charts, aes(x=DATE_KEY, y=as.numeric(CURRENT_POSITION),
                        col = as.factor(COUNTRY_CODE))) +
  geom_line() + 
  xlab("") +
  scale_x_date(date_labels = "%Y-%m-%d") +
  labs(title = "Ghost Town Spotify 200 Chart progression") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
p

## Offenbach
### DV here is Spotify 200 (where lower position is better)
charts <- read_tsv('/cloud/project/raw/offenbach.tsv')

charts_total <- charts %>%
  filter(COUNTRY_CODE %in% c("FR", "LU", "LT",
                             "DE", "PL", "BE",
                             "CH", "AT", "WW")) %>%
  filter(ACCOUNT == "Spotify") %>%
  filter(TITLE == "Head Shoulders Knees & Toes (feat. Norma Jean Martine)") %>%
  select(COUNTRY_CODE, CURRENT_POSITION, DATE_KEY)
charts_ww <- charts %>%
  filter(COUNTRY_CODE == "WW") %>%
  filter(ACCOUNT == "Spotify") %>%
  filter(TITLE == "Head Shoulders Knees & Toes (feat. Norma Jean Martine)") %>%
  select(COUNTRY_CODE, CURRENT_POSITION, DATE_KEY)
charts_france <- charts %>%
  filter(COUNTRY_CODE == "FR") %>%
  filter(ACCOUNT == "Spotify") %>%
  filter(TITLE == "Head Shoulders Knees & Toes (feat. Norma Jean Martine)") %>%
  select(COUNTRY_CODE, CURRENT_POSITION, DATE_KEY)

## lower number is higher on the charts
p_worldwide <- ggplot(charts_ww, aes(x=DATE_KEY, y=as.numeric(CURRENT_POSITION),
                                     col = COUNTRY_CODE)) +
  geom_line() + 
  xlab("") +
  scale_x_date(date_labels = "%Y-%m-%d") +
  labs(title = "Ghost Town Spotify 200 Chart progression - Worldwide") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
p_worldwide
p_country <- ggplot(charts_total, aes(x=DATE_KEY, y=as.numeric(CURRENT_POSITION),
                                      col = COUNTRY_CODE)) +
  geom_line() + 
  xlab("") +
  scale_x_date(date_labels = "%Y-%m-%d") +
  labs(title = "Ghost Town Spotify 200 Chart progression - All Countries") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
p_country
p_france <- ggplot(charts_france, aes(x=DATE_KEY, y=as.numeric(CURRENT_POSITION),
                                      col = COUNTRY_CODE)) +
  geom_line() + 
  xlab("") +
  scale_x_date(date_labels = "%Y-%m-%d") +
  labs(title = "Ghost Town Spotify 200 Chart progression - France") +
  theme(axis.text.x=element_text(angle=60, hjust=1))
p_france
library(gridExtra)
library(cowplot)
plot_grid(p_worldwide, p_france, p_country, labels=c("A", "B", "C"), ncol = 2, nrow = 2)


## all together is better

## lower number is higher on the charts, we hit at week 16
p_all <- ggplot(charts_total, aes(x=DATE_KEY, y=as.numeric(CURRENT_POSITION),
                                     col = COUNTRY_CODE, )) +
  geom_line(aes(linetype = ifelse(COUNTRY_CODE == "WW",
                          "dashed", "solid")))+ 
  theme_minimal()+
  xlab("")+
  ylab("Current Position")+
   scale_x_date(date_labels = "%Y-%m-%d",
                date_breaks = "month") +
  labs(title = "Offenbach HSNT Spotify 200 Chart Progression",
       caption = "Spotify 200 position where lower position indicates chart climbing \n
       HSNT hits WW charts (global) in week 16") +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        axis.title = element_text(size = 18),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 18)) +
  guides(linetype ="none",
         color = guide_legend("Country"))
p_all
