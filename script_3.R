
### Ghsot Town Covariance Steps

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
### ccf function over time ## symmetric because it is the same variable
cov(test[,3:6])
## Step 2A: heat map: if there is a covariance, strong at a time lag
## there is a strong time lag in GB and the US, for instance
mat<- data.matrix(cov(test[,3:6]))
nba_heatmap <- heatmap(mat, Rowv=NA, Colv=NA, col = cm.colors(256), 
                       scale="column", margins=c(5,10))

### as a correlation matrix 
library(reshape)
cormat <- round(cor(test[,3:6]), 2)
melted_cormat <- melt(cormat)
head(melted_cormat)
library(ggplot2)
ggplot(data = melted_cormat, aes(x=X1, y=X2, fill=value)) + 
  geom_tile()

### correlation matrix has redunant information, 
# so set upper half to NA
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
upper_tri

## finished
# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(X2, X1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

### reorder correlation matrix according to thecorrelation coefficient
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(X2, X1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

## add correlation coefficients onto it

ggheatmap + 
  geom_text(aes(X2, X1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

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






