library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

#Importing the data 
caro <- read_delim("caro60.csv",",") 
caro <- tibble(caro)

caro <- caro %>%
  mutate(
    nMinus3 = sqrt((lag(E,3)-E)^2+(lag(N,3)-N)^2),   # distance to pos -30 secs
    nMinus2 = sqrt((lag(E,2)-E)^2+(lag(N,2)-N)^2),   # distance to pos -20 secs
    nMinus1 = sqrt((lag(E,1)-E)^2+(lag(N,1)-N)^2),   # distance to pos -10 secs
    nPlus1  = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2), # distance to pos +10 secs
    nPlus2  = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2), # distance to pos +20 secs
    nPlus3  = sqrt((E-lead(E,3))^2+(N-lead(N,3))^2)  # distance to pos +30 secs
  )

#Calculating the means of steplength
caro <- caro %>%
  rowwise() %>% #rowwise specifies that we want the mean value per row
  mutate(
    stepMean = mean(c(nMinus3, nMinus2, nMinus1,nPlus1,nPlus2, nPlus3))
  ) %>%
  ungroup() 

caro <- caro %>% 
  ungroup() %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))


caro%>%
  ggplot(aes(E, N))  +
  geom_path() +
  geom_point(aes(colour = static)) +
  coord_fixed() +

#Assign unique IDs based on the column static
rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}

caro <- caro %>%
  mutate(segment_id = rle_id(static))

#removing static points
caro <- caro %>%
  filter(!static)

#all segments
caro%>%
  ggplot(aes(E, N))  +
  geom_path(aes(colour = segment_id)) +
  geom_point(aes(colour = segment_id)) +
  coord_fixed() +
  ggtitle("all segments")

#long segments 1: removing segments less than 5 mins
caro <- caro %>%
  group_by(segment_id) %>%
   mutate(
    x = as.integer(difftime(max(DatetimeUTC),min(DatetimeUTC), units="mins"))) %>% 
      filter(x > 5)  

#long segments 2: plotting long segments
caro %>%
  ggplot(aes(E, N, col= segment_id))  +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")+
  ggtitle("segments >= 5 minutes")

#Task 5

