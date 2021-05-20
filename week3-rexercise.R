library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

#Importing the data 
caro <- read_delim("caro60.csv",",") 

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
  coord_fixed() 

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

#long segments 1: removing segments less than 5 mins (x)
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
pedestrian <- read_delim("pedestrian.csv",",") 

pedestrian %>%
  ggplot(aes(y=N, x=E)) +
  geom_path()+ 
  geom_point(aes(col=TrajID)) + 
  coord_fixed()+ 
  facet_wrap(~TrajID,nrow=2) 

#Task 6
library(SimilarityMeasures)
#Creating a matrix for each pedestrian
pedestrian1 <- as.matrix(filter(pedestrian[,1:3], TrajID == 1))
pedestrian2 <- as.matrix(filter(pedestrian[,1:3], TrajID == 2))
pedestrian3 <- as.matrix(filter(pedestrian[,1:3], TrajID == 3))
pedestrian4 <- as.matrix(filter(pedestrian[,1:3], TrajID == 4))
pedestrian4 <- as.matrix(filter(pedestrian[,1:3], TrajID == 5))
pedestrian5 <- as.matrix(filter(pedestrian[,1:3], TrajID == 6))

#DTW
dtw2 <- DTW(pedestrian1,pedestrian2)
dtw3 <- DTW(pedestrian1,pedestrian3)
dtw4 <- DTW(pedestrian1,pedestrian4)
dtw5 <- DTW(pedestrian1,pedestrian5)
dtw6 <- DTW(pedestrian1,pedestrian6)

DTW <- c(dtw2,dtw3,dtw4,dtw5,dtw6)

#EditDist
edit2 <- EditDist(pedestrian1,pedestrian2)
edit3 <- EditDist(pedestrian1,pedestrian3)
edit4 <- EditDist(pedestrian1,pedestrian4)
edit5 <- EditDist(pedestrian1,pedestrian5)
edit6 <- EditDist(pedestrian1,pedestrian6)

EditDist <- c(edit2,edit3,edit4,edit5,edit6)

#Frechet
f2 <- Frechet(pedestrian1,pedestrian2)
f3 <- Frechet(pedestrian1,pedestrian3)
f4 <- Frechet(pedestrian1,pedestrian4)
f5 <- Frechet(pedestrian1,pedestrian5)
f6 <- Frechet(pedestrian1,pedestrian6)

Frechet <- c(f2,f3,f4,f5,f6)

#LCSS
lcss1 <- LCSS(pedestrian1,pedestrian2)
lcss2 <- LCSS(pedestrian1,pedestrian3)
lcss3 <- LCSS(pedestrian1,pedestrian4)
lcss4 <- LCSS(pedestrian1,pedestrian5)
lcss5<- LCSS(pedestrian1,pedestrian6)
lcss6 <- c(lcss2,lcss3,lcss4,lcss5,lcss6)

LCSS <- c(lcss1, lcss2, lcss3, lcss4, lcss5, lcss6)


