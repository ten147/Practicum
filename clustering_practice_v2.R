# check out some clustering stuff

library(tidyverse)
library(factoextra)
library(cluster)

mydf <- read.csv("C:/Users/sonia/Desktop/grad_school/practicum/data_v2/df_unhealth.csv")

#explore original dataset
summary(mydf)
str(mydf)

#drop the state column because it is a text variable
clus_df <- mydf %>%
  select(-c(STATE))

#convert numeric data to integers because clustering algorithm shows an error
a <- as.integer(mydf$BINGE)
b <- as.integer(mydf$CSMOKING)
c <- as.integer(mydf$LPA)
d <- as.integer(mydf$OBESITY)
e <- as.integer(mydf$SLEEP)

newdf <- cbind(a,b,c,d,e)

colnames(newdf) <- c("binge", "smoking", "lpa", "obesity", "sleep")

#calculate the gap statistic
gap_stat <- clusGap(newdf, FUN = kmeans, nstart = 25, K.max = 10, B = 10)
print(gap_stat)
#unfortunately, this shows us an optimal cluster of 1
#we probably need to normalize the data, or add all the datasets together to get a better cluster number

#visualization for the gap stat
gap_viz <- fviz_gap_stat(gap_stat)
print(gap_viz)

#use 2 clusters for example purposes
clus_k_2 <- kmeans(newdf, 2, nstart = 10)
print(clus_k_2)

#visualize the two clusters with the two strongest principal components dimensions
clus_plot <- fviz_cluster(clus_k_2, newdf)
print(clus_plot)
