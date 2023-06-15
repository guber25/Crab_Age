install.packages("tidyverse")
install.packages("reshape2")
install.packages("corrplot")

library(corrplot) 
library(tidyverse)
library(reshape2)

ds1=read.csv("https://raw.githubusercontent.com/guber25/Crab_Age/main/Dataset/Crab_age.csv")
#https://www.kaggle.com/datasets/sidhus/crab-age-prediction
or_dim=dim(ds1)[1]
ds1[,1] <- as.factor(ds1[,1])
Sex_M=ifelse(ds1$Sex=="M",yes = 1, 0)
Sex_F=ifelse(ds1$Sex=="F",yes = 1, 0)
Sex_I=ifelse(ds1$Sex=="I",yes = 1, 0)
ds1$Sex_M=Sex_M
ds1$Sex_F=Sex_F
ds1$Sex_I=Sex_I


View(ds1)

#We try to find some correlation between the variables. Clearly some correlation is present but there are many outliers.
plot(ds1)



#In this way we look at the distribution of the variables and we see, again, the presence of outliers and of skewed distribution
ggplot(gather(select(ds1, -c("Sex"))), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free') 

glimpse(ds1)

ggplot(gather(select(ds1, -c("Sex"))), aes(value)) + 
  geom_boxplot() + 
  facet_wrap(~key, scales = 'free')

#we look if there are NULL values or NA values
cat("NULL values count:",sum(is.null(ds1)),"\nNA values count:", sum(is.na(ds1)))


#### Outlier removal


# OUTLIER REMOVAL (substituting outliers with NA)
for (i in 1:3) {
  for (x in ds1 %>% select(-"Sex") %>% names())
  {
    value = ds1[,x][ds1[,x] %in% boxplot.stats(ds1[,x])$out]
    ds1[,x][ds1[,x] %in% value] = NA
    ds1 = drop_na(ds1)
  } }
head(ds1)


#Check if there are no more NA values
as.data.frame(colSums(is.na(ds1)))



#as the plots show, there are no more outliers
ggplot(gather(select(ds1, -c("Sex"))), aes(value)) + 
  geom_boxplot() + 
  facet_wrap(~key, scales = 'free')



cat(((or_dim-dim(ds1)[1])/or_dim)*100,"%")


#Due to the removal of outliers, we have lost 9.86% of the data.


# We see how outlier removal impacted distributions
ggplot(gather(select(ds1, -c("Sex"))), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free')

#Now correlations are much more stable
plot(ds1)

dim(ds1) 
ds3 <- ds1 
ds3$Sex <- NULL 
ds3$Sex_M <- NULL 
str(ds3) 
summary(ds3) 
summary(is.na(ds3)) # there are no missing values



ggplot(ds3, aes(Age, Weight)) + geom_point(alpha = 0.25) + xlab("Age") + ylab("Weight")


cor.result <- cor(ds3) 
corrplot(cor.result, method="ellipse") + title(main = "Correlation")


## we can see from here that most of the variables are almost correlated with each other

ds3 <- select(ds3, -Age)

#we need to scale our data now

df <- as.data.frame(scale(ds3)) 
summary(df)


d<-dist(ds3, method = "euclidean")
h<-hclust(d)
hclust(d = d)

hc.complete <- hclust(dist(ds3), method = "complete")
plot(hc.complete)
rect.hclust(hc.complete, k = 5, border = 2:3508)
plot(as.dendrogram(hc.complete), xlim = c(1,3508), ylim = c(1,70))
hcd <- as.dendrogram(hc.complete)
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), cex = 0.7, col = "orange")
plot(hcd, type = "rectangle", ylab = "Height", nodePar=nodePar, leaflab = "none")


hc.average <- hclust(dist(ds3), method = "average")
plot(hc.average)
hcd2 <- as.dendrogram(hc.average)
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), cex = 0.7, col = "blue")
plot(hcd2, type = "rectangle", ylab = "Height", nodePar=nodePar, leaflab = "none")


