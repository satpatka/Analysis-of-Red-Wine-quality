install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("skimr")
install.packages("knitr")
install.packages("stringr")
install.packages("viridis")
install.packages("factoextra")
install.packages("hrbrthemes")
install.packages("tibble")
install.packages("mclust")
install.packages("LPCM")
install.packages("cluster")
install.packages("NbClust")
install.packages("funModeling")
install.packages("fpc")



library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(skimr)
library(knitr)
library(stringr)
library(viridis)
library(factoextra)
library(hrbrthemes)
library(tibble)
library(forcats)
library(mclust)
library(fpc)
library(LPCM)
library(cluster)
library(NbClust)
library(funModeling)

??cluster.stats

set.seed(13232767)
winequality <- read.csv("C:/Users/keyas/OneDrive/Desktop/DMBI/Unsupervised/winequality-red.csv")
str(winequality)

#Exploratory Data Analysis

#A simple density plot on the dataset shows that our dataset has 2 bumps (considering threshold density as 0.5 for significance), suggesting that these data might be coming from two different sources
options(scipen = 999)
density_winequality <- ggplot(winequality, aes(x = quality)) +
  geom_density()
density_winequality 

density_winequality + 
  geom_vline(xintercept = 5, col = "red", size = 2) + 
  geom_vline(xintercept = 6, col = "blue", size = 2)
#For instance, there likely is a subpopulation with quality of 5 with some variance around this mean (red vertical line) and another population with a quality of 6 with again some variance around this mean (blue vertical line)

#Correlation Plot Analysis
corrplot(cor(winequality))
#Since our aim to is cluster wine types by the descriptors, so we care about the columns which are influencing quality the most.As the heatmap suggest,residual sugar,pH and free sulphur dioxide factors are also not playing a really big role in the quality of wine. 

#Histogram Analysis
plot_num(winequality)
#From the histograms, we observe that
#1.The pH value seems to dispaly a normal distribution with major samples exhibiting values between 3.0 and 3.5
#2.The free sulfur dioxide seems to be between the 1-72 count with peaking around 10 mark.
#3.The total sulfur dioxide seems to a have a spread between 0 and 175 and exhibiting peak around 50.
#4.The alcohol content seems to vary from 8 to 14 with major peaks around 10 with a lower count between 13 and 14
#5.The fixed acidity, volatile acidity and density can almost be considered to be normally distributed
#6.Majorly, the variables distributions are right-tailed


#Boxplot Analysis
boxplot(scale(winequality),xlab="Value",ylab="Parameters",main="Boxplot Presentation of different Parameters")
#We have scaled all the values for boxplot analysis to bring them at similar scales and avoid overrepresntation of any independant parameter.
#A simple analysis of the boxplot shows that there are major outliers in residual sugar, chlorides and sulphates and minor outliers in citric acid
#Overall, data distribution is approximately uniform except for quality which has values mostly in the higher range

#Distribution analysis of different physicochemical factors
winequality %>%
  select(fixed.acidity, quality) %>%
  group_by(quality)%>%
  ggplot(mapping = aes(x = quality, y = fixed.acidity, color = quality))+
  facet_wrap(~quality)+
  geom_point()+
  theme_minimal()
#Fixed acidity does not effect wine quality significantly, except for very low quality wines in which the fixed acidity content is significantly less

winequality %>%
  select(citric.acid, quality) %>%
  group_by(quality)%>%
  ggplot(mapping = aes(x = quality, y = citric.acid, color = quality))+
  facet_wrap(~quality)+
  geom_point()+
  theme_minimal()
#Citric acid does not effect wine quality significantly as the distribution range is approximately uniform across all categories of quality


winequality %>%
  select(chlorides, quality) %>%
  group_by(quality)%>%
  ggplot(mapping = aes(x = quality, y = chlorides, color = quality))+
  facet_wrap(~quality)+
  geom_point()+
  theme_minimal()
#Chloride content is significantly less in high quality wines

winequality %>%
  select(alcohol, quality) %>%
  group_by(quality)%>%
  ggplot(mapping = aes(x = quality, y = alcohol, color = quality))+
  facet_wrap(~quality)+
  geom_point()+
  theme_minimal()
#Alcohol content does not significantly effect wine quality, except for low quality wine in which the alcohol content is significantly less



#Since this a high dimension dataset, we will first use PCA to reduce the dataset dimension
#The principal components are supplied with normalized version of original predictors. This is because, the original predictors may have different scales
#Performing PCA on un-normalized variables will lead to insanely large loadings for variables with high variance. In turn, this will lead to dependence of a principal component on the variable with high variance. This is undesirable.
#Scaling the variables
winequality <- scale(winequality)


#Performing PCA
wine.pca <- prcomp(winequality)
summary(wine.pca)
#As per the summary above (Importance of components); the first 8 PC's contribte to ~92% of the information required for the entire data. Hence the 13 components can be reduced to 8 for further analysis with 92% information. The other variables can be included in case we intend to have more accurate analysis/forcasting/prediction.

wine.pca

plot(wine.pca)
biplot(wine.pca)
#From the PCA biplot, we observe that residual sugar, sulphates, free sulphur dioxide and chlorides are not of much influence


#Creating a new variable to indicate if the wine is good or bad
winequality$good.wine <- ifelse(winequality$quality>6,1,0)

summary(winequality)
#Here we can see that we are dealing with an unbalanced dataset, wherein only 13.57% out of 1599 wines is considered as good wine

#First glimpse of data with skim
#Using skim to obtain the outlook of the dataset, no.of observations, no.of columns, range of variables, no.of missing/unique values, histograms, etc.
winequality %>% skim %>% kable()





#So we will remove the following variables from the dataset:
#residual sugar
#pH
#free sulphur dioxide
#chlorides


winequality <- winequality[,c(1,2,3,7,8,10,11)]

#Also we can see that volatile acidity, alcohol and sulphates significantly determine wine quality.Creating a Plotly 3D interactive graph to analyze how these variables directly identify quality
winequality %>% plot_ly(x = ~alcohol, y = ~volatile.acidity, z = ~sulphates, 
                        color = ~quality, hoverinfo = 'text', colors = viridis(3),
                        text = ~paste('Quality:', quality,
                                      '<br>Alcohol:', alcohol,
                                      '<br>Volatile Acidity:' , volatile.acidity,
                                      '<br>Sulphates:', sulphates))%>%
  add_markers(opacity = 0.8) %>%
  layout(title = "3D Scatter plot: Alcohol vs Volatile Aidity vs Sulphates",
         annotations = list(yref='paper', xref='paper',y=1.05,x=1.1,text='Quality', showarrow=F),
         scene = list(xaxis = list(title = 'Alcohol'),
                      yaxis = list(title = 'Volatile Acidity'),
                      zaxis = list(title = 'Sulphates')))



#Data pre-processing
#Converting quality variable into factor
colnames(winequality) <- winequality %>% colnames() %>% str_replace_all(" ","_")
#winequality$quality <- as.factor(winequality$quality)

str(winequality)

#K-Means Clustering
distance <- get_dist(winequality)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


#The data is standardised before clustering
winequality <- scale(winequality)

#Determining optimal number of clusters
#Elbow Method
#Finding number of clusters for which the model is not overfitting but clusters the data as per the actual distribution.

#In elbow method, percentage of variance is explained as a function of the number of clusters plotted. This "elbow" cannot always be unambiguously identified. From the plot, where the sum of squares for number of clusters have been plotted from 1 to 12, I have chosen 3 as the number of clusters.
wss <- (nrow(winequality)-1)*sum(apply(winequality,2,var))

for (i in 2:12) wss[i] <- sum(kmeans(winequality,
                                     centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#Silhoutte Method
#Verifying the number of clusters based on average silhouette width as well. The silhouette value is a measure of how similar an object is to its own cluster (cohesion) compared to other clusters (separation). The silhouette ranges from -1 to +1, where a high value indicates that the object is well matched to its own cluster and poorly matched to neighboring clusters. We chose the number of clusters that gives the maximum value for silhouette width. I get 2 in this method
#fviz_nbclust(winequality, kmeans, method = "silhouette")

#Running K-Means Clustering for 6 clusters
k6_model <- kmeans(winequality, centers = 6, nstart = 25)
fviz_cluster(k6_model, geom = c("point", "text"),  data = winequality) + ggtitle("Clustering with 3 groups")
k6_model

#Dunn index
dunn_kmeans = dunn(clusters = k6_model$cluster, Data=winequality)
dunn_kmeans




#Gaussian mixture model
#Why would you use a mixture model?
#From the density plot of our dataset, we have observed that our data is bi-modal, suggesting that the data might have been collected from two different sources.
#In fact, from the density plot, what we've done is a naive attempt at trying to group the data into subpopulations/clusters. But surely there must be some objective and "automatic" way of defining these clusters. This is where mixture models come in by providing a "model-based approach" to clustering through the use of statistical distributions. Here we will utilize an R package to perfom Gaussian mixture model clustering.

#The package mclust performs various types of model-based clustering and dimension reduction. Plus, it's really intuitive to use. It requires complete data (no missing), which is not an issue in our case since our dataset has no missing values
#The dataset has also been standardize of all the indicators so when we plot the profiles it's clearer to see the differences between clusters
BIC <- mclustBIC(winequality)

#We'll start by plotting Bayesian Information Criteria for all the models with profiles ranging from 1 to 9
plot(BIC)

#It's not immediately clear which model is the best since the y-axis is so large and many of the models score close together. summary(BIC) shows the top three models based on BIC.
summary(BIC)
#There is not significant difference between the best BIC values. So, we will go with VEV,7 as the best fit between model complexity and efficiency. VEV,7 says there are 7 clusters with equal shape and ellipsodial distribution.

#If we want to look at this model more closely, we save it as an object and inspect it with summary().
mod_BIC <- Mclust(winequality, modelNames = "VEV", G = 7, x = BIC)
summary(mod_BIC)
#The output describes the characteristics of the wine descriptors and the number of cases classified into each of the 7 clusters.

#Dunn index
dunn_BIC = dunn(clusters = mod_BIC$classification, Data=winequality)
dunn_BIC

#BIC is one of the best fit indices, but it's always recommended to look for more evidence that the solution we've chosen is the correct one. So we have also compared values of the Integrated Completed Likelikood (ICL) criterion.
#ICL isn't much different from BIC, except that it adds a penalty on solutions with greater entropy or classification uncertainty.
ICL <- mclustICL(winequality)
plot(ICL)
summary(ICL)
#Similar to BIC model, there is no significant difference between the best ICL values. ICL suggests that model VEV4, VVV4 and VVV7 fits quite well. Here also we notice that there is no significant differences between the best ICL values. So, we will go with VEV,4 as the best model for GMM.
mod_ICL <- Mclust(winequality, modelNames = "VEV", G = 4, x = BIC)
summary(mod_ICL)

#Dunn index
dunn_ICL = dunn(clusters = mod_ICL$classification, Data=winequality)
dunn_ICL


#Classification, uncertainty and density plots illustrating which observations are assigned to each cluster and their level of assignment uncertainty.
plot(mod_BIC, what = 'classification')
plot(mod_BIC, what = 'uncertainty')
plot(mod_BIC, what = 'density')





#Hierarchical Clustering
#Hierarchical clustering is an alternative approach to k-means clustering for identifying groups in a data set. In contrast to k-means, hierarchical clustering will create a hierarchy of clusters and therefore does not require us to pre-specify the number of clusters. Furthermore, hierarchical clustering has an added advantage over k-means clustering in that its results can be easily visualized using an attractive tree-based representation called a dendrogram.
#While agglomerative clustering is good at identifying small clusters, Divisive hierarchical clustering, on the other hand, is better at identifying large clusters.
#There are multiple agglomeration methods to define clusters when performing a hierarchical cluster analysis; however, complete linkage and Ward's method are often preferred for AGNES clustering and we will be using them in our project.


#Calculate the distance matrix using Euclidean distance by default.
winequality.dist=dist(winequality)

#Obtain clusters using the Wards method
#Ward's minimum variance method minimizes the total within-cluster variance. At each step the pair of clusters with the smallest between-cluster distance are merged. It tends to produce more compact clusters.
winequality.hclust.ward=hclust(winequality.dist, method="ward")
plot(winequality.hclust.ward)

#Selecting optimal number of clusters
fviz_nbclust(winequality, FUN = hcut, method = "wss")
##The elbow method suggests 5 clusters

#Cutting the tree at 5 clusters
winequality.cuttree.ward= cutree(winequality.hclust.ward, k = 5) 


fviz_dend(winequality.hclust.ward,k = 5,
          cex = 0.5, # label size
          k_colors = c( "#00AFBB", "#E7B800"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_border = c("#00AFBB", "#E7B800"), 
          rect_fill = TRUE)


#Hierarchical clustering using Complete Linkage
#The complete linkage method computes all pairwise dissimilarities between the elements in cluster 1 and the elements in cluster 2, and considers the largest value of these dissimilarities as the distance between the two clusters. It tends to produce more compact clusters.
winequality.hclust.complete <- hclust(winequality.dist, method = "complete")
plot(winequality.hclust.complete )

#Cutting the tree at 5 clusters
winequality.cuttree.complete= cutree(winequality.hclust.complete, k = 5)

fviz_dend(winequality.hclust.complete,k = 5,
          cex = 0.5, # label size
          k_colors = c( "#00AFBB", "#E7B800"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_border = c("#00AFBB", "#E7B800"), 
          rect_fill = TRUE)

#However, due to the size of the dataset, the dendrogram is not very legible. Consequently, we may want to zoom into one particular region or cluster. This allows us to see which observations are most similar within a particular group.
winequality[winequality.cuttree.ward==3,]

#Dunn's index for complete-linkage: dunn_complete
dunn_single = dunn(clusters = winequality.cuttree.single, Data=winequality)
dunn_single

dunn_ward = dunn(clusters = winequality.cuttree.ward, Data=winequality)
dunn_ward





#DBSCAN
#Given that DBSCAN is a density based clustering algorithm, it does a great job of seeking areas in the data that have a high density of observations, versus areas of the data that are not very dense with observations. Another advantage of DBSCAN is that it can sort data into clusters of varying shapes.
#Due to the MinPts parameter, the single-link effect (different clusters being connected by a thin line of points) is reduced. DBSCAN has a notion of noise. 
#The most common distance metric used in DBSCAN is Euclidean distance. Especially for high-dimensional data, this metric can be rendered almost useless due to the "Curse of dimensionality", making it difficult to find an appropriate value for epsilon. DBSCAN cannot cluster data sets well with large differences in densities, since the minPts-epsilon combination cannot then be chosen appropriately for all clusters.
install.packages("dbscan")
library(dbscan)

db <- kNNdistplot(winequality, k = 8)
abline(h = 2, col = "red", lty = 2)

results <- dbscan(winequality, eps = 2, minPts = 2)
results

fviz_cluster(results, winequality, geom = "point")

dunn_dbscan = dunn(clusters = results$cluster, Data=winequality)
dunn_dbscan