##Package Installation
install.packages("ggplot2")
install.packages("factoextra")

##File Import
Customers=read.csv("Mall_Customers.csv", header=TRUE)
head(Customers)
attach(Customers)

#Summary and Structure of File
str(Customers)
summary(Customers)

##Cleaning
names(Customers)
names(Customers)[3]="Income"
names(Customers)[4]="Score"
names(Customers)

##Descriptive Stats
##Gender
p=as.data.frame(table(Gender))
ggplot(p, aes(x = Gender, y=Freq, fill= Gender)) +
  geom_bar(stat = "identity")  

##Age
summary(Age)
ggplot(as.data.frame(Age), aes(y = Age)) + geom_boxplot(fill='#F8766D')

##Gender And Age
ggplot(Customers, aes( x = Age, fill = Gender)) + geom_density(alpha = 0.4)


#######################################################
#######################################################

##Performing k-means clustering for income and score

Cust_scaled=scale(Customers[,3:4])
res = get_clust_tendency(Cust_scaled, 40, graph = TRUE)
res

## Hopkins statistic
##      H0: Dataset is Uniformly Distributed (i.e., no meaningful clusters)
##      H1: Dataset is NOT Uniformly Distributed (i.e., contains meaningful clusters)

res$hopkins_stat
##Since H<0.5, meaningful clusters exist in dataset

head(Cust_scaled)
##Since H<0.5, meaningful clusters exist in dataset

fviz_nbclust(Cust_scaled, kmeans, method = "wss") +         ## wss - Within Sum of Squares
  geom_vline(xintercept = 5, line = 2)

##5 clusters assumed
set.seed(100)
km.model = kmeans(Cust_scaled, centers = 5, nstart = 25)

km.model

names(km.model)

table(km.model$cluster)   ## Number of records in each cluster

km.model$centers          ## Cluster means

## Append Cluster number to USArrests dataset
Customers = cbind(Customers, cluster = km.model$cluster)
head(Customers)

##Plotting Clusters
fviz_cluster(km.model, data = Cust_scaled,
             palette = c("red", "orange","black","blue"),
             ellipse.type = "euclid",
             star.plot = TRUE,
             repel = TRUE,
             ggtheme = theme_classic())

##Validating Clustering
library(cluster)
sil = silhouette(km.model$cluster, dist(Cust_scaled))

## Assign Row Names from USArrests dataset
rownames(sil) = rownames(Customers)
install.packages("prettydoc")
## Display first 3 columns
sil[,1:3]

## Visualize sil
fviz_silhouette(sil)

## OBSERVATIONS:
##  Some samples have negative Si - Which ones are those?

neg_sil = which(sil[,"sil_width"] < 0)
neg_sil
sil[neg_sil]
##Customer IDs 43 and 147 in cluster 4 and 5 respectively are not well clustered


######################################################################################
######################################################################################

Cust_scaled=scale(Customers[,2:4])
library(factoextra)
res = get_clust_tendency(Cust_scaled, 40, graph = TRUE)
res

## Hopkins statistic
##      H0: Dataset is Uniformly Distributed (i.e., no meaningful clusters)
##      H1: Dataset is NOT Uniformly Distributed (i.e., contains meaningful clusters)

res$hopkins_stat
##Since H<0.5, meaningful clusters exist in dataset

fviz_nbclust(Cust_scaled, kmeans, method = "wss") +         ## wss - Within Sum of Squares
  geom_vline(xintercept = 4, line = 2)

set.seed(100)
km.model = kmeans(Cust_scaled, centers = 4, nstart = 25)

km.model

names(km.model)

table(km.model$cluster)   ## Number of records in each cluster

km.model$centers          ## Cluster means

## Append Cluster number to USArrests dataset
Customers = cbind(Customers, cluster = km.model$cluster)
head(Customers)

##Plotting Clusters
fviz_cluster(km.model, data = Cust_scaled,
             palette = c("red", "orange","black","blue"),
             ellipse.type = "euclid",
             star.plot = TRUE,
             repel = TRUE,
             ggtheme = theme_classic())

##Validating Clustering
library(cluster)
sil = silhouette(km.model$cluster, dist(Cust_scaled))

## Assign Row Names from USArrests dataset
rownames(sil) = rownames(Customers)

## Display first 3 columns
sil[,1:3]

## Visualize sil
fviz_silhouette(sil)

## OBSERVATIONS:
##  Some samples have negative Si - Which ones are those?

neg_sil = which(sil[,"sil_width"] < 0)
neg_sil
sil[neg_sil]
##Customer ID 129 in cluster 3 is not well clustered
