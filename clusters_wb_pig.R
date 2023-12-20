# Ruiz-Rodríguez et al. A methodological framework to characterize
# the wildlife-livestock interface: the case of wild boar in mainland Spain
# Supplementary Materials
# In this script we reproduce the procedure to compute cluster analyses for the
# wild boar - pig farms interface.

# In this script, the steps for cluster analysis are detailed, but if you wish 
# to obtain the raw data, you can download it from the following link:
'https://raw.githubusercontent.com/CarmenRuiz14/WLinterface/main/data_WLinterface.csv'

# The first step is to load the 'factoextra' library, necessary both for 
# applying different methods of selecting the number of clusters and 
#for applying the function in which the grouping is done with a previously 
#chosen number of clusters.
library(factoextra)

# We load a subset of the wild boar and pig farms data per cell from GitHub
data_WLinterface <- read.csv(
  'https://raw.githubusercontent.com/CarmenRuiz14/WLinterface/main/data_WLinterface.csv')

# We visualize the header of the data
head(data_WLinterface)
#X:'Cell identification number'
#ID:'Cell identification number'
#jab:'Standardized wild boar abundance index'
#ext:'Standarized number of extensive pig farms'
#int:'Standarized number of intensive pig farms'
#red:'Standarized number of small pig farms'
#jab_ex:'Standardized number of extensive pig farms multiplied by the  
# standardized wild boar relative abundance' 
#jab_in:'Standardized number of intensive pig farms multiplied by the  
# standardized wild boar relative abundance'
#jab_re:'Standardized number of small pig farms multiplied by the standardized 
# wild boar relative abundance'
#province:'Administrative territorial demarcation'

#We select the variables of wild boar abundance, pig farms, along with the
# standardized interaction variables to create the clusters
clust <- data_WLinterface[ ,3:9]

#We apply three different statistical methods for selecting the number of 
#clusters using the 'fviz_nclust' function (consult 'help(fviz_nclust)' for 
#more information on the classification methods: 'silhouette,' 'elbow,' and '
#gap_stat.' To apply this function, we set k.max=10 to avoid a large number of 
#clusters that could be uninformative
fviz_nbclust(clust, kmeans, method = "silhouette", k.max = 10)
#Optimal number of clusters obtained with silhouette method = 2

fviz_nbclust(clust, kmeans, method = "wss", k.max = 10)
#Optimal number of clusters obtained with elbow method = it is not conclusive

fviz_nbclust(clust, kmeans, method = "gap_stat", k.max = 10)
#Optimal number of clusters obtained with gap method = 1

#The number of clusters determined by each method does not converge; each method
#proposes a different optimal number of clusters. Therefore, we made several 
#divisions until we found a division that we consider optimal for our data, 
#taking into account the biological sense of the variables and the 
#interpretation of the results so that their application and visualization 
#are practical for management.

#We use the 'kmeans()' function to cluster the data into 2, 4, 6 and 8 clusters
cl_2 <- kmeans(clust, centers = 2)
cl_4 <- kmeans(clust, centers = 4)
cl_6 <- kmeans(clust, centers = 6)
cl_8 <- kmeans(clust, centers = 8)

#We associate each cell with its assigned cluster after applying the 'kmeans()' 
#function.
clust_wb_pig <- cbind(data_WLinterface, cl_2$cluster)
clust_wb_pig <- cbind(clust_wb_pig, cl_4$cluster)
clust_wb_pig <- cbind(clust_wb_pig, cl_6$cluster)
clust_wb_pig <- cbind(clust_wb_pig, cl_8$cluster)

#We rename the column that contains the number of the cluster to which each cell
# belongs as 'cluster'
names(clust_wb_pig)[11] <- 'clust_2'
clust_wb_pig$clust_2 <- as.factor(clust_wb_pig$clust_2)
names(clust_wb_pig)[12] <- 'clust_4'
clust_wb_pig$clust_4 <- as.factor(clust_wb_pig$clust_4)
names(clust_wb_pig)[13] <- 'clust_6'
clust_wb_pig$clust_6 <- as.factor(clust_wb_pig$clust_6)
names(clust_wb_pig)[14] <- 'clust_8'
clust_wb_pig$clust_8 <- as.factor(clust_wb_pig$clust_8)

#Finally we characterize the clusters based on the variables they group using 
#boxplots and compare between different numbers of clusters
library(gridExtra)
library(ggplot2)

#Boxplots when we group data in 2 clusters
bp1 <- ggplot(clust_wb_pig, aes(x = clust_2, y = jab)) + geom_boxplot() +  
  labs(title = "Wild boar") + theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())
bp2 <- ggplot(clust_wb_pig, aes(x = clust_2, y = jab_in)) +   geom_boxplot() +   
  labs(title = "Wb*Intensive pig farms")+ theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())
bp3 <- ggplot(clust_wb_pig, aes(x = clust_2, y = jab_re)) + geom_boxplot() +  
  labs(title = "Wb*Small pig farms")+ theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())
bp4 <- ggplot(clust_wb_pig, aes(x = clust_2, y = jab_ex)) +   geom_boxplot() +   
  labs(title = "Wb*Extensive pig farms")+ theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())

#Boxplots when we group data in 4 clusters
bp5 <- ggplot(clust_wb_pig, aes(x = clust_4, y = jab)) +   geom_boxplot() +   
   theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())
bp6 <- ggplot(clust_wb_pig, aes(x = clust_4, y = jab_in)) +   geom_boxplot() +
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())
bp7 <- ggplot(clust_wb_pig, aes(x = clust_4, y = jab_re)) +   geom_boxplot() +   
   theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())
bp8 <- ggplot(clust_wb_pig, aes(x = clust_4, y = jab_ex)) +   geom_boxplot() +   
   theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())

#Boxplots when we group data in 6 clusters
bp9 <- ggplot(clust_wb_pig, aes(x = clust_6, y = jab)) +   geom_boxplot() +   
   theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())
bp10 <- ggplot(clust_wb_pig, aes(x = clust_6, y = jab_in)) +   geom_boxplot() +   
   theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())
bp11 <- ggplot(clust_wb_pig, aes(x = clust_6, y = jab_re)) +   geom_boxplot() +   
   theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())
bp12 <- ggplot(clust_wb_pig, aes(x = clust_6, y = jab_ex)) +   geom_boxplot() +   
   theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())

#Boxplots when we group data in 8 clusters
bp13 <- ggplot(clust_wb_pig, aes(x = clust_8, y = jab)) +   geom_boxplot() +   
   theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())
bp14 <- ggplot(clust_wb_pig, aes(x = clust_8, y = jab_in)) +   geom_boxplot() +   
   theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())
bp15 <- ggplot(clust_wb_pig, aes(x = clust_8, y = jab_re)) +   geom_boxplot() +   
   theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())
bp16 <- ggplot(clust_wb_pig, aes(x = clust_8, y = jab_ex)) +   geom_boxplot() +   
   theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())

grid.arrange(bp1,bp2,bp3,bp4,bp5,bp6,bp7,bp8,bp9,bp10,bp11,bp12,bp13,bp14,
             bp15,bp16, ncol=4, bottom='Clusters')
