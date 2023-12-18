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
#more information on the classification methods: 'silhouette,' 'wss,' and '
#gap_stat.' To apply this function, we set k.max=10 to avoid a large number of 
#clusters that could be uninformative
fviz_nbclust(clust, kmeans, method = "silhouette", k.max = 10)
#Optimal number of clusters obtained = 2

fviz_nbclust(clust, kmeans, method = "wss", k.max = 10)
#Optimal number of clusters obtained = it is not conclusive

fviz_nbclust(clust, kmeans, method = "gap_stat", k.max = 10)
#Optimal number of clusters obtained = 1

#The number of clusters determined by each method does not converge; each method
#proposes a different optimal number of clusters. Therefore, we make a division 
#that we consider optimal for our data, taking into account the biological sense
#of the variables and the interpretation of the results so that their 
#application and visualization are practical for management.

#We use the 'kmeans()' function to cluster the data into 6 clusters
cl_6 <- kmeans(clust, centers = 6)

#We associate each cell with its assigned cluster after applying the 'kmeans()' 
#function.
clust_wb_pig <- cbind(data_WLinterface, cl_6$cluster)

#We rename the column that contains the number of the cluster to which each cell
# belongs as 'cluster'
names(clust_wb_pig)[11] <- 'cluster'

#Finally we characterize the clusters based on the variables they group using 
#boxplots
boxplot(jab~cluster, data=clust_wb_pig)
boxplot(int~cluster, data=clust_wb_pig)
boxplot(ext~cluster, data=clust_wb_pig)
boxplot(red~cluster, data=clust_wb_pig)