#Clusters#
library(factoextra)
wb_pig <-read.csv('C:/wb_pig.csv',header = TRUE, sep = ";")
clust<-wb_pig[ ,2:8]#selecciono variables para clusterizar

#Metodos seleccion num. clusters
fviz_nbclust(clust, kmeans, method = "silhouette", k.max = 10)
fviz_nbclust(clust, kmeans, method = "wss", k.max = 10)
fviz_nbclust(clust, kmeans, method = "gap_stat", k.max = 10)
#Seleccion manual num.clusters
cl_6 <- kmeans(clust, centers = 6)
clust_wb_pig <- cbind(wb_pig, cl_6$cluster)#unon num.cluster con resto de datos


