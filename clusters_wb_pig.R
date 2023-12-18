# Ruiz-Rodríguez et al. A methodological framework to describe and characterize
# the wildlife-livestock interface: the case of wild boar in mainland Spain
# Supplementary Materials
# In this script we reproduce the procedure to compute cluster analyses for the
# wild boar - pig farms interface.
#Clusters#poner algp de info: loading library (paquetes necesariospara ver el 
#num clusters)
#esto es un script para el analisis de clusters de los datos pero los datos en 
# bruto pueden descargarse desde aqui: url
'https://raw.githubusercontent.com/CarmenRuiz14/WLinterface/main/data_WLinterface.csv'
library(factoextra)

#Cargar subset de los datos desde github
data_WLinterface <- read.csv(
  'https://raw.githubusercontent.com/CarmenRuiz14/WLinterface/main/data_WLinterface.csv')

#visualizamos cabecera de los datos
head(data_WLinterface)
#comentario donde describa cada columna. jab:standardized wild boar abundance index
#red: standarized number of small pig farms
#int:
#X:
#ID:
#province

#selecciono variables para clusterizar Detallar cuanto mas mejor
clust <- data_WLinterface[ ,3:9]

#Metodos seleccion num. clusters utilizamos esta funcion para obtener numeor 
#ptimo de clusters mediante 3 metodologias diferentes
#consultar help(fviz_nclust) para consultar los metodos shilouette, etc
#fijamos k.max en 10 para evitar un gran numero de clusters que seria useless
fviz_nbclust(clust, kmeans, method = "silhouette", k.max = 10)
#num optimo de cluster que se obtiene es 2

fviz_nbclust(clust, kmeans, method = "wss", k.max = 10)
#num optimo de cluster que se obtiene no es concluyente

fviz_nbclust(clust, kmeans, method = "gap_stat", k.max = 10)
#num optimo de cluster que se obtiene es 1

#Prrafo: num clusters por cada uno de los metodos no converge y por tanto lo que
#vamos a hacer e sunas un numero de clusters que nosotros considderemaos optimo 
#que tenga sentido bilogico y sea practico para el manejo. Dar una vuelta y 
#hablar con Jota

#Computar los clusters. Corremos clusters analisis con el numero de lcusters 
#seleccionado, en nuetsro caso 6
cl_6 <- kmeans(clust, centers = 6)

#asociamos cada observacion a su cluster asignado
clust_wb_pig <- cbind(data_WLinterface, cl_6$cluster)
names(clust_wb_pig)[11] <- 'cluster'

#caracterizacion de clusters con boxplot
boxplot(jab~cluster, data=clust_wb_pig)
boxplot(int~cluster, data=clust_wb_pig)
boxplot(ext~cluster, data=clust_wb_pig)
boxplot(red~cluster, data=clust_wb_pig)