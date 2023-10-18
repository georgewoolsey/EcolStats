#libaries
library(vegan)	#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/hclust
library(pvclust)	#https://cran.r-project.org/web/packages/pvclust/index.html
			#https://github.com/shimo-lab/pvclust

#data
data_random		<- 	read.csv("community_random_NMDS.csv", header=T)
data_structured	<- 	read.csv("community_structured_NMDS.csv", header=T)

#first example using random data
#manipulating data into correct format
data_random2	<-	data_random[,3:9]		#removes non-numeric columns
data_random2	

data_random3	<-	as.matrix(data_random2)	#creates a matrix object
data_random3

distance_matrix	<-	dist(data_random3)	#making a square 'distance matrix' object
distance_matrix

#creating dendrogram objects and applying different methods (default is 'complete')
cluster1		<-	hclust(distance_matrix)	
cluster2		<-	hclust(distance_matrix, method='single')
cluster3		<-	hclust(distance_matrix, method='average')
cluster4		<-	hclust(distance_matrix, method='centroid')
cluster5		<-	hclust(distance_matrix, method='ward.D')
cluster6		<-	hclust(distance_matrix, method='ward.D2')

#comparing different methods
par(mfrow=c(2,3))
plot(cluster1)
plot(cluster2)
plot(cluster3)
plot(cluster4)
plot(cluster5)
plot(cluster6)

#testing clusters for significance using 'pvclust' package
test1			<-	pvclust(t(data_random3), method.hclust='single', 	#note that if you dont transpose the matrix
				nboot=1000, use.cor='all.obs')				#the function will run from 'species' rather
													#than 'sites'

plot(test1, labels=data_random$habitat)	#no statistically significant groups identified
pvrect(test1, alpha=0.99)


#repeat process using structured data and compare
#manipulating data into correct format
data_structured2	<-	data_structured[,3:9]		#removes non-numeric columns
data_structured2	

data_structured3	<-	as.matrix(data_structured2)	#creates a matrix object
data_structured3

distance_matrix2	<-	dist(data_structured3)	#making a square 'distance matrix' object
distance_matrix2

#creating dendrogram object
cluster7		<-	hclust(distance_matrix2)	

#comparing 'random' to 'structured'
par(mfrow=c(1,2))
plot(cluster1, labels=data_random$habitat)
plot(cluster7, labels=data_structured$habitat)

#testing clusters for significance using 'pvclust' package
test2			<-	pvclust(t(data_structured3), method.hclust='single', 
				nboot=1000, use.cor='all.obs')	

plot(test2, labels=data_structured$habitat)
pvrect(test2, alpha=0.95)		