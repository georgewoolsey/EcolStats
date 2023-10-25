# clustering can tell you which groups are different but NMDS may not tell you which groups are different
# see vegan package for nmds and clustering

# adonis test : stress values also indicate the usefulness of the ordination
# where stress <= .1 is good; stress <= .05 is very good
# "there is significant differences in the structure" but not which clusters are differnt

# NMDS data arrangement: rows are site/subject; columns are measurements/species/answer

# see:
#   1) Davis, T. S., Rhoades, P. R., Mann, A. J., & Griswold, T. (2020). Bark beetle outbreak enhances biodiversity and foraging habitat of native bees in alpine landscapes of the southern Rocky Mountains. Scientific Reports, 10(1), 16400.
#   2) Davis, T. S., Horton, D. R., Munyaneza, J. E., & Landolt, P. J. (2012). Experimental infection of plants with an herbivore-associated bacterial endosymbiont influences herbivore host selection behavior. PLoS One, 7(11), e49330.


#libaries
library(vegan)	#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/hclust
library(pvclust)	#https://cran.r-project.org/web/packages/pvclust/index.html
			#https://github.com/shimo-lab/pvclust
library(tidyverse)
# can use DAVIS_Bombus_dataset.csv which is data from 1 above
data_bombus <- read.csv("DAVIS_Bombus_dataset.csv")
#data
data_random		<- 	read.csv("community_random_NMDS.csv", header=T)
data_structured	<- 	read.csv("community_structured_NMDS.csv", header=T)

#first example using random data
#manipulating data into correct format
data_random2	<-	data_random[,3:9]		#removes non-numeric columns
data_random2 %>% head()

data_random3	<-	as.matrix(data_random2)	#creates a matrix object
data_random3 %>% head()

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
test1			<-	pvclust::pvclust(t(data_random3), method.hclust='single', 	#note that if you dont transpose the matrix
				nboot=1000, use.cor='all.obs')				#the function will run from 'species' rather
													#than 'sites'

plot(test1, labels=data_random$habitat)	#no statistically significant groups identified
pvclust::pvrect(test1, alpha=0.99)
# notice rectangle is highlighting one big group...no significant difference across groups

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

# see heatmap to make better publication graphics
###############################################################
###############################################################
###############################################################
###############################################################

#repeat process using structured real data and compare
#manipulating data into correct format
data_structured_bees	<-	data_bombus[,6:ncol(data_bombus)-1]		#removes non-numeric columns
data_structured_bees	

data_structured_bees_mat	<-	as.matrix(data_structured_bees)	#creates a matrix object
data_structured_bees_mat

data_structured_bees_mat2	<-	dist(data_structured_bees_mat)	#making a square 'distance matrix' object
data_structured_bees_mat2

#creating dendrogram object
cluster99		<-	hclust(data_structured_bees_mat2)	

#comparing 'random' to 'structured'
plot(cluster99, labels=data_bombus$site)

#testing clusters for significance using 'pvclust' package
test99			<-	pvclust(t(data_structured_bees_mat), method.hclust='single', 
				nboot=1000, use.cor='all.obs')	

plot(test99, labels=data_bombus$site)
pvrect(test99, alpha=0.95)		

# see heatmap to make better publication graphics

# https://bioinformatics.ccr.cancer.gov/docs/data-visualization-with-r/Lesson5_intro_to_ggplot/