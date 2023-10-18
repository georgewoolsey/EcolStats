#libaries
library(vegan)

#data
data_random		<- 	read.csv("community_random_NMDS.csv", header=T)
data_structured	<- 	read.csv("community_structured_NMDS.csv", header=T)

#manipulating data into correct format
data_random2	<-	data_random[,3:9]		#removes non-numeric columns
data_random2	

data_random3	<-	as.matrix(data_random2)	#creates a matrix object
data_random3

ordination_1	<- 	metaMDS(data_random3, 
				trymax="1000")		#making the NMDS object
ordination_1

stressplot(ordination_1)				#diagnostic plot
plot(ordination_1, cex=2)				#plotting unaltered example
plot(ordination_1, cex=2, display="sites")	#plotting unaltered example, sites
plot(ordination_1, cex=2, display="species")	#plotting unaltered example, species#

#testing for significance
test1			<-	adonis2(data_random3~data_random$habitat)	#PERMANOVA
test1

#dirty example of plotting
color_vector<- rep(c(1,2,3), each=10)		#a vector to specify color
color_vector
plot(ordination_1, type="n")					#plotting in correct dimension, whitholding points
points(ordination_1, pch=21, bg=color_vector, cex=2)	#plotting points, specifying color


#repeat the process with the 'structured' dataset and compare
data_structured2	<-	data_structured[,3:9]		#removes non-numeric columns
data_structured2	

data_structured3	<-	as.matrix(data_structured2)	#creates a matrix object
data_structured3

ordination_2	<- 	metaMDS(data_structured3, 
				trymax="1000")		#making the NMDS object
ordination_2

stressplot(ordination_2)				#diagnostic plot
plot(ordination_2, cex=2)				#plotting unaltered example
plot(ordination_2, cex=2, display="sites")	#plotting unaltered example, sites
plot(ordination_2, cex=2, display="species")	#plotting unaltered example, species


#dirty example of plotting
color_vector<- rep(c(1,2,3), each=10)		#a vector to specify color
color_vector
plot(ordination_2, type="n")					#plotting in correct dimension, whitholding points
points(ordination_2, pch=21, bg=color_vector, cex=2)	#plotting points, specifying color

#testing for significance
test2			<-	adonis2(data_structured3~data_structured$habitat)	#PERMANOVA
test2