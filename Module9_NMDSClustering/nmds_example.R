# clustering can tell you which groups are different but NMDS may not tell you which groups are different
# see vegan package for nmds and clustering

# adonis test : stress values also indicate the usefulness of the ordination
# where stress <= .1 is good; stress <= .05 is very good
# "there is significant differences in the structure" but not which clusters are different

# NMDS data arrangement: rows are site/subject; columns are measurements/species/answer

# see:
#   1) Davis, T. S., Rhoades, P. R., Mann, A. J., & Griswold, T. (2020). Bark beetle outbreak enhances biodiversity and foraging habitat of native bees in alpine landscapes of the southern Rocky Mountains. Scientific Reports, 10(1), 16400.
#   2) Davis, T. S., Horton, D. R., Munyaneza, J. E., & Landolt, P. J. (2012). Experimental infection of plants with an herbivore-associated bacterial endosymbiont influences herbivore host selection behavior. PLoS One, 7(11), e49330.
# can use DAVIS_Bombus_dataset.csv which is data from 1 above
data_bombus <- read.csv("DAVIS_Bombus_dataset.csv")

#libaries
library(vegan)
library(tidyverse)

#data
data_random		<- 	read.csv("community_random_NMDS.csv", header=T)
data_structured	<- 	read.csv("community_structured_NMDS.csv", header=T)

#manipulating data into correct format
data_random2	<-	data_random[,3:9]		#removes non-numeric columns
data_random2 %>% dplyr::glimpse()

data_random3	<-	as.matrix(data_random2)	#creates a matrix object
data_random3 %>% head()

ordination_1	<- 	vegan::metaMDS(data_random3, 
				trymax="1000")		#making the NMDS object
ordination_1
# note the stress value of 0.21

vegan::stressplot(ordination_1)				#diagnostic plot
# good linear fit is above 0.9
plot(ordination_1, cex=2)				#plotting unaltered example
plot(ordination_1, cex=2, display="sites")	#plotting unaltered example, sites
plot(ordination_1, cex=2, display="species")	#plotting unaltered example, species#
# ... but don't know which grouping/ordination these are so have to attach that to plot another way
#testing for significance
test1			<-	vegan::adonis2(data_random3~data_random$habitat)	#PERMANOVA
test1
# non significant ordination: that means that the grouping does not find differnces by habitat type
#dirty example of plotting
color_vector<- rep(c(1,2,3), each=10)		#a vector to specify color
color_vector
plot(ordination_1, type="n")					#plotting in correct dimension, whitholding points
points(ordination_1, pch=21, bg=color_vector, cex=2)	#plotting points, specifying color
# this plot is colored by habitat type
# this is an indication that these sites are similar

#repeat the process with the 'structured' dataset and compare
data_structured2	<-	data_structured[,3:9]		#removes non-numeric columns
data_structured2	%>% head()

data_structured3	<-	as.matrix(data_structured2)	#creates a matrix object
data_structured3 %>% head()

ordination_2	<- 	vegan::metaMDS(data_structured3, 
				trymax="1000")		#making the NMDS object
ordination_2
# note the better stress value of 0.07
vegan::stressplot(ordination_2)				#diagnostic plot
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


# use bombas data to figure out what is going on with outlier site after 