#packages
library(lme4)		#for random effects models, glmer
library(corrplot)	#for plotting correlations

#data
data1<- read.csv("lizard_data_glm.csv", head=T)
dim	(data1)
head	(data1)


#DOCUMENTATION: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/glm	
#GLM help file

attach	(data1)

#simple pre-analysis diagnostics
hist		(mean_tail_length)					#diagnostic check for distribution

corr_matrix	<- cor(data1[,3:7])					#making a correlation matrix from numeric data
corr_matrix

testRes		<- cor.mtest(corr_matrix, conf.level=0.90)		#creating a correlation test
corrplot	(corr_matrix, method="ellipse", p.mat=testRes$p)	#plotting the correlation test

#basic model using all variables in dataset

model.1	<- glm(mean_tail_length	~	habitat.type+
					year+
					insect_abundance+
					lizard_density,
					family=gaussian(link="identity")		#normal distribution
		)

model.1
summary	(model.1)
hist	(model.1$residuals)

#same model, different link function
model.2	<- glm(mean_tail_length	~	habitat.type+
					year+
					insect_abundance+
					lizard_density,
					family=Gamma(link="identity")			#positive values only possible
		)

model.2
summary	(model.2)
hist	(model.2$residuals)

#lets add an interaction term (using '*' instead of '+')

model.3	<- glm(mean_tail_length	~	habitat.type+
					year+
					insect_abundance*				#interaction term added
					lizard_density,
					family=Gamma(link="identity"),
		)

model.3
summary	(model.3)
hist	(model.3$residuals)

#lets add a random effect and look at model structure again, using 'glmer'

model.4	<- glmer(mean_tail_length ~	habitat.type+
					insect_abundance+
					lizard_density+
					(1|year),				#specifying a random effect
					family=Gamma(link="identity"),
		)

summary	(model.4)


#plotting significant effects in an xy framework
plot		(lizard_density, mean_tail_length, 
		cex=2, xlab="Lizards per hectare", ylab="mean tail length, cm")		#some graphical parameters
model_1.3.1	<- lm(mean_tail_length~lizard_density)					#visualizing significant result
abline		(model_1.3.1)								#plotting line
points		(lizard_density, mean_tail_length, cex=2, pch=21, 			#coloring points by habitat
		bg=year)

plot		(insect_abundance, mean_tail_length,
		cex=2, xlab="Insect abundance", ylab="mean tail length, cm")		#some graphical parameters
model_1.3.2	<- lm(mean_tail_length~insect_abundance)				#visualizing significant result
abline		(model_1.3.2)								#plotting line
points		(insect_abundance, mean_tail_length, cex=2, pch=21, 			#coloring points by habitat
		bg=year)

detach		(data1)

#practice making a GLM to predict dNPP and aNPP using data from  
#https://doi.org/10.1002/eap.2704
#TRY: (1) making a 'best' model and (2) comparing effect sizes in a 'sensible' model