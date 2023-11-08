#library
library(ncf)

#data
data1<- read.csv("DAVIS_Bombus_dataset_reduced.csv", head=T)
str(data1)

data2<- data1[244:280,]		#subsetting to a single study
str(data2)

#making spline correlogram objects, whole dataset
ncf1<- ncf::spline.correlog(x=data1$long, y=data1$lat, z=data1$centralis, latlon=T,	#200 resamples
				resamp=200)	
ncf2<- ncf::spline.correlog(x=data1$long, y=data1$lat, z=data1$centralis, latlon=T,	#1000 resamples
				resamp=1000)
str(ncf1)


#making spline correlogram objects, one study
ncf3<- spline.correlog(x=data2$long, y=data2$lat, z=data2$centralis, latlon=T,	#200 resamples
				resamp=200)
ncf4<- spline.correlog(x=data2$long, y=data2$lat, z=data2$centralis, latlon=T,	#1000 resamples
				resamp=1000)

#plotting and comparing boostrap resamples

par(mfrow=c(2,1))
plot(ncf1)
plot(ncf2)

plot(ncf3)
plot(ncf4)

# to fix for auto correlation
# can use grouping at distance where autocorrelation is a problem
# include the group as a factor in model
# run second model on residuals to get estimates of covariates with the spatial dependence removed
# see Davis et al. 2022 on NPP, treatment, disturbance recovery
# https://scholar.google.com/scholar?cluster=3799449034981948335&hl=en&as_sdt=0,6

# investigate a different study to look for autocorrelation
# do a cross-correlation to investigate species autocorrelation in multiple species