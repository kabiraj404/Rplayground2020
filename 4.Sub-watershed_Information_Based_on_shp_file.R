
rm(list=ls())

library(raster)
library(rgdal)
library(rasterVis)
library(ggplot2)

#Reading the DEM file
dem <- raster("G:\\workdir_koshi\\srtm30_hgt2_tiff.tif")
#reading the shapefile 
shpFle <- readOGR("G:\\workdir_koshi\\subBasinMolung.shp")

#Plotting the DEM and the location of the shapefile
plot(dem)
plot(shpFle, add = TRUE)

#Extraction of the DEM from the Molung area only 
cropMolung <- crop (dem, shpFle)

#Getting the map of the Molung area only
plot(cropMolung)
plot(shpFle, add = TRUE)
text(shpFle, labels = shpFle$cat)

# Just in case if you want to know the details
# cropMolung
# shpFle
# summary(shpFle)

#finding the number of the sub-watershed in the watersheed 
lengthh <- nrow(shpFle@data)
print(paste("There are", lengthh, "sub-watersheds in the Molung watershed"))

#just in case if you are interested to print the list of subwatershed ID 
names(shpFle@data)


@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ADD THE ATTRIBUTE INFROMATION IN THE FIGURE
#to view the attribute table
shpFle@data
names(shpFle@data)


#selecting the particular sub-Basin
one <- shpFle[shpFle$cat == 3334,]
#one <- shpFle[shpFle$cat == 3366,]

#showing where that subbasin lies in the whole catchment
plot(cropMolung)
plot(one, add = TRUE, col = "red", lwd = 1.5)
plot(shpFle, add = TRUE)

#information of that particular sub-watershed
Subasin1 <- crop (dem, one)
Subasin2 <- mask(Subasin1, one)

#plotting the sub-watershed information 
plot(Subasin2)
plot(one, add = TRUE, border= "black", lwd = 2)

#if better view in 3D is needed. 
slope <- terrain(Subasin2, opt='slope')
aspect <- terrain(Subasin2, opt='aspect')
hill <- hillShade(slope, aspect, 40, 270)
plot(hill, col=grey(0:100/100), legend=FALSE, main='Watershed 1')
plot(Subasin2, col=topo.colors(250, alpha=0.45), add=TRUE)

#Extracting the informaton
minimumElevation <- minValue(Subasin2)
print(paste("The minimum elevation of this sub-watershed is:", minimumElevation , "meters")) 
maximumElevation <- maxValue(Subasin2)
print(paste("The maximum elevation of this sub-watershed is:", maximumElevation , "meters")) 

#finding the size of the sub-watershed. 
cell_size<-area(Subasin2, na.rm=TRUE, weights=FALSE)
#removing the na 
cell_size<-cell_size[!is.na(cell_size)]
#calculating the area of the sub-watershed
areaWatershed<-round(length(cell_size)*median(cell_size), 2)
#printing the value 
print(paste("The area of this sub-watershed is:",  areaWatershed, "km2", sep = " "))

#Identifying the elevation wise area. 
h3 <- hist(Subasin2, breaks = 4) 
#calculation of the percentage 
h3$density = (h3$counts)*median(cell_size)
#to make sure that the total percentage is ok
#sum(h$density)
plot(h3,
	freq=FALSE,
	#breaks = c(1000, 2000, 3000, 4000),
	ylab = expression(paste("Area (km"^"2", ")")),
	xlab = "Elevation",
	main = "Area in each elevation band")

#now creating the slope and aspect
x <- terrain(Subasin2, opt=c('slope', 'aspect'), unit='degrees')

#plotting the slope of the region 
plot(x$slope)

minSlope <- round(minValue(x$slope),2)
print(paste("The minimum slope of the subwatershed is:",  minSlope, "degree", sep = " "))

maxSlope <- round(maxValue(x$slope),2)
print(paste("The minimum slope of the subwatershed is:",  maxSlope, "degree", sep = " "))

##Percentage of slope in the basin in histogram 
slopeHist = hist(x$slope, breaks = 3, plot= FALSE) 
#calculation of the percentage 
slopeHist$density = slopeHist$counts/sum(slopeHist$counts)*100
plot(slopeHist,
	freq=FALSE, 
	ylab = "Percentage",
	xlab = "degree",
	main = "Percentage of the slope in the basin")


#plotting the aspect of the region
plot(x$aspect)


 #creating a function to show the area occupied by each aspect 
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[2] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
}

##calculate the area of the region
m <- c(-1, 90, 1,  90, 180, 2, 180, 270, 3, 270, 370, 4)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- reclassify(x$aspect, rclmat)
freqq <- freq(rc)

a<- as.numeric(freqq[1,2]) #for 1
NW <- round(a * median(cell_size), 0)

B<- as.numeric(freqq[2,2]) #for 2
WS <- round(B * median(cell_size), 0)

cc<- as.numeric(freqq[3,2]) #for 1
SE <- round(cc * median(cell_size), 0)

dd<- as.numeric(freqq[4,2]) #for 2
NE <- round(dd * median(cell_size), 0)


#plotting the area occupied in each aspect. 
dat <- circleFun(c(1,-1),2.3,npoints = 100)

ggplot(dat,aes(x,y)) + 
		geom_path() +
		theme_void() +
		geom_segment(aes(x=-1, y=-1, xend=0.2, yend=-1), colour="blue", size = 1) + #west
		geom_segment(aes(x=-2.2, y=-1, xend=-1, yend=-1), colour="blue", size = 1) + #east
		geom_segment(aes(x=-1, y=-1, xend=-1, yend=0.2), colour="blue", size = 1) + #north
		geom_segment(aes(x=-1, y=-1, xend=-1, yend=-2.2), colour="blue", size = 1) +#south
		annotate("text", x = -1, y=0.3, label = "North") +
		annotate("text", x = -1, y=-2.3, label = "South") +
		annotate("text", x = -2.3, y=-1, label = "East") +
		annotate("text", x = 0.3, y=-1, label = "West") +
		annotate("text", x = -0.57, y=-0.7, label = print(paste("Area ~", NW, "sq km "))) +
		annotate("text", x = -0.57, y=-1.2, label = print(paste("Area ~", WS, "sq km "))) +
		annotate("text", x = -1.67, y=-1.2, label = print(paste("Area ~", SE, "sq km "))) +
		annotate("text", x = -1.67, y=-0.7, label = print(paste("Area ~", NE, "sq km "))) 
	















