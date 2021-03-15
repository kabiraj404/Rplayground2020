
rm(list=ls())

library(raster)
library(rgdal)
#library(rasterVis)
library(ggplot2)

#slope <- raster("D:\\OneDriveeDrive - ICIMOD\\Watershed_profiling\1\\Watershed_profiling\\1.working_together_with_DODSC\\1.Watershed_delination\\Test\\Slope.tif")
dem <- raster("G:\\workdir_koshi\\srtm30_hgt2_tiff.tif")
dem <- raster("D:\\OneDrive - ICIMOD\\Watershed_profiling\\1.working_together_with_DODSC\\1.Watershed_delination\DEM\srtm_30\\srtm30_hgt2_tiff.tif")
shpFle <- readOGR("G:\\workdir_koshi\\subBasinMolung.shp")

plot(dem)
plot(shpFle, add = TRUE)



# hist(dem, 
#      main= "elevation and frequency",
#      xlab = "elevation",
#      ylab= "frequency")
# 
# hist(dem, 
#      #breaks = 5,
#      breaks = c(0, 2500, 5000, 10000),
#      main= "elevation and frequency",
#      xlab = "elevation",
#      ylab= "frequency")

#helpful to get the median, maximum and minimum of the location
#summary(dem)

# hist(dem,
# xlim = c(0, 9000))

## showing the Molung watershed and its subbasins
cropMolung <- crop (dem, shpFle)
plot(cropMolung)
plot(shpFle, add = TRUE)
text(shpFle, labels = shpFle$cat)

#cropMolung
#shpFle

#summary(shpFle)

#to view the attribute table

lengthh <- nrow(shpFle@data)

print(paste("There are", lengthh, "sub-watersheds in the Molung watershed"))

#To plot the sub-watershed ID in the figure
plot(cropMolung)
plot(shpFle, add = TRUE)
text(shpFle, labels = shpFle$cat)

#to print the list of the sub-watershed ID
names(shpFle@data)


#selecting the particular sub-Basin
one <- shpFle[shpFle$cat == 3334,]
one <- shpFle[shpFle$cat == 3366,]

#showing where that subbasin lies in the whole catchment
plot(cropMolung)
plot(one, add = TRUE, col = "red", lwd = 1.5)
plot(shpFle, add = TRUE)

#information of that particular sub-Basin
Subasin1 <- crop (dem, one)
Subasin2 <- mask(Subasin1, one)

plot(Subasin2)
plot(one, add = TRUE, border= "black", lwd = 2)

#BETTER VIEW 
slope <- terrain(Subasin2, opt='slope')
aspect <- terrain(Subasin2, opt='aspect')
hill <- hillShade(slope, aspect, 40, 270)
plot(hill, col=grey(0:100/100), legend=FALSE, main='Watershed 1')
plot(Subasin2, col=topo.colors(250, alpha=0.45), add=TRUE)

##
minimumElevation <- minValue(Subasin2)
print(paste("The minimum elevation of the basin is :", minimumElevation , "meters")) 
maximumElevation <- maxValue(Subasin2)
print(paste("The maximum elevation of the basin is :", maximumElevation , "meters")) 

#finding the size of the cell
## useful link https://caucasus-spiders.info/r-spatial/raster-basics-3/ 
cell_size<-area(Subasin2, na.rm=TRUE, weights=FALSE)
#removing the na 
cell_size<-cell_size[!is.na(cell_size)]
#calculating the area of the sub-watershed
areaWatershed<-round(length(cell_size)*median(cell_size), 2)
#printing the value 
print(paste("The Area of the sub-watershed is:",  areaWatershed, "km2", sep = " "))

###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# h <- hist(Subasin2, breaks = 5) 
# #calculation of the percentage 
# h$density = h$counts/sum(h$counts)*100
# #to make sure that the total percentage is ok
# #sum(h$density)
# plot(h,
# freq=FALSE,
# #breaks = c(1000, 2000, 3000, 4000),
# ylab = "Percentage",
# main = "Percentage of the elevation")
############@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#h3 <- hist(Subasin2, breaks = 4) 
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


# print(paste("The Area of the sub-watershed is:",  areaWatershed, "km2", sep = " ")) 


###################################################################
# dem2 <- dem


# ellsiz <-area(dem2, na.rm=TRUE, weights=FALSE)
# cellsiz<-cellsiz[!is.na(cellsiz)]


#calculating the area of the sub-watershed
# areaWatershed<-round(length(cell_size)*median(cell_size), 2)

# dem2$area2 = cellsiz*1000000

# hist(dem, 
# #breaks = 4,
# #freq = FALSE,
# #breaks = c(0, 2500, 5000, 10000),
# main= "elevation and frequency",
# xlab = "elevation",
# ylab= "frequency")


# h = hist(dem2, plot= FALSE) 
# #calculation of the percentage 
# h$area2 = cellsiz*1000000

# h$area2

# #h$density = h$counts/sum(h$counts)*100
# #to make sure that the total percentage is ok
# #sum(h$density)
# plot(h,
# freq=FALSE, 
# ylab = "Percentage",
# main = "Percentage of the elevation")


########################################2222222222222222222
# plot(hill, col=grey(0:100/100), legend=FALSE, main='Watershed 1')

# rasterVis::levelplot(Subasin2,
# margin = list(x = TRUE, 
# y = TRUE),
# col.regions = topo.colors(200),
# xlab = list(label = "", 
# vjust = -0.25),
# sub = list(
# label = "masl",
# font = 1,
# cex = .9,
# hjust = 1.5))



#########@@@@@@@@@@@@@@@@@@@@@@@@

# #BETTER VIEW 
# slope <- terrain(Subasin2, opt='slope')
# aspect <- terrain(Subasin2, opt='aspect')
# hill <- hillShade(slope, aspect, 40, 270)
# plot(hill, col=grey(0:100/100), legend=FALSE, main='Watershed 1')
# plot(Subasin2, col=topo.colors(250, alpha=0.45), add=TRUE)


#############################################################


# h = hist(Subasin2, breaks = 5, plot= FALSE) 
# #calculation of the percentage 
# h$density = h$counts/sum(h$counts)*100
# #to make sure that the total percentage is ok
# #sum(h$density)
# plot(h,
# freq=FALSE,
# #breaks = c(1000, 2000, 3000, 4000),
# ylab = "Percentage",
# main = "Percentage of the elevation")




#now creating the slope and aspect
x <- terrain(Subasin2, opt=c('slope', 'aspect'), unit='degrees')
#plot(x)

#plotting the single of slope layer if needed
plot(x$slope) ##title 

maxSlope <- round(maxValue(x$slope),2)
print(paste("The minimum slope of the subwatershed is:",  maxSlope, "degree", sep = " "))
minSlope <- round(minValue(x$slope),2)
print(paste("The minimum slope of the subwatershed is:",  minSlope, "degree", sep = " "))


##Percentage of slope in the basin in histogram 
slopeHist = hist(x$slope, breaks = 3, plot= FALSE) 
#calculation of the percentage 
slopeHist$density = slopeHist$counts/sum(slopeHist$counts)*100
plot(slopeHist,
     freq=FALSE, 
     ylab = "Percentage",
     xlab = "degree",
     main = "Percentage of the slope in the basin")



# plot(x$aspect)


# #Percentage of the aspect of the basin in histogram 
# aspectHist = hist(x$aspect, plot= FALSE) 
# #calculation of the percentage 
# aspectHist$density = aspectHist$counts/sum(aspectHist$counts)*100
# plot(aspectHist,
# freq=FALSE, 
# #breaks = 4,
# ylab = "Percentage",
# xlab = "degree",
# main = "Percentage of the aspect in the basin")
##############################################################











# as <- x$aspect

# aspectHist$density <- as$counts

# image(as, zlim= c(0, 45), main = "south east facing region")


# #to view the attribute table
# names(as@data)
# as@aspect

# names(shpFle@data)




# plot(rc, type = n)


# box(x$aspect)

# hist(rc, breaks = 4)

# levels(rc) <- list(1 = "C", 2 = "a", 3 = "b", 4 = "bb")

# legend(x = 1, y = 10000, legend = print(paste("nw direction is %", one, " hr")), col = c(1:3), pch = 16)
# legend(x = 86.38, y = 27.47, legend = print(paste("nw direction is %", one, " hr")), col = c(1:3), pch = 16)


# hist(rc, breaks = 4)

# r <- raster(ncols=36, nrows=18)
# values(r) <- runif(ncell(r))
# reclassify the values into three groups
# all values > 0 and <= 0.25 become 1, etc.







# one <- 44

# hist(rc, text = one)

# plot




# hist(rc, breaks = 4)

# rc$counts





#pie(as.integer(rc))

# for values >= 0 (instead of > 0), do
# rc <- reclassify(r, rclmat, include.lowest=TRUE)
# # equivalent to
# rc <- reclassify(r, c(-Inf,0.25,1, 0.25,0.5,2, 0.5,Inf,3))



##############################################

plot(x$aspect)


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





dat <- circleFun(c(1,-1),2.3,npoints = 100)

#geom_path will do open circles, geom_polygon will do filled circles
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
















