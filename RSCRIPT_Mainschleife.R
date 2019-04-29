#########################################################################################################
# >>> The effect of drought events on water expanse of the river Main at Mainschleife Volkach <<<

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# what is done:
# classification of water surface to compare low water values to mean- or medium-values,
# to search for difference in horizontal expanse of the river
# [If not, compare to flood condition data for a noticeable difference in result]
# and illustrate surface difference.

#########################################################################################################
# 00) SETUP / PREPARATION 

  # working directory
getwd()
setwd("D:/Dokumente/R_working_directory")
getwd()
list.files("D:/Dokumente/R_working_directory")

  # installation of needed packages
install.packages("raster", repos="http://cran.rstudio.com/")
install.packages("sp", repos="http://cran.rstudio.com/")
install.packages("rgdal")
install.packages("maptools", repos="http://cran.rstudio.com/")
install.packages("sf")
install.packages("rpart", repos="http://cran.rstudio.com/")
install.packages("stringr", repos="http://cran.rstudio.com/")
install.packages("RStoolbox")
install.packages("ggplot2")
install.packages("devtools")

install.packages("randomForest")
install.packages("dplyr") #           testing
install.packages("measurements") #    testing...
install.packages("RColorBrewer")

devtools::install_github("16EAGLE/getSpatialData")

  # calling packages
library(raster)
library(maptools)
library(sp)
library(sf)
library(rpart)
library(readxl)
library(stringr)
library(RStoolbox)
library(ggplot2)
library(devtools)
library(rgdal)
library(getSpatialData)
library(randomForest)
library(dplyr)
library(measurements)
library(RColorBrewer)


#########################################################################################################
# 1) ANALYSIS OF WATER DEPTH MEASUREMENT DATA

  # read Waterlevel measurements Main/Astheim:

waterlevels.df <- read_excel("WaterlevelMainAstheim.xlsx") #            table: 2017-01-01 to 2019-01-24
View(waterlevels.df) #                                                  data inspection...
head(waterlevels.df)
names(waterlevels.df) #                                                 data consist from min, max & mean for dates
str(waterlevels.df)
mode(waterlevels.df)
summary(waterlevels.df) #                                               create statistics

plot(waterlevels.df$meanV) #                                            view water depth oscillations
lines(waterlevels.df$minV, col="red") #                                 add min. daily values
lines(waterlevels.df$maxV, col="blue")

plot(density(waterlevels.df$meanV), col="blue") #                       check waterlevel distribution
shapiro.test(waterlevels.df$meanV) #                                    low p value: significantly different from normal distribution

highest <- sort(waterlevels.df$meanV[], decreasing = TRUE) #            check highest values
plot(highest) #                                                         only few dates with extreme values, large gaps between high levels


  # select dates of the min and mean values for "mean"-column:

highestValues <- waterlevels.df[waterlevels.df$meanV>300,]
lowestValues <- waterlevels.df[waterlevels.df$meanV<80,]
meanValues <- waterlevels.df[waterlevels.df$meanV>100 
                              & waterlevels.df$meanV<121,] #            average/regular water level
lowestValues
View(lowestValues)

View(meanValues)

#########################################################################################################
# 2) STUDY AREA CREATION

x_coord <- c(585056, 587952, 588499, 587424, 583509, 583556) #          x coordinates
y_coord <- c(5524562, 5523987, 5517893, 5517968, 5522053, 5523685) #    y coordinates
XandY <- cbind(x_coord, y_coord)
XandY
p = Polygon(XandY)
ps = Polygons(list(p),1) #                                              polygon with coordinates
Mainschleife = SpatialPolygons(list(ps))
proj4string(Mainschleife) = CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") # set CRS to UTM
plot(Mainschleife)
crs(Mainschleife)


#########################################################################################################
# 3) DATA DOWNLOAD AND PREPARATION
  
  # preparation; using "getSpatialData" package

set_aoi(Mainschleife) #                                                 define study area as AOI
view_aoi() #                                                            view Mainschleife study area in AOI viewer
login_CopHub(username = "flbaum")
set_archive("D:/Dokumente/R_working_directory")


  # compare sensing dates to waterlevels; check cloudcover; search for data

records1 <- getSentinel_query(time_range = c("2018-11-17", "2018-11-23"), platform = "Sentinel-2")
records1 #                                                              no records filtered by cloudcover; check over study area manually
View(records1)
getSentinel_preview(record = records1[1,]) #                            without cloud cover: S2A_MSIL2A_20181118, 83cm water level

records2 <- getSentinel_query(time_range = c("2018-10-14", "2018-10-24"), platform = "Sentinel-2")
View(records2)
getSentinel_preview(record = records2[2,]) #                            without cloud cover: S2B_MSIL2A_20181017, 85cm water level

getSentinel_query(time_range = c("2018-11-13", "2018-11-14"), platform = "Sentinel-2") # no result
getSentinel_query(time_range = c("2018-11-07", "2018-11-09"), platform = "Sentinel-2") # cloud cover
getSentinel_query(time_range = c("2018-10-02", "2018-10-04"), platform = "Sentinel-2") # cloud cover
getSentinel_query(time_range = c("2017-01-24", "2017-01-26"), platform = "Sentinel-2") # no result
# ...
recordsLow <- getSentinel_query(time_range = c("2018-09-18", "2018-09-20"), platform = "Sentinel-2")
View(recordsLow)
getSentinel_preview(record = recordsLow[1,]) #                          without cloud cover:  S2A_MSIL2A_20180919, 80cm water level

recordsMean <- getSentinel_query(time_range = c("2018-07-18", "2018-07-20"), platform = "Sentinel-2")
View(recordsMean)
getSentinel_preview(record = recordsMean[3,]) #                         without cloud cover:  S2B_MSIL2A_20180719, 102cm water level


  # data download / layerstack
  # (data Level-2A: bottom of atmosphere reflection in cartographic geometry // preprocessed)

##low water level:
S2_low_raw <- getSentinel_data(records = recordsLow[c(1), ])

unzip("D:/Dokumente/R_working_directory/get_data/Sentinel-2/S2A_MSIL2A_20180919T102021_N0208_R065_T32UNA_20180919T132226.zip", 
      list=F, junkpaths = FALSE, exdir = "D:/Dokumente/R_working_directory", unzip = "internal", setTimes = FALSE )

file.rename("D:/Dokumente/R_working_directory/S2A_MSIL2A_20180919T102021_N0208_R065_T32UNA_20180919T132226.SAFE", 
            "D:/Dokumente/R_working_directory/20180919") #              change too long filename

list.files("D:/Dokumente/R_working_directory/20180919/GRANULE/L2A_T32UNA_A016936_20180919T102018/IMG_DATA/R10m")

lowlevel <- stack("D:/Dokumente/R_working_directory/20180919/GRANULE/L2A_T32UNA_A016936_20180919T102018/IMG_DATA/R10m/T32UNA_20180919T102021_B02_10m.jp2",
                  "D:/Dokumente/R_working_directory/20180919/GRANULE/L2A_T32UNA_A016936_20180919T102018/IMG_DATA/R10m/T32UNA_20180919T102021_B03_10m.jp2",
                  "D:/Dokumente/R_working_directory/20180919/GRANULE/L2A_T32UNA_A016936_20180919T102018/IMG_DATA/R10m/T32UNA_20180919T102021_B04_10m.jp2",
                  "D:/Dokumente/R_working_directory/20180919/GRANULE/L2A_T32UNA_A016936_20180919T102018/IMG_DATA/R10m/T32UNA_20180919T102021_B08_10m.jp2")
plot(lowlevel) #                                                        19.09.2018

##mean water level:
S2_mean_raw <- getSentinel_data(records = recordsMean[c(1), ])

unzip("D:/Dokumente/R_working_directory/get_data/Sentinel-2/S2B_MSIL2A_20180719T103019_N0208_R108_T32UNA_20180719T145501.zip", 
      list=F, junkpaths = FALSE, exdir = "D:/Dokumente/R_working_directory", unzip = "internal", setTimes = FALSE )

file.rename("D:/Dokumente/R_working_directory/S2B_MSIL2A_20180719T103019_N0208_R108_T32UNA_20180719T145501.SAFE", 
            "D:/Dokumente/R_working_directory/20180719")

list.files("D:/Dokumente/R_working_directory/20180719/GRANULE/L2A_T32UNA_A007141_20180719T103020/IMG_DATA/R10m")

meanlevel <- stack("D:/Dokumente/R_working_directory/20180719/GRANULE/L2A_T32UNA_A007141_20180719T103020/IMG_DATA/R10m/T32UNA_20180719T103019_B02_10m.jp2",
                   "D:/Dokumente/R_working_directory/20180719/GRANULE/L2A_T32UNA_A007141_20180719T103020/IMG_DATA/R10m/T32UNA_20180719T103019_B03_10m.jp2",
                   "D:/Dokumente/R_working_directory/20180719/GRANULE/L2A_T32UNA_A007141_20180719T103020/IMG_DATA/R10m/T32UNA_20180719T103019_B04_10m.jp2",
                   "D:/Dokumente/R_working_directory/20180719/GRANULE/L2A_T32UNA_A007141_20180719T103020/IMG_DATA/R10m/T32UNA_20180719T103019_B08_10m.jp2")
plot(meanlevel) #                                                       19.07.2018
meanlevel

new_names <- c("blue", "green", "red", "NIR") #                         new names for bands
names(lowlevel) <- new_names
names(meanlevel) <- new_names

plotRGB(lowlevel, 4, 2, 1, stretch="lin")
plotRGB(meanlevel, 4, 2, 1, stretch="lin")
crs(lowlevel)
crs(meanlevel)


#########################################################################################################
# 4) CLIPPING AND NDVI TO EXTRACT WATER

mainschleife_low <- crop(lowlevel, Mainschleife)
plotRGB(mainschleife_low, 4, 2, 1, stretch="hist")
NDVI_low <- (mainschleife_low$NIR-mainschleife_low$red)/(mainschleife_low$NIR+mainschleife_low$red)
NDVI_low
plot(NDVI_low)
writeRaster(mainschleife_low, filename=str_c("D:/Dokumente/R_working_directory/mainschleife_low.tif"), format="GTiff", overwrite=T)

mainschleife_mean <- crop(meanlevel, Mainschleife)
plotRGB(mainschleife_mean, 4, 2, 1, stretch="hist")
NDVI_mean <- (mainschleife_mean$NIR-mainschleife_mean$red)/(mainschleife_mean$NIR+mainschleife_mean$red)
NDVI_mean
plot(NDVI_mean)
writeRaster(mainschleife_mean, filename=str_c("D:/Dokumente/R_working_directory/mainschleife_mean.tif"), format="GTiff", overwrite=T)

x <- NDVI_mean #                                                        exclude positive values
values(x)[values(x) > 0.0] = NA
plot(x) #                                                               insufficient

x1 <- NDVI_mean
values(x1)[values(x1) > 0.1] = NA
plot(x1) #                           ==> including areas that are not water; other classification required!

## => Also no significant differences in water surface extent noticeable between low and mean on first look


#########################################################################################################
# 5) ANALYSIS / CLASSIFICATION AND ACCURACY ASSESSMENT

  # test unsupervised classification RStoolbox
ucNDVI <- unsuperClass(NDVI_mean, nClasses = 5)
plot(ucNDVI$map)
ucRGB <- unsuperClass(mainschleife_mean, nClasses = 5)
plot(ucRGB$map)


  # introduce ROI / training areas
td_low <- rgdal::readOGR(str_c(getwd(), "/roi_low.shp")) # load training dataset created in qgis
td_low #                      inspect and view training areas: classes 'water', 'urban', 'field', 'forest', 'ground'
names(td_low)
View(td_low)
plot(td_low)

td_mean <- rgdal::readOGR(str_c(getwd(), "/roi_mean.shp")) 
td_mean
names(td_mean)
plot(td_mean)

  # supervised random forest classification

sc_low <- superClass(mainschleife_low, trainData = td_low,
                     responseCol = "class",
                     filename = "Mainschleife_lowlevel_Class.tif",
                     trainPartition = 0.7) #    classification of water surface cover at 80cm depth
plot(sc_low$map)
SClow <- sc_low$map
plot(SClow)
writeRaster(SClow, filename="Classification_low.tif", format="GTiff", overwrite=TRUE)


  ###
sc_low1 <- superClass(mainschleife_low, trainData = td_low,
                     responseCol = "class",
                     filename = "Mainschleife_lowlevel_Class.tif",
                     trainPartition = 0.2) #    test other fraction of validation

sc_low2 <- superClass(mainschleife_low, trainData = td_low,
                     responseCol = "class",
                     filename = "Mainschleife_lowlevel_Class.tif",
                     trainPartition = 0.8) #    test other fraction of validation
  ###

sc_mean <- superClass(mainschleife_mean, trainData = td_mean,
                     responseCol = "class",
                     filename = "Mainschleife_meanlevel_Class.tif",
                     trainPartition = 0.7) #   classification of water surface cover at 102cm depth
plot(sc_mean$map)
SCmean <- sc_mean$map
writeRaster(SCmean, filename="Classification_mean.tif", format="GTiff", overwrite=TRUE)
## aRaster <- raster("D:/Dokumente/R_working_directory/Classification_mean.tif")
## plot(aRaster)


  # accuracy assessment / validation / confusion matrix

## overall accuracy: Nr of pixels correctly classified / Nr of pixels. no information about individual classes accuracy
## users accuracy: Nr of pixels classified as water are indeed water on ground
##                        Nr of correct class pix in class / by Nr of pix in class
## producers accuracy water: Nr of water pixels that are just classified as water
##                          Nr of correct class pix in class / by Nr of refer pix in class

sc_low$validation$performance
getValidation(sc_low)
getValidation(sc_low, metrics="classwise")

sc_mean$validation$performance
getValidation(sc_mean)
getValidation(sc_mean, metrics="classwise")


#########################################################################################################
# 7) POST CLASSIFICATION AND CHANGE DETECTION / EVALUATION

#CVA is limited to 2 bands. can either be optical bands or two classes of each classification
cvaTEST01 <- rasterCVA(mainschleife_low[[3:4]],mainschleife_mean[[3:4]])
plot(cvaTEST01)

#cvaTEST02 <- rasterCVA(sc_low$map[[4:5]],sc_mean$map[[4:5]])
#cvaTEST02 <- rasterCVA(sc_low[4:5],sc_mean[4:5])
#plot(cvaTEST02)

## now convert sc_XX from rasterLayer to rasterBrick/Stack
## then stack/brick and insert into rasterCVA

sc_low$map
mainschleife_low

  # low water level:
##create binary map of water with two layers
plot(sc_low$map)

sc_low_water <- sc_low$map %in% 5
plot(sc_low_water)
sc_low_water

sc_low_nonwater <- sc_low$map %in% 1:4
plot(sc_low_nonwater)
sc_low_nonwater

water_mask_low <- brick(sc_low_water, sc_low_nonwater)
water_mask_low
plot(water_mask_low)


  # mean water level:
##create binary map of water with two layers
sc_mean_water <- sc_mean$map %in% 5
plot(sc_mean_water)
sc_mean_water

sc_mean_nonwater <- sc_mean$map %in% 1:4
plot(sc_mean_nonwater)
sc_mean_nonwater

water_mask_mean <- brick(sc_mean_water, sc_mean_nonwater)
water_mask_mean
plot(water_mask_mean)


# now CVA:
cvaTEST02 <- rasterCVA(water_mask_low[[1:2]],water_mask_mean[[1:2]])
#...computer sais no

#simpler and more straight way:
changeAnalysis <- sc_mean$map %in% 5 - sc_low$map %in% 5 #    substract low level from mean level
  
  ## now there is a layer indicating:
  ##      => changes from "WATER" to "NO WATER" for positive values
  ##      => changes from "NO WATER" to "WATER" for negative values, and
  ##      => no changes for 0.

  ## meaning that:
  ## -1 : not flooded at mean depths but flooded at low depths (??) -> controversial// misclassifications
  ## +1 : flooded at mean depths but not flooded at low depths

plot(changeAnalysis)
changeAnalysis
writeRaster(changeAnalysis, filename="changeAnalysis.tif", format="GTiff", overwrite=TRUE) # create image object
changeAnalysis <- raster(str_c(getwd(), "/changeAnalysis.tif"))
plot(changeAnalysis)
crs(changeAnalysis)

# calculate change area:
# pixel size 10m*10m:: 1km²=1.000.000m² ;; 1ha=10.000m²...

  ## calculate area of flooded at mean depths but not flooded at low depths (in ha):
NrPix1 <- freq(changeAnalysis, digits = 0, value = 1.0, useNA = "ifany") / 10000
NrPix1

  ## calculate area of not flooded at mean depths but flooded at low depths (in ha):
NrPix2 <- freq(changeAnalysis, digits = 0, value = -1.0, useNA = "ifany")
NrPix2

  ## calculate area of no change (in ha):
NrPix3 <- freq(changeAnalysis, digits = 0, value = 0.0, useNA = "ifany")
NrPix3


#########################################################################################################
# 8) DISPLAY AND ILLUSTRATION

display.brewer.all(n=10, exact.n=FALSE)


#plot water depth data:

pdf("water_level_data.pdf")
grid <- matrix(c(1,1,2,3),nrow = 2,
               ncol = 2, byrow = TRUE)
grid
layout(grid)

plot(waterlevels.df$meanV, col = "white", 
     main = "Daily minima and maxima",
     xlab = "Recording day (2017-01-01 to 2019-01-24)",
     ylab = "Water depth")
lines(waterlevels.df$minV, col="red") 
lines(waterlevels.df$maxV, col="blue")
plot(water_depth,
     main = "Sorted water level measurements",
     xlab = "Recording day",
     ylab = "Water depth",
     type = "o",
     pch = 18)
plot(waterlevels.df$meanV,
     main = "Daily mean value",
     xlab = "Recording day",
     ylab = "Water depth",
     type = "l")
dev.off()

# plot study area:

view_aoi()


# plot false-color Sentinel-2 images:

pdf("false-color Sentinel-2 images.pdf")
grid <- matrix(c(1,2,1,2),nrow = 2,
               ncol = 2, byrow = TRUE)
grid
layout(grid)
plotRGB(mainschleife_low, 4, 2, 1, 
        stretch = "lin", 
        xlab = "19.09.2018; 80cm  depth")    
plotRGB(mainschleife_mean, 4, 2, 1, 
        stretch = "lin", 
        xlab = "19.07.2018; 102cm depth")
dev.off()


# plot e.g. NDVI:

pdf("NDVI images.pdf")
grid <- matrix(c(1,2,1,2),nrow = 1,
               ncol = 2, byrow = TRUE)
grid
layout(grid)
plot(NDVI_low, 
     main = "NDVI; 19.09.2018; 80cm  depth",
     cex.axis = 0.6)
plot(NDVI_mean, 
     main = "NDVI; 19.07.2018; 102cm depth",
     cex.axis = 0.6)
dev.off()


# plot classification results:

pdf("classification results.pdf")
grid <- matrix(c(1,2,1,2),nrow = 1,
               ncol = 2, byrow = TRUE)
grid
layout(grid)
plot(SClow, 
     main = "RF-Classification; 19.09.2018; 80cm  depth",
     cex.axis = 0.6,
     col=brewer.pal(n = 5, name = "RdBu"))
plot(SCmean, 
     main = "RF-Classification; 19.07.2018; 102cm depth",
     cex.axis = 0.6)
dev.off()


# plot change analysis:

pdf("change analysis.pdf")
grid <- matrix(1)
grid
layout(grid)
plot(changeAnalysis, 
     col=brewer.pal(n = 3, name = "RdBu"), 
     main = "Change analysis")
dev.off()
