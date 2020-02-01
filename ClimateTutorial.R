# If you don't have these libraries, you will need to use install.packages("LIBRARYNAME") to download them.
# install.packages("raster")
library(raster)
library(sp)
library(rgdal)
library(rasterVis)
library(ggplot2)
library(reshape2)

# Set a working file path where all of the data you download will be saved
setwd("C:/Tutorial/climate_tutorial") # Change if necessary

### SETTING UP THE DATA ###

# Get the global administrative boundary layer shapefile
b <- getData('GADM', country='MNG', level=0) #Level0=country, MNG is Mongolia
aimag <- getData('GADM', country='MNG', level=1) #Level1=aimag, these are like US states
plot(aimag)

# Just to test it out you can download 'WorldClim' bio data directly from the {raster} server
r <- getData("worldclim", var="bio", res=10) # Resultion is 10 arc second (~1km)
bio <- r[[c(1,12)]] # Bio 1 (Mean Annual Temperature) and Bio 12 (Annual Precipitation)
names(bio) <- c("Temp","Prec")

# Take a look!
plot(bio$Temp)
plot(bio$Prec)

# Get shapefiles of ecoregions from the "climate_tutorial/ecoregions" folder
grass <- readOGR(dsn = "C:/Tutorial/climate_tutorial/ecoregion", layer = "grassland")
mount <- readOGR(dsn = "C:/Tutorial/climate_tutorial/ecoregion", layer = "mountain_steppe")
basin <- readOGR(dsn = "C:/Tutorial/climate_tutorial/ecoregion", layer = "terminal_basin")

# Transform shapefiles to be the same projection as WorldClim data
grass.trans <- spTransform(grass, CRS("+proj=longlat +datum=WGS84")) 
mount.trans <- spTransform(mount, CRS("+proj=longlat +datum=WGS84"))
basin.trans <- spTransform(basin, CRS("+proj=longlat +datum=WGS84"))

# Crop/clip WorldClim data to Mongolia boundary/extent
crop <- crop(bio, b)

# Take a look at Grassland and Terminal Basin ecoregion shapefiles on top of the Temperature raster
plot(crop$Temp)
plot(grass.trans, add=TRUE)
plot(basin.trans, add=TRUE)

### CREATING SAMPLE POINTS ###

# Let's create random sample points within Mongolia boundary just to look at what info they can extract
points <- spsample(as(b, 'SpatialPolygons'),n=100, type="random")

# Plot the map of Worldclim temperature data, Monogolia boundary, and sample points
plot(crop[["Temp"]])
plot(b, add=TRUE)
plot(points, add=TRUE)

# Let's create random sample points within the Mountain Steppe ecoregion just to look at it
mount.points <- spsample(as(mount.trans, 'SpatialPolygons'),n=100, type="random")

# Plot the precipitation raster and random sample points in Mountain Steppe ecoregino
plot(crop[["Prec"]])
plot(mount.trans, add=TRUE)
plot(mount.points, add=TRUE)

# Now create a data table of values extracted from random points throughout all of Mongolia
values <- extract(bio, points) # Extract Temp and Precip values from the points
df <- cbind.data.frame(coordinates(points),values) # Creates table of point coordinates and values
head(df) # Just show the top few values of table.
# You can see the point coordinates with the exact temp and precip values extracted at each point.

# Plot all values with regression line to see how Precipitation decreases as Temperature increases
plot(df$Temp, df$Prec) + abline(lm(df$Prec~df$Temp), col="red") 

### MONTHLY MEAN TEMPERATURE ###

## Now let's do an analysis of monthly climate values
# Open WorldClim mean monthly temperature tiles from the folder "climate_tutorial/tmean_10m_bil"
setwd("C:/Tutorial/climate_tutorial/tmean_10m_bil")
mJan <- raster("tmean1.bil") #mean temp for January etc.
mFeb <- raster("tmean2.bil")
mMar <- raster("tmean3.bil")
mApr <- raster("tmean4.bil")
mMar <- raster("tmean5.bil")
mJun <- raster("tmean6.bil")
mJul <- raster("tmean7.bil")
mAug <- raster("tmean8.bil")
mSep <- raster("tmean9.bil")
mOct <- raster("tmean10.bil")
mNov <- raster("tmean11.bil")
mDec <- raster("tmean12.bil")

# will get the basic R plot of the global raster
plot(mFeb)

# Stack the separate tiles into a single image
mstack <- stack(mJan, mFeb, mMar, mApr, mMar, mJun, mJul, mAug, mSep, mOct, mNov, mDec)
cutStack <- (crop(mstack, b))/10 # crop to Mongolia shapefile
names(cutStack) <- month.abb # Names stacks by month

# Plot and create histogram of mean temperature ranges by month. THESE COULD TAKE A MINUTE TO LOAD!
levelplot(cutStack) # Standard color gradient
levelplot(cutStack, col.regions=topo.colors) # This may be easier to see?
histogram(cutStack) # Notice how temperature shifts throughout the year

### MONTHLY PRECIPITATON ###

# Open WorldClim monthly precipitation tiles from the file "climate_tutorial/precip_19"
# These files are already clipped to tiles of smaller areas. #19 is the tile for most of Mongolia.
setwd("C:/Tutorial/climate_tutorial/prec_19")
pJan <- raster("prec1_19.bil")
pFeb <- raster("prec2_19.bil")
pMar <- raster("prec3_19.bil")
pApr <- raster("prec4_19.bil")
pMar <- raster("prec5_19.bil")
pJun <- raster("prec6_19.bil")
pJul <- raster("prec7_19.bil")
pAug <- raster("prec8_19.bil")
pSep <- raster("prec9_19.bil")
pOct <- raster("prec10_19.bil")
pNov <- raster("prec11_19.bil")
pDec <- raster("prec12_19.bil")

# Take a look!
plot(pFeb)
plot(b, add=TRUE)

# Stack separate tiles into a single image
pstack <- stack(pJan, pFeb, pMar, pApr, pMar, pJun, pJul, pAug, pSep, pOct, pNov, pDec)
putStack <- (crop(pstack, b))/10 # crop to Mongolia shapefile
names(putStack) <- month.abb

# Again, see the difference in precipitation between January and July
plot(putStack$Jan)
plot(putStack$Jul)

# Plot and create histogram of mean temperature ranges by month
levelplot(putStack, col.regions=topo.colors)
histogram(putStack) # Notice the dry Mongolian winters, and (relatively) wet summers.

### TEMPERATURE BY ECOREGION ###

# Next you can extract the temperature data from each ecoregion polygon to view differences between them
grassMean <- extract(cutStack, grass.trans)[[1]]
mountMean <- extract(cutStack, mount.trans)[[1]]
basinMean <- extract(cutStack, basin.trans)[[1]]

n <- dim(grassMean)[1] #for sampling convenience
m <- dim(mountMean)[1]
l <- dim(basinMean)[1]

grassMat <- grassMean[sample(n, 10),] #this is the final extracted data
mountMat <- mountMean[sample(m, 10),]
basinMat <- basinMean[sample(l, 10),]

# This code creates the table style using ggplot2
plotter <- data.frame(rbind(grassMat, mountMat, basinMat), region = c(rep("Grass",10), rep("Mount",10), rep("Basin",10)))
forplo <- melt(plotter)
g1 <- ggplot(forplo, aes(x = value, fill = region)) + geom_histogram() + facet_grid(variable~.)

# The plot shows the range of temperature in each ecoregion.
# You can see that the Mountain Steppe is the coolest region, while the Terminal Basin is the hottest with the most diverse range
plot(g1)

### PRECIPITATION BY ECOREGION ###

# Now let's look at precipitation differences
grassPM <- extract(putStack, grass.trans)[[1]]
mountPM <- extract(putStack, mount.trans)[[1]]
basinPM <- extract(putStack, basin.trans)[[1]]

o <- dim(grassPM)[1] #for sampling convenience
p <- dim(mountPM)[1]
q <- dim(basinPM)[1]

grassPrep <- grassPM[sample(o, 100),] #this is the final extracted data
mountPrep <- mountPM[sample(p, 10),]
basinPrep <- basinPM[sample(q, 10),]

prep.plotter <- data.frame(rbind(grassPrep, mountPrep, basinPrep), region = c(rep("Grass",10), rep("Mount",10), rep("Basin",10)))
porplo <- melt(prep.plotter)
g2 <- ggplot(porplo, aes(x = value, fill = region)) + geom_histogram() + facet_grid(variable~.)

# Now you can see that There is mostly NO precipitation in December-Februrary. Summer precipitation is high but infrequent.
plot(g2)

### END ###

### PAY NO ATTENTION TO ME! ###

#v <- extract(crop$Prec, grass.trans)
#v.counts <- lapply(v, table)

#### AVERAGE FROM BUFFER FOR MICROCLIMATE FROM SAMPLE POINTS
#coordinates(chamaerops)<-c("lon", "lat")
#tminCM.IP.sim <- extract(tminCM.IP, chamaerops, method='simple', buffer=5000, fun=mean, df=TRUE) ## If df=TRUE, the results are returned as a 'dataframe'
#head(tminCM.IP.sim) ## Temperature data are in °C × 10 to reduce the file sizes