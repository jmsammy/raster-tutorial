# Intro to spatial analysis tutorial
# Satellite data available from https://scihub.copernicus.eu/

# Joshua Sammy js1689@york.ac.uk
# 03-12-2018
##############################################################

# Not setting working directory as this is part of a version-controlled project

# Load packages

# If you haven't installed the packages before, use e.g.:
# install.packages("sp")

library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)

# Load data
tay <- raster('taycrop.tif')

# Get properties of the Tay raster
tay

# Look at each of the 12 bands separately
b1 <- raster('taycrop.tif', band=1)
b2 <- raster('taycrop.tif', band=2)
b3 <- raster('taycrop.tif', band=3)
b4 <- raster('taycrop.tif', band=4)
b5 <- raster('taycrop.tif', band=5)
b6 <- raster('taycrop.tif', band=6)
b7 <- raster('taycrop.tif', band=7)
b8 <- raster('taycrop.tif', band=8)
b9 <- raster('taycrop.tif', band=9)
b10 <- raster('taycrop.tif', band=10)
b11 <- raster('taycrop.tif', band=11)
b12 <- raster('taycrop.tif', band=12)

# Compare rasters to see whether their extent, number of rows and column, projection, resolution and origin are the same

compareRaster(b2, b3)
compareRaster(b4, b12)

# Plot Rasters. You can use plot() or image(). plot() only plots 100000 pixels, image 'stretches the view' (?) does this mean it actually stretches the image, or that it plots more pixels?

plot(b8)

image(b8)

#It actually just stretches the image

# You can zoom in on bits of the plot

plot(b8)
zoom(b8)    # run this line, then click twice on your plot to define a box. when you click you are defining corners of the box.

# Alternatively, an extent can be cropped and plotted from the plot image using the same double click method described above and the code below. 
# Zooming in allows you to visualise spatial data for specific areas you might be interested in.

plot(tay)
e <- drawExtent()    # run this line, then click twice on your plot to define a box
cropped_tay <- crop(b7, e)
plot(cropped_tay)

# Bands can be plotted using different colour palettes

png('tayplot.png', width = 4, height = 4, units = "in", res = 300)                	# to save plot
image(b8, col= viridis_pal(option="D")(10), main="Sentinel 2 image of Loch Tay")
dev.off()         									# to save plot
# dev.off() is a function that "clears the slate" - it just means you are done using that specific plot
# if you don't dev.off(), that can create problems when you want to save another plot

# That chunk of code (70-74) is for saving the plot. If you wanted to just look at the plot with the colour bands, use this:

image(b8, col= viridis_pal(option="D")(10), main="Sentinel 2 image of Loch Tay")

# We can also create a red/green/blue image, so that it looks like what it would look like irl. 
# In order to do this, we're going to make a group of raster objects, called a 'raster stack', that contains the red (b4), green (b3) and blue (b2) bands.

# this code specifies how we want to save the plot
png('RGB.png', width = 5, height = 4, units = "in", res = 300)
tayRGB <- stack(list(b4, b3, b2))              # creates raster stack
plotRGB(tayRGB, axes = TRUE, stretch = "lin", main = "Sentinel RGB colour composite")
dev.off()
## Making a false colour composite (FCC), where the red, green, and blue bands have been replaced in order to accentuate vegetation

gplot(b8) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  # value is the specific value (of reflectance) each pixel is associated with
  scale_fill_viridis_c() +
  coord_quickmap() +
  ggtitle("West of Loch tay, raster plot") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +   					    # removes defalut grey background
  theme(plot.title = element_text(hjust = 0.5),             # centres plot title
        text = element_text(size=20),		       	    # font size
        axis.text.x = element_text(angle = 90, hjust = 1))  # rotates x axis text

ggsave("ggtay.png", scale = 1.5, dpi = 300) 		# to save plot

# To visualise all the bands together, we can use facet_wrap in gplot. First, we will create a stack of all the bands, so just putting them all on top of each other, like layers in a cake.

t <- stack(b1,b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12)

# Then we can use t to make a faceted plot

gplot(t) +
  geom_raster(aes(x = x, y = y, fill = value))+
  scale_fill_viridis_c() +
  facet_wrap(~variable) +
  coord_quickmap()+
  ggtitle("Sentinel 2 Loch tay, raster plots") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("allbands.png", scale = 1.5, dpi = 300) # to save plot

# To do all of this quickly, the original raster file can loaded as a raster 'brick' and plotted all at once:

s_tay <- brick('taycrop.tif')
plot(s_tay)

## Using NVDI ratio - a value that indicates how likely it is that vegetation will be present. The closer the value is to 1, the higher the probability of vegetation presence.

# NDVI

# Created a VI function (vegetation index)
VI <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}

# For Sentinel 2, the relevant bands to use are:
# NIR = 8, red = 4

ndvi <- VI(s_tay, 8, 4)
# 8 and 4 refer to the bands we'll use

png('ndviplot.png', width = 4, height = 4, units = "in", res = 300)
plot(ndvi, col = rev(terrain.colors(10)), main = 'Sentinel 2, Loch Tay-NDVI')
dev.off()

# So, high probability of vegetation presence around lake?

# Create histogram of NDVI data

png('ndvihist.png', width = 4, height = 4, units = "in", res = 300)
hist(ndvi,
     main = "Distribution of NDVI values",
     xlab = "NDVI",
     ylab= "Frequency",
     col = "aquamarine3",
     xlim = c(-0.5, 1),
     breaks = 30,
     xaxt = 'n')
axis(side = 1, at = seq(-0.5,1, 0.05), labels = seq(-0.5,1, 0.05))
dev.off()

# Right-skewed values, likely to be dominated by vegetation

# We can then hide values that are very low, and less likely to be vegetation:

# Mask cells that have NDVI of less than 0.4 (less likely to be vegetation)

png('ndvimask.png', width = 4, height = 4, units = "in", res = 300)

veg <- reclassify(ndvi, cbind(-Inf, 0.4, NA))
# We are reclassifying our object and making all values between
# negative infinity and 0.4 be NAs

plot(veg, main = 'Veg cover')
dev.off()

# We can save all this stuff as a raster, rather than as an image, if we want to have it available for use later

writeRaster(x = ndvi,
            
            # where your file will go - update with your file path!
            
            filename="tay_ndvi_2018.tif", 	
            format = "GTiff", 					# save as a tif
            datatype = 'INT2S') 					# save as a INTEGER rather than a float


# Let's do some analysis. We're going to be doing kmeans clustering.
# convert the raster to vector/matrix ('getValues' converts the RasterLAyer to array) )

nr <-getValues(ndvi)
str(nr)

# important to set the seed generator because `kmeans` initiates the centres in random locations
# the seed generator just generates random numbers

set.seed(99)

# create 10 clusters, allow 500 iterations, start with 5 random sets using 'Lloyd' method

kmncluster <- kmeans(na.omit(nr), centers = 10, iter.max = 500,
                     nstart = 5, algorithm = "Lloyd")

# kmeans returns an object of class 'kmeans'

str(kmncluster)


# First create a copy of the ndvi layer
knr <- ndvi

# Now replace raster cell values with kmncluster$cluster
# array
knr[] <- kmncluster$cluster

# Alternative way to achieve the same result
values(knr) <- kmncluster$cluster
knr

par(mfrow = c(1, 2))
plot(ndvi, col = rev(terrain.colors(10)), main = "NDVI")
plot(knr, main = "Kmeans", col = viridis_pal(option = "D")(10))

png('rgb_kmeans.png', width = 10, height = 8, units = "in", res = 300)
par(mar = c(10.8, 5, 10.8, 2), mfrow = c(1, 2))
plotRGB(tayRGB, axes = TRUE, stretch = "lin", main = "RGB")
plot(knr, main = "Kmeans", yaxt = 'n', col = viridis_pal(option = "D")(10))
dev.off()


