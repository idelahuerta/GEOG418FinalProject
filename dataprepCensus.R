#Libraries
install.packages("spgwr")
library("spgwr")
library("spatstat")
library("tmap")
library("gstat")
library("sf")
library("raster")
library("rgdal")
library("e1071")
library("spdep")
library("gtable")


#Set working directory
dir <- "/Users/Ivan/Documents/UVIC/T5/GEOG 418/Project"
setwd(dir)

#Reading in particulate matter dataset
#Read in PM2.5 data:
pm2.5 <- readOGR(dsn = "/Users/Ivan/Documents/UVIC/T5/GEOG 418/Project", layer = "Pm25Sample")
pm2.5 <- spTransform(pm2.5, CRS("+init=epsg:26910"))
#Select only ID and Income columns:
colnames(pm2.5) <- c("DAUID", "PM25") 

#Reading in dissemination tract and income data
#Read in census income data:
income <- read.csv("Income.csv")  
#Select only ID and Income columns:
colnames(income) <- c("DAUID", "Income") 
#Read in dissemination tract shapefile:
census.tracts <- readOGR(dsn = "/Users/Ivan/Documents/UVIC/T5/GEOG 418/Project", layer = "BC_DA")
#Merge income and dissemination data:
income.tracts <- merge(census.tracts,income, by = "DAUID") 
#Determine the number of columns in the dataframe:
nrow(income.tracts)
#Remove NA values:
income.tracts <- income.tracts[!is.na(income.tracts$Income),]
#Reproject the data:
income.tracts <- spTransform(income.tracts, CRS("+init=epsg:26910"))

#Create choropleth map of income:
map_Income <- tm_shape(income.tracts) +
  tm_polygons(col = "Income",
              title = "Median Income",
              style = "jenks",
              palette = "Greens", n = 10) +
  tm_legend(legend.position = c("LEFT", "BOTTOM"))

map_Income

map_TM <- tm_shape(income.tracts) + #make the main shape
  tm_fill(col = "gray70") +  #fill polygons
  tm_compass() +
  tm_scale_bar() +
  tm_layout(title = "Metro Vancouver", title.position = c("LEFT", "TOP"))
map_TM

tmaptools::palette_explorer()

#Create a grid called grd to use in your interpolation
# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(pm2.5, "regular", n=5000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
# Create SpatialPixel object:
gridded(grd)     <- TRUE  
# Create SpatialGrid object:
fullgrid(grd)    <- TRUE  
#Reproject the grid:
proj4string(grd) <- proj4string(income.tracts)



