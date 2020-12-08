

install.packages("spdep")
install.packages("shinyjs")

library(rgdal)
library(tmap)
library(spdep)
library(raster)
library(shinyjs)
#######################

income.tractsClean <- income.tracts[!is.na(income.tracts@data$Income), ]

#Define neighbourhoods with rook method
income.tracts.nb <- poly2nb(income.tractsClean, queen = FALSE) 
income.tracts.net <- nb2lines(income.tracts.nb, coords=coordinates(income.tractsClean))
crs(income.tracts.net) <- crs(income.tractsClean)

tm_shape(income.tractsClean) + tm_borders(col='lightgrey') + 
  tm_shape(income.tracts.net) + tm_lines(col='blue') +
  tm_compass() +
  tm_scale_bar() +
  tm_layout(title = "Neighbourhood Map with \nRook Method", title.position = c("RIGHT", "TOP"))

########################

income.tracts.lw <- nb2listw(income.tracts.nb, zero.policy = TRUE, style = "W")
print.listw(income.tracts.lw, zero.policy = TRUE)

########################

########################
mi <- moran.test(income.tractsClean$Income, income.tracts.lw, zero.policy = TRUE)
mi


moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(income.tracts.lw)


mI <- mi$estimate[[1]] #moran's I global value
eI <- mi$estimate[[2]] #Expected Moran's I
var <- mi$estimate[[3]] #variance

z <- (mI-eI)/sqrt(var)
z
########################  
moran.plot(income.tractsClean$Income, income.tracts.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Income", 
           ylab="Spatially Lagged Variable", quiet=NULL)
########################