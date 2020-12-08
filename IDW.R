#Spatial Interpolation with IDW

P.idw <- gstat::idw(PM25 ~ 1, pm2.5, newdata=grd, idp=5)
r       <- raster(P.idw)
r.m     <- mask(r, income.tracts)

tm_shape(r.m) + 
  tm_raster(n=10, palette = "-RdBu", title="PM 2.5 (ppm)") + 
  tm_shape(pm2.5) + tm_dots(size=0.001) +
  tm_legend(legend.outside=TRUE)

#################################################
# Leave-one-out validation routine
IDW.out <- vector(length = length(pm2.5))
for (i in 1:length(pm2.5)) {
  IDW.out[i] <- idw(PM25 ~ 1, pm2.5[-i,], pm2.5[i,], idp=5)$var1.pred
}

# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ pm2.5$PM25, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ pm2.5$PM25), col="red", lw=2,lty=2)
abline(0,1)
par(OP)
sqrt( sum((IDW.out - pm2.5$PM25)^2) / length(pm2.5)) #units of pm2.5

#################################################
# Implementation of a jackknife technique to estimate a confidence interval at each unsampled point.
# Create the interpolated surface
img <- gstat::idw(PM25~1, pm2.5, newdata=grd, idp=5)
n   <- length(pm2.5)
Zi  <- matrix(nrow = length(img$var1.pred), ncol = n)

# Remove a point then interpolate (do this n times for each point)
st <- stack()
for (i in 1:n){
  Z1 <- gstat::idw(PM25~1, pm2.5[-i,], newdata=grd, idp=5)
  st <- addLayer(st,raster(Z1,layer=1))
  # Calculated pseudo-value Z at j
  Zi[,i] <- n * img$var1.pred - (n-1) * Z1$var1.pred
}

# Jackknife estimator of parameter Z at location j
Zj <- as.matrix(apply(Zi, 1, sum, na.rm=T) / n )

# Compute (Zi* - Zj)^2
c1 <- apply(Zi,2,'-',Zj)            # Compute the difference
c1 <- apply(c1^2, 1, sum, na.rm=T ) # Sum the square of the difference

# Compute the confidence interval
CI <- sqrt( 1/(n*(n-1)) * c1)

# Create (CI / interpolated value) raster
img.sig   <- img
img.sig$v <- CI /img$var1.pred 


# Plot the map
tm_shape(r.m) + tm_raster(n=10,title="95% CI (ppm)") +
  tm_shape(income.tracts) + tm_dots(size=0.001) +
  tm_legend(legend.outside=TRUE)


