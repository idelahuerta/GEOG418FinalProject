#####
library("spatstat")
library("maptools")
library("sp")
library("plyr")
library("lubridate")
library("rgdal")
library("raster")
library("tmap")
library("gtable")
library("gridExtra")
library("grid")
#intersect the two datasets
pm2.5Points <- raster::intersect(pm2.5, income.tracts)
plot(income.tracts)
plot(pm2.5Points, add = TRUE)
#convert the crimes data type to factor
pm2.5Points@data$PM25 <- as.factor(pm2.5Points@data$PM25)
levels(pm2.5Points$PM25)
kma <- pm2.5Points
kma$x <- coordinates(kma)[,1]
kma$y <- coordinates(kma)[,2]
#check for and remove duplicated points
#first, finds zero distance among points to see if there are any duplicates
zd <- zerodist(kma)
zd
#if there are duplicates, remove them
kma <- remove.duplicates(kma)
#create an "extent" object which can be used to create the observation window for spatstat
kma.ext <- as.matrix(extent(kma)) 
#observation window
window <- as.owin(list(xrange = kma.ext[1,], yrange = kma.ext[2,]))
#create ppp oject from spatstat
kma.ppp <- ppp(x = kma$x, y = kma$y, window = window)

##K-FUNCTION 
#basic k-function
k.fun <- Kest(kma.ppp, correction = "Ripley")
plot(k.fun)

#use simulation to test the point pattern against CSR
k.fun.e <- envelope(kma.ppp, Kest, nsim = 99, correction = "Ripley")
plot(k.fun.e)

###KERNEL DENSITY ESTIMATION
#2D (gaussian) kernel, compare how bandwidth (sigma) selection influences the point density estimates
#since data are projected, sigma is represented in metres
#eps is the width and height of the pixels (1000m X 1000m)
#coerce to a SpatialGridDataFrame for plotting
kde.50 <- density(kma.ppp, sigma = 50, at = "pixels", eps = c(100, 100))
kde.SG <- as(kde.50, "SpatialGridDataFrame")
kde.250 <- density(kma.ppp, sigma = 250, at = "pixels", eps = c(100, 100))
kde.SG <- cbind(kde.SG, as(kde.250, "SpatialGridDataFrame"))

names(kde.SG) <- c("size50", "size250")
#plot
spplot(kde.SG)

#can see how the bandwidth selection influences the density estimates
summary(kde.SG)

#use cross-validation to get the bandwidth that minimizes MSE
bw.d <- bw.diggle(kma.ppp)
#plot the "optimal" bandwidth
plot(bw.d, ylim=c(-10, 10), main= "Sigma Cross-validation Analysis: PM 2.5")

#density using the cross-validation bandwidth
kde.bwo <- density(kma.ppp, sigma = bw.d, at = "pixels", eps = c(100, 100))
plot(kde.bwo)

##QUADRAT ANALYSIS
##First, determine the number of quadrats 
quads <- 10

qcount <- quadratcount(kma.ppp, nx = quads, ny = quads)

plot(kma.ppp, pch = "+", cex = 0.5)
plot(qcount, add = T, col = "red")

qcount.df <- as.data.frame(qcount)

##Second, count the number of quadrats with a distinct number of points.
qcount.df <- plyr::count(qcount.df,'Freq')

##Change the column names so that x=number of points and f=frequency of quadrats with x point.
colnames(qcount.df) <- c("x","f")

sum.f.x2 <- sum(qcount.df["f"]*(qcount.df["x"]^2))

M <- 100

sum.fx.2 <- sum((qcount.df["f"]*qcount.df["x"])^2)

N <- 5293

VAR <- (sum.f.x2 - (sum.fx.2)/M)/M-1

MEAN <- M/N

VMR <- VAR/MEAN


##Finally, perform the test statistic to test for the existence of a random spatial pattern.
chi.square = VMR*(M-1)
p = 1 - pchisq(chi.square, (M - 1))

##MAP Intro
map_TM <- tm_shape(VanCity) + #make the main shape
  tm_fill(col = "gray70") +  #fill polygons
  tm_add_legend(type = "symbol", shape = 22, col ="gray70", labels = "Neighbourhoods") +
  tm_compass() +
  tm_scale_bar() +
  tm_layout(title = "City of Vancouver", title.position = c("LEFT", "TOP"))
map_TM

#TABLE##
Crime = c("Theft Of Vehicle")
VAR <- round(VAR,1)
VMR <- round(VMR,1)
Pvalue <- p
data.for.table = data.frame(Crime, VAR, VMR, Pvalue)
table <- tableGrob(data.for.table, rows = c("")) #make a table "Graphical Object" (GrOb) 
t1Caption <- textGrob("Table 2: Measures of Variance for the Crime Theft Of Vehicle", gp = gpar(fontsize = 08))
padding <- unit(5, "mm")

table <- gtable_add_rows(table, 
                         heights = grobHeight(t1Caption) + padding, 
                         pos = 0)

table <- gtable_add_grob(table,
                         t1Caption, t = 1, l = 2, r = ncol(data.for.table) + 1)
grid.arrange(table, newpage = TRUE)

