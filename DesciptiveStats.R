#######

#Install Libraries
install.packages("rgdal")
install.packages("lubridate")
install.packages("e1071")
install.packages("gtable")
install.packages("gridExtra")
install.packages("grid")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("bcmaps")
install.packages('bcmapsdata', repos='https://bcgov.github.io/drat/')
install.packages("raster")
install.packages("maps")
install.packages("tmap")
install.packages("rgeos")

#####
#Load Libraries
library("rgdal")
library("lubridate")
library("e1071")
library("gtable")
library("gridExtra")
library("grid")
library("ggplot2")
library("dplyr")
library("bcmaps")
library('bcmapsdata', repos='https://bcgov.github.io/drat/')
library("raster")
library("maps")
library("tmap")
library("rgeos")

# Descriptive Stats
meanIncome <- mean(income$Income, na.rm = TRUE)
meanPM25 <- mean(pm2.5$PM25, na.rm = TRUE)
sdIncome <- sd(income$Income, na.rm = TRUE)
sdPM25 <- sd(pm2.5$PM25, na.rm = TRUE)
modeIncome <- as.numeric(names(sort(table(income$Income), decreasing = TRUE))[1])
modePM25 <- as.numeric(names(sort(table(pm2.5$PM25), decreasing = TRUE))[1])
medIncome <- median(income$Income, na.rm = TRUE)
medPM25 <- median(pm2.5$PM25, na.rm = TRUE)
skewIncome <- skewness(income$Income, na.rm = TRUE)[1]
skewPM25 <- skewness(pm2.5$PM25, na.rm = TRUE)[1]
kurtIncome <- kurtosis(income$Income, na.rm = TRUE)[1]
kurtPM25 <- kurtosis(pm2.5$PM25, na.rm = TRUE)[1]
CoVIncome <- (sdIncome / meanIncome) * 100
CoVPM25 <- (sdPM25 / meanPM25) * 100
normIncome_PVAL <- shapiro.test(income$Income)$p.value
normPM25_PVAL <- shapiro.test(pm2.5$PM25)$p.value
# Tables
Samples = c("Income", "PM 2.5") #Create an object for the labels
Mean = c(meanIncome, meanPM25) #Create an object for the means
SD = c(sdIncome, sdPM25) #Create an object for the standard deviations
Median = c(medIncome, medPM25) #Create an object for the medians
Mode = c(modeIncome, modePM25) #Create an object for the modes
Skewness = c(skewIncome, skewPM25) #Create an object for the skewness
Kurtosis = c(kurtIncome, kurtPM25) #Create an object for the kurtosis
CoV = c(CoVIncome, CoVPM25) #Create an object for the CoV
Normality = c(format(normIncome_PVAL,scientific = TRUE, digits = 3), format(normPM25_PVAL,scientific = TRUE, digits = 3)) #Create an object for the normality PVALUE

Mean
Mean <- round(Mean,3)
SD <- round(SD,3)
SD
Median
Mode <- round(Mode,3)
Mode
Skewness
Skewness <- round(Skewness,3)
Skewness
Kurtosis <- round(Kurtosis,3)
Kurtosis
CoV
CoV<- round(CoV,3)
Normality

data.for.table1 = data.frame(Samples, Mean, Median, Mode, SD)
data.for.table2 = data.frame(Samples, Skewness, Kurtosis, CoV, Normality)

#Make tables
table1 <- tableGrob(data.for.table1, rows = c("","")) #make a table "Graphical Object" (GrOb) 
t1Caption <- textGrob("Table 1: Descriptive Statistics for Income and PM 2.5", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table1 <- gtable_add_rows(table1, 
                          heights = grobHeight(t1Caption) + padding, 
                          pos = 0)

table1 <- gtable_add_grob(table1,
                          t1Caption, t = 1, l = 2, r = ncol(data.for.table1) + 1)


table2 <- tableGrob(data.for.table2, rows = c("",""))
t2Caption <- textGrob("Table 2: Descriptive Statistics for Income and PM 2.5", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table2 <- gtable_add_rows(table2, 
                          heights = grobHeight(t2Caption) + padding, 
                          pos = 0)

table2 <- gtable_add_grob(table2,
                          t2Caption, t = 1, l = 2, r = ncol(data.for.table2) + 1)




grid.arrange(table1, newpage = TRUE)
grid.arrange(table2, newpage = TRUE)
