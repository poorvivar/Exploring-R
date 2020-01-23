#
#Author: Poorvi Varma
#Purpose: Expore Crime rates using R
#IST 719 
#

###########################################################
# Load the data
#scatter plot
crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.csv",sep=",", header=TRUE)
View(crime)

plot(crime$murder, crime$burglary)
crime2 <- crime[crime$state != "District of Columbia",]
crime2 <- crime2[crime2$state != "United States",]
crime2


scatter.smooth(crime2$murder, crime2$burglary,
               xlim=c(0,10), ylim=c(0, 1200))

#Create a Scatterplot Matrix
plot(crime2[,2:9])
pairs(crime2[,2:9], col="blue", panel=panel.smooth)

#####################################################################################
#Bubble plot
crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.tsv",header=TRUE, sep="\t")

symbols(crime$murder, crime$burglary, circles=crime$population)
radius <- sqrt( crime$population/ pi )

symbols(crime$murder, crime$burglary, circles=radius)

symbols(crime$murder, crime$burglary,
        circles=radius, inches=0.5, col="purple")
text(crime$murder, crime$burglary, crime$state, cex=0.5)

######################################################################################
#Distribution
birth <- read.csv("http://datasets.flowingdata.com/birth-rate.csv")
stem(birth$X2008)
hist(birth$X2008)

#######################################################################
#density plot

birth2008 <- birth$X2008[!is.na(birth$X2008)]
d2008 <- density(birth2008)
d2008frame <- data.frame(d2008$x, d2008$y)
write.table(d2008frame, "birthdensity.txt", sep="\t")
write.table(d2008frame, "birthdensity.txt", sep=",", row.names=FALSE)
    
 plot(d2008, type="n")
 polygon(d2008, col="#821122", border="#cccccc")
 
 #library(lattice)
 histogram(birth$X2008, breaks=10)
 lines(d2008)

 ###############################################################################
 # Load data
 tvs <-read.table("http://datasets.flowingdata.com/tv_sizes.txt",sep="\t", header=TRUE)
 # Filter outliers
 tvs <- tvs[tvs$size < 80, ]
 tvs <- tvs[tvs$size > 10, ]
 # Set breaks for histograms
 breaks = seq(10, 80, by=5)

 # Set the layout
 par(mfrow=c(1,3))
 # Draw histograms, one by one
 hist(tvs[tvs$year == 2009,]$size, breaks=breaks, angle = 90)
 hist(tvs[tvs$year == 2008,]$size, breaks=breaks)
 hist(tvs[tvs$year == 2007,]$size, breaks=breaks)
 hist(tvs[tvs$year == 2006,]$size, breaks=breaks)
 
######################################################################################
 my.file.name <- file.choose()
 my.file.name
 mydata <- read.csv(file=my.file.name, header = TRUE, stringsAsFactors = FALSE)
 View(mydata)
 
 mydata <- mydata[, 2:26]
 View(mydata)
 # Set the layout
 par(mfrow=c(3,2))
 
 barplot(table(mydata$work_interfere , mydata$Country=="United States"),legend = c("Often","Rarely","Never","Sometimes"), col = c("red", "blue", "yellow","green")
         ,space = .25 , beside = TRUE)
 
 barplot(table(mydata$work_interfere , mydata$Country=="United Kingdom"), col = c("red", "blue", "yellow","green")
         ,space = .25 , beside = TRUE)
 
 barplot(table(mydata$work_interfere , mydata$Country=="Germany"), col = c("red", "blue", "yellow","green")
         ,space = .25 , beside = TRUE)
 
 barplot(table(mydata$work_interfere , mydata$Country=="Canada"), col = c("red", "blue", "yellow","green")
         ,space = .25 , beside = TRUE)
 
 barplot(table(mydata$work_interfere , mydata$Country=="Netherlands"), col = c("red", "blue", "yellow","green")
         ,space = .25 , beside = TRUE)
 
 barplot(table(mydata$work_interfere , mydata$Country=="Australia"), col =c("red", "blue", "yellow","green")
         ,space = .25 , beside = TRUE)
 
 barplot(table(mydata$work_interfere , mydata$Country=="India"), col = c("red", "blue", "yellow","green")
         ,space = .25 , beside = TRUE)
 
 barplot(table(mydata$work_interfere , mydata$Country=="Belgium"), col = c("red", "blue", "yellow","green")
         ,space = .25 , beside = TRUE)
 
 
 
 
 
 