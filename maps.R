#
#Author: Poorvi Varma
#Purpose: Maps Vizs
#
###########################################################
#rm clears memory
rm(list=ls())# ls show what is in the memory 
x <- c(8.11,8.21,8.11,8.33,8.34,8.11,7.91,7.79,8.68,9.82,10.33,8.66,7.62,7.5,7.64,6.32,5.75,0,0.11,1.2,0.96,2.72,3.01,3.93,4.58,4.52,4.68,4.47,4.64,4.95,5.36,6.27,8.11)
y <- c(7.78,6.78,6.35,5.23,4.34,1.87,1,0.54,0.77,1.15,1.02,0.21,0,0.23,0.75,1.52,2.41,2.54,3.05,3.94,4.71,4.73,4.66,4.8,5.15,5.66,5.85,5.99,6.43,6.58,7.07,7.72,7.78)
plot(x,y, type ="l")

library(maps)

map("state")

fname <- file.choose()
load(fname)
View(shootings)
table(shootings$State)

#gsub("^\\s+|\\s+$",""," poo r v i")#removing unwantd space in the start

shootings$State<-gsub("^\\s+|\\s+$","", shootings$State)
sort(shootings$State)

agg.dat <- aggregate(shootings$Total.Number.of.Victims
                     , list(shootings$State)
                     , sum)

colnames(agg.dat) <- c("state","victims")

num.cols <-255
my.col.vec <- rev(heat.colors(num.cols))
pie(rep(1,num.cols), col=my.col.vec)

my.col.vec[c(1,1,1,1)]

tmp <-agg.dat$victims/max(agg.dat$victims)
agg.dat$index <- 1+round(((num.cols-1)*tmp),0)
agg.dat$color <- my.col.vec[agg.dat$index]


state.order <- match.map(database = "state"
                         , regions = agg.dat$state
                         ,exact = FALSE
                         , warn = TRUE )


map("state", col=agg.dat$color[state.order]
    ,fill=TRUE, border ="tan")

###################################################
map("county", "new york")

us.cities
my.col <- rep(rgb(1,.6,.2,.7), dim(us.cities)[1])
my.col[us.cities$capital >0] <- rgb(.2,.6,1,.9)


library(plotrix)
map("state")
points(us.cities$long, us.cities$lat
       , col =my.col
       ,pch=16
       , cex= rescale(us.cities$pop, c(.5,7)))

#####################################################

library(rworldmap)
fname <- file.choose()

countries <- read.delim(fname, quote = "\""
                        ,header = TRUE
                        ,sep =";"
                        , stringsAsFactors = FALSE)


colnames(countries)
countries$Life.expectancy
plot(countries$Life.expectancy)


missing.data <-which(countries$Life.expectancy < 1)
#comment out below after running once, otherwise it will keep deleting records
#countries <- countries[-missing.data,]
#rm(missing.data)

num.cats <- 10

iso3.codes <- tapply(countries$Country..en.
                     , 1:dim(countries)[1]
                     ,rwmGetISO3)


df <- data.frame(country = iso3.codes
                 , lables = countries$Country..en.
                 , life = countries$Life.expectancy)


df.map <- joinCountryData2Map(df, joinCode = "ISO3"
                             , nameJoinColumn = "country" )

mapCountryData(df.map, nameColumnToPlot = "life"
               ,numCats = num.cats
               , catMethod = "fixedWidth"
               ,colourPalette = heat.colors(num.cats)
               ,oceanCol = "royalblue4"
               , borderCol = "white"
               , missingCountryCol = "peachpuff4"
               ,mapTitle ="Life Expectancy"
               )


#########################################

#WordCloud
library(wordcloud)

fname <- file.choose()
tweets <- read.csv(fname, header = TRUE
                   , stringsAsFactors = FALSE
                   , quote = "\"")
dim(tweets)
tweet.tags <- tweets$hashtags[tweets$hashtags != ""]

all.tags  <- unlist(strsplit(tweet.tags, "\\|"))
tag.tab  <- table(all.tags)
tag.words <- names(tag.tab)
tag.freq <- as.numeric(tag.tab)
length(tag.freq)
plot(tag.freq)


wordcloud(tag.words, tag.freq, scale = c(4,.5))

par(mfrow=c(2,2))
plot(tag.freq)
plot(tag.freq^2)
plot(tag.freq^(1/2))
plot(log10(tag.freq))


size <- tag.freq^(1/2)
par(mfrow = c(1,1))
plot(size)
summary(size)


wordcloud(tag.words, size, scale= c(5,.70))

par(mar=c(1,1,1,1))
wordcloud(tag.words, size, scale= c(5,.75)
          , min.freq = 1
          , random.order = FALSE
          ,rot.per = 0)

warnings()

myPalFun <- colorRampPalette(
  c("gold", "red", "green")
)

tag.cols.vec <- myPalFun(max(size)+1)
tag.col <- tag.cols.vec[round(size)+1]

par(bg="black")
wordcloud(tag.words, size, scale=c(5,.75)
          , min.freq = 3, random.order = FALSE
          , rot.per = 0.7
          ,random.color = FALSE
          ,colors = tag.col
          ,ordered.colors = TRUE
          )























