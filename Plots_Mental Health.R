#
#Author: Poorvi Varma
#Purpose:Exploring Mental Health issues using R
#

my.file.name <- file.choose()
my.file.name
mydata <- read.csv(file=my.file.name, header = TRUE, stringsAsFactors = FALSE)
View(mydata)

mydata <- mydata[, 2:26]
View(mydata)

newdf  <- na.omit(mydata)
View(newdf)

x <-barplot(table(mydata$Country), col = "yellow", main = "Mental health issues reported in different Country from a 2014 survey",
            ylab="Frequency of cases reported", xlab="Country",beside = TRUE, xaxt="n")
labs <- paste(names(table(mydata$Country)))
text(cex=0.60, x=x-.25, y=-80.25, labs, xpd=TRUE, srt=90)

y <-barplot(table(mydata$family_history), main = "Does family history of Mental health issues effect a person?",
            col = "orange",
            border = "blue",
            ylab = "Frequency of cases reported" 
            ,beside = TRUE)

z <-pie(table(mydata$work_interfere), 
        main = "If you have a mental health condition, do you feel that it interferes with your work?"
        ,col = c("blue1", "seagreen1","yellow3","maroon1"))

barplot(table(mydata$tech_company , mydata$state),legend = c("No","Yes"), col = c("red", "white")
        , main = "Survey capture of Mental health Issue in different states of USA related to working in a tech company"
        ,xlab = "States" , ylab = "Frequency", beside = TRUE)




