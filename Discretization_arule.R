install.packages("arules")
install.packages("plyr")
install.packages("dplyr")
library(arules)
library(dplyr)
library(plyr)

my.file.name <- file.choose()
my.file.name

storydata = read.csv(file=my.file.name, header = TRUE, stringsAsFactors = FALSE)

nrow(storydata)
ncol(storydata)
View(storydata)

new_df <- storydata[2:12]

new_df
View(new_df)
str(new_df)

#Discretize age  and give labing 

new_df$age = cut(new_df$age,
                   breaks = c(0,12,19,35,52,65,Inf),
                   labels = c("child", "teen", "youth","middle", "old", "senior" ))

#Discretize income.
#find minimum 
#find maximum 
#difference between minimum and maximum divided by 3 slabs

min_income = min(new_df$income)
min_income
max_income = max(new_df$income)
width_income = (max_income - min_income)/3

#labelling or discretize

new_df$income = cut(new_df$income, breaks = c(min_income, max_income, width_income))
new_df$income


#convert numeric to nominal for children

new_df$children =  factor(new_df$children)


#encoding YES's and NOs to 

new_df$married = dplyr::recode(new_df$married, YES="married=YES", NO="married=NO")
new_df$car = dplyr::recode(new_df$car, YES= "car = YES", NO = "car=NO")
new_df$save_act = dplyr::recode(new_df$save_act, YES="save_act=YES", NO="save_act=NO")

new_df$current_act = dplyr::recode(new_df$current_act, YES= "current_act=YES", NO = "current_act=NO")
new_df$mortgage = dplyr::recode(new_df$mortgage, YES="mortgage=YES", NO= "mortgage = NO")


new_df$pep = dplyr::recode(new_df$pep, YES="pep=YES", NO="pep=NO")


myRules = apriori(new_df, parameter = list(supp = 0.004, conf = 0.7, maxlen = 3))
myRules

summary(myRules)

itemFrequencyPlot(new_df, top)

#visualizing the results
#top five rules by lift

inspect(sort(myRules, subset = rhs %pin% "pep=", by = 'lift')[1:10])

#set PEP as the right hand side of the rules, 
peprightRules = inspect( subset(myRules, subset = rhs %pin% "pep=" ,by = 'lift')[21:23])





