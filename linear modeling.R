load("IST687_Cali_Data_clean.RData")  #loading  cleaned data set into R directory
View(IST687_Cali_Data_clean)  #You can view the data set in a new tab.

#We are doing linear modeling. In all the models Likelihood_Recommend_H is our independent variable and the other variables are our dependent variable from the data set.
model1 <- lm(Likelihood_Recommend_H ~ Overall_Sat_H,IST687_Cali_Data_clean)
summary(model1)

model2 <- lm(Likelihood_Recommend_H ~ Guest_Room_H, IST687_Cali_Data_clean)
summary(model2)

model3 <- lm(Likelihood_Recommend_H ~ Guest_Room_H + Tranquility_H, IST687_Cali_Data_clean)
summary(model3)

model4 <- lm(Likelihood_Recommend_H ~ Tranquility_H, IST687_Cali_Data_clean)
summary(model4)

model5 <- lm(Likelihood_Recommend_H ~ Condition_Hotel_H, IST687_Cali_Data_clean)
summary(model5)

model6 <- lm(Likelihood_Recommend_H ~ Guest_Room_H + Condition_Hotel_H, IST687_Cali_Data_clean)
summary(model6)

model7 <- lm(Likelihood_Recommend_H ~ Customer_SVC_H, IST687_Cali_Data_clean)
summary(model7)

model8 <- lm(Likelihood_Recommend_H ~ Guest_Room_H + Condition_Hotel_H + Customer_SVC_H, IST687_Cali_Data_clean)
summary(model8)

model9 <- lm(Likelihood_Recommend_H ~ Guest_Room_H + Customer_SVC_H, IST687_Cali_Data_clean)
summary(model9)

model10 <- lm(Likelihood_Recommend_H ~ Condition_Hotel_H + Customer_SVC_H, IST687_Cali_Data_clean)
summary(model10)

model11 <- lm(Likelihood_Recommend_H ~ Staff_Cared_H, IST687_Cali_Data_clean)
summary(model11)

model12 <- lm(Likelihood_Recommend_H ~ Internet_Sat_H, IST687_Cali_Data_clean)
summary(model12)

model13 <- lm(Likelihood_Recommend_H ~ Check_In_H, IST687_Cali_Data_clean)
summary(model13)

#changing the column name of our 11th column Food&Beverages as it has a '&' symbol in it.
colnames(IST687_Cali_Data_clean)[11] <- "FoodandBeverages_Overall_Experience_H"
View(IST687_Cali_Data_clean)

model14 <- lm(Likelihood_Recommend_H ~ FoodandBeverages_Overall_Experience_H, IST687_Cali_Data_clean)
summary(model14)


