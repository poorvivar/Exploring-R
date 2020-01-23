######################
####   library #######
######################
library("kernlab")
library("ggplot2")
library("e1071")
library("grid")
library("gridExtra")
library("sqldf")

load("IST687_Cali_Data_clean.RData")
Final_df_svm<-IST687_Cali_Data_clean
#a<-Final_df_svm
#Final_df_svm1<-data.frame(a$NPS_Type,a$Guest_Room_H,a$Tranquility_H,a$Condition_Hotel_H,a$Customer_SVC_H,a$Staff_Cared_H,a$Check_In_H)
Final_df_svm$Tranquility_H<-ifelse(Final_df_svm$Tranquility_H>8,"high",ifelse((Final_df_svm$Tranquility_H>6),"medium","low"))
Final_df_svm$Guest_Room_H<-ifelse(Final_df_svm$Guest_Room_H>8,"high",ifelse(Final_df_svm$Guest_Room_H>6,"medium","low"))
Final_df_svm$Condition_Hotel_H<-ifelse(Final_df_svm$Condition_Hotel_H>8,"high",ifelse(Final_df_svm$Condition_Hotel_H>6,"medium","low"))
Final_df_svm$Customer_SVC_H<-ifelse(Final_df_svm$Customer_SVC_H>8,"high",ifelse(Final_df_svm$Customer_SVC_H>6,"medium","low"))
Final_df_svm$Staff_Cared_H<-ifelse(Final_df_svm$Staff_Cared_H>8,"high",ifelse(Final_df_svm$Staff_Cared_H>6,"medium","low"))
Final_df_svm$Check_In_H<-ifelse(Final_df_svm$Check_In_H>8,"high",ifelse(Final_df_svm$Check_In_H>6,"medium","low"))
#####creating the cut points for train and test data
#####################################################
nrows<-nrow(Final_df_svm)
random.index<-sample(1:nrows,replace=FALSE)
cutpoint_2_3<-floor(nrows*2/3)
df_train<-Final_df_svm[random.index[1:cutpoint_2_3],]
df_test <-Final_df_svm[random.index[(1+cutpoint_2_3):nrows],]

###########################################################
####################  Tranquility_H  ######################
t_train<-data.frame(df_train$NPS_Type,df_train$Tranquility_H)
t_train<-na.omit(t_train)
colnames(t_train)<-c("NPS_Type","Tranquility_H")
t_test<-data.frame(df_test$NPS_Type,df_test$Tranquility_H)
t_test<-na.omit(t_test)
colnames(t_test)<-c("NPS_Type","Tranquility_H")

#ksvm model with C=10, cross=3
ksvm_model1<-ksvm(as.factor(NPS_Type)~ Tranquility_H,data=t_train,kernel = "rbfdot",
                  kpar = "automatic",C=10,cross=3,prob.model=TRUE)

#running model on test data
predict_model1<-predict(ksvm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
t<-c("ksvm","Tranquility_H",perct_predict_model1,"10","3")
#######################################
#ksvm model with C=5, cross=3
ksvm_model1<-ksvm(as.factor(NPS_Type)~ Tranquility_H,data=t_train,kernel = "rbfdot",
                  kpar = "automatic",C=5,cross=3,prob.model=TRUE)

#running model on test data
predict_model1<-predict(ksvm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
tt<-c("ksvm","Tranquility_H",perct_predict_model1,"5","3")
###################################################
#svm model 
svm_model1<-svm(as.factor(NPS_Type)~ Tranquility_H,data=t_train)

#running model on test data
predict_model1<-predict(svm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
ttt<-c("svm","Tranquility_H",perct_predict_model1)

#########################################################
################## Customer_SVC_H #######################

t_train<-data.frame(df_train$NPS_Type,df_train$Customer_SVC_H)
t_train<-na.omit(t_train)
colnames(t_train)<-c("NPS_Type","Customer_SVC_H")
t_test<-data.frame(df_test$NPS_Type,df_test$Customer_SVC_H)
t_test<-na.omit(t_test)
colnames(t_test)<-c("NPS_Type","Customer_SVC_H")

#ksvm model with c=10,cross=3
ksvm_model1<-ksvm(as.factor(NPS_Type)~ Customer_SVC_H,data=t_train,kernel = "rbfdot",
                  kpar = "automatic",C=10,cross=3,prob.model=TRUE)

#running model on test data
predict_model1<-predict(ksvm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
t1<-c("ksvm","Customer_SVC_H",perct_predict_model1,"10","3")
t<-rbind(t,t1)

############################
#ksvm model with c=5,cross=3
ksvm_model1<-ksvm(as.factor(NPS_Type)~ Customer_SVC_H,data=t_train,kernel = "rbfdot",
                  kpar = "automatic",C=5,cross=3,prob.model=TRUE)

#running model on test data
predict_model1<-predict(ksvm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
tt1<-c("ksvm","Customer_SVC_H",perct_predict_model1,"5","3")
tt<-rbind(tt,tt1)
#################################
#svm
svm_model1<-svm(as.factor(NPS_Type)~ Customer_SVC_H,data=t_train)

#running model on test data
predict_model1<-predict(svm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
ttt1<-c("svm","Customer_SVC_H",perct_predict_model1)
ttt<-rbind(ttt,ttt1)
#########################################################
################## Staff_Cared_H ########################

t_train<-data.frame(df_train$NPS_Type,df_train$Staff_Cared_H)
t_train<-na.omit(t_train)
colnames(t_train)<-c("NPS_Type","Staff_Cared_H")
t_test<-data.frame(df_test$NPS_Type,df_test$Staff_Cared_H)
t_test<-na.omit(t_test)
colnames(t_test)<-c("NPS_Type","Staff_Cared_H")

#ksvm model with C=10,cross=3
ksvm_model1<-ksvm(as.factor(NPS_Type)~ Staff_Cared_H,data=t_train,kernel = "rbfdot",
                  kpar = "automatic",C=10,cross=3,prob.model=TRUE)

#running model on test data
predict_model1<-predict(ksvm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
t1<-c("ksvm","Staff_Cared_H",perct_predict_model1,"10","3")
t<-rbind(t,t1)

#################
#ksvm model with C=5,cross=3
ksvm_model1<-ksvm(as.factor(NPS_Type)~ Staff_Cared_H,data=t_train,kernel = "rbfdot",
                  kpar = "automatic",C=5,cross=3,prob.model=TRUE)

#running model on test data
predict_model1<-predict(ksvm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
tt1<-c("ksvm","Staff_Cared_H",perct_predict_model1,"5","3")
tt<-rbind(tt,tt1)
##################SVM
#ksvm model with C=10,cross=3
svm_model1<-svm(as.factor(NPS_Type)~ Staff_Cared_H,data=t_train)

#running model on test data
predict_model1<-predict(svm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
ttt1<-c("ksvm","Staff_Cared_H",perct_predict_model1)
ttt<-rbind(ttt,ttt1)
#################################################
##################### Check_In_H ################
t_train<-data.frame(df_train$NPS_Type,df_train$Check_In_H)
t_train<-na.omit(t_train)
colnames(t_train)<-c("NPS_Type","Check_In_H")
t_test<-data.frame(df_test$NPS_Type,df_test$Check_In_H)
t_test<-na.omit(t_test)
colnames(t_test)<-c("NPS_Type","Check_In_H")

#ksvm model with c=10,cross=3
ksvm_model1<-ksvm(as.factor(NPS_Type)~ Check_In_H,data=t_train,kernel = "rbfdot",
                  kpar = "automatic",C=10,cross=3,prob.model=TRUE)

#running model on test data
predict_model1<-predict(ksvm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
t1<-c("ksvm","Check_In_H",perct_predict_model1,"10","3")
t<-rbind(t,t1)


##############################
#ksvm model with c=5,cross=3
ksvm_model1<-ksvm(as.factor(NPS_Type)~ Check_In_H,data=t_train,kernel = "rbfdot",
                  kpar = "automatic",C=5,cross=3,prob.model=TRUE)

#running model on test data
predict_model1<-predict(ksvm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
tt1<-c("ksvm","Check_In_H",perct_predict_model1,"5","3")
tt<-rbind(tt,tt1)

##############################SVM
svm_model1<-svm(as.factor(NPS_Type)~ Check_In_H,data=t_train)

#running model on test data
predict_model1<-predict(svm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
ttt1<-c("svm","Check_In_H",perct_predict_model1)
ttt<-rbind(ttt,ttt1)

#################################################
##################### Guest_Room_H ################
t_train<-data.frame(df_train$NPS_Type,df_train$Guest_Room_H)
t_train<-na.omit(t_train)
colnames(t_train)<-c("NPS_Type","Guest_Room_H")
t_test<-data.frame(df_test$NPS_Type,df_test$Guest_Room_H)
t_test<-na.omit(t_test)
colnames(t_test)<-c("NPS_Type","Guest_Room_H")

#ksvm model with c=10,cross=3
ksvm_model1<-ksvm(as.factor(NPS_Type)~ Guest_Room_H,data=t_train,kernel = "rbfdot",
                  kpar = "automatic",C=10,cross=3,prob.model=TRUE)

#running model on test data
predict_model1<-predict(ksvm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
t1<-c("ksvm","Guest_Room_H",perct_predict_model1,"10","3")
t<-rbind(t,t1)

#ksvm model with c=5,cross=3
ksvm_model1<-ksvm(as.factor(NPS_Type)~ Guest_Room_H,data=t_train,kernel = "rbfdot",
                  kpar = "automatic",C=5,cross=3,prob.model=TRUE)

#running model on test data
predict_model1<-predict(ksvm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
tt1<-c("ksvm","Guest_Room_H",perct_predict_model1,"5","3")
tt<-rbind(tt,tt1)

####################svm
svm_model1<-svm(as.factor(NPS_Type)~ Guest_Room_H,data=t_train)

#running model on test data
predict_model1<-predict(svm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
ttt1<-c("svm","Guest_Room_H",perct_predict_model1,"5","3")
ttt<-rbind(ttt,ttt1)

######################################################
################Condition_Hotel_H#####################

t_train<-data.frame(df_train$NPS_Type,df_train$Condition_Hotel_H)
t_train<-na.omit(t_train)
colnames(t_train)<-c("NPS_Type","Condition_Hotel_H")
t_test<-data.frame(df_test$NPS_Type,df_test$Condition_Hotel_H)
t_test<-na.omit(t_test)
colnames(t_test)<-c("NPS_Type","Condition_Hotel_H")

#ksvm model with c=10,cross=3
ksvm_model1<-ksvm(as.factor(NPS_Type)~ Condition_Hotel_H,data=t_train,kernel = "rbfdot",
                  kpar = "automatic",C=10,cross=3,prob.model=TRUE)

#running model on test data
predict_model1<-predict(ksvm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
t1<-c("ksvm","Condition_Hotel_H",perct_predict_model1,"10","3")
t<-rbind(t,t1)

###########################
#ksvm model with c=5,cross=3
ksvm_model1<-ksvm(as.factor(NPS_Type)~ Condition_Hotel_H,data=t_train,kernel = "rbfdot",
                  kpar = "automatic",C=5,cross=3,prob.model=TRUE)

#running model on test data
predict_model1<-predict(ksvm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
tt1<-c("ksvm","Condition_Hotel_H",perct_predict_model1,"5","3")
tt<-rbind(tt,tt1)

###########################
#svm model 
svm_model1<-svm(as.factor(NPS_Type)~ Condition_Hotel_H,data=t_train)

#running model on test data
predict_model1<-predict(svm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
ttt1<-c("svm","Condition_Hotel_H",perct_predict_model1)
ttt<-rbind(ttt,ttt1)

######################################################
################Fitness Center_PL#####################

t_train<-data.frame(df_train$NPS_Type,df_train$"Fitness Center_PL")
colnames(t_train)<-c("NPS_Type","Fitness_Center_PL")
t_train<-t_train[which(t_train$Fitness_Center_PL=="Y" | t_train$Fitness_Center_PL=="N"),]
t_test<-data.frame(df_test$NPS_Type,df_test$"Fitness Center_PL")
colnames(t_test)<-c("NPS_Type","Fitness_Center_PL")
t_test<-t_test[which(t_test$Fitness_Center_PL=="Y" | t_test$Fitness_Center_PL=="N"),]

#ksvm model with c=10,cross=3
ksvm_model1<-ksvm(as.factor(NPS_Type)~ Fitness_Center_PL,data=t_train,kernel = "rbfdot",
                  kpar = "automatic",C=10,cross=3,prob.model=TRUE)

#running model on test data
predict_model1<-predict(ksvm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
t1<-c("ksvm","Fitness_Center_PL",perct_predict_model1,"10","3")
t<-rbind(t,t1)

###########################
#ksvm model with c=5,cross=3
ksvm_model1<-ksvm(as.factor(NPS_Type)~ Fitness_Center_PL,data=t_train,kernel = "rbfdot",
                  kpar = "automatic",C=5,cross=3,prob.model=TRUE)

#running model on test data
predict_model1<-predict(ksvm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
tt1<-c("ksvm","Fitness_Center_PL",perct_predict_model1,"5","3")
tt<-rbind(tt,tt1)

###########################
#svm model 
svm_model1<-svm(as.factor(NPS_Type)~ Fitness_Center_PL,data=t_train)

#running model on test data
predict_model1<-predict(svm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
ttt1<-c("svm","Fitness_Center_PL",perct_predict_model1)
ttt<-rbind(ttt,ttt1)

#######################################################
################# with the colomns below #############
##Guest_Room_H=9                                                                            
##Tranquility_H=9                                                                           
#Condition_Hotel_H=9                                                                        
##Customer_SVC_H=9                                                                           
##Staff_Cared_H=9                                                                           
##Check_In_H=9

t_train<-data.frame(df_train$NPS_Type,df_train$Guest_Room_H,df_train$Tranquility_H,
                    df_train$Condition_Hotel_H,df_train$Customer_SVC_H,df_train$Staff_Cared_H,
                    df_train$Check_In_H,df_train$`Fitness Center_PL`)
t_train<-na.omit(t_train)
colnames(t_train)<-c("NPS_Type","Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H",
                     "Staff_Cared_H","Check_In_H","Fitness_Center_PL")
t_test<-data.frame(df_test$NPS_Type,df_test$Guest_Room_H,df_test$Tranquility_H,
                   df_test$Condition_Hotel_H,df_test$Customer_SVC_H,df_test$Staff_Cared_H,
                   df_test$Check_In_H,df_test$`Fitness Center_PL`)
t_test<-na.omit(t_test)
colnames(t_test)<-c("NPS_Type","Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H",
                    "Staff_Cared_H","Check_In_H","Fitness_Center_PL")

#ksvm model with c=10,cross=3
ksvm_model1<-ksvm(as.factor(NPS_Type)~ .,data=t_train,kernel = "rbfdot",
                  kpar = "automatic",C=10,cross=3,prob.model=TRUE)

#running model on test data
predict_model1<-predict(ksvm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
t1<-c("ksvm","all 7",perct_predict_model1,"10","3")
t<-rbind(t,t1)

####confusion matrix with all 6 parameters for cross value 3 and C=10
table(predict_model1,t_test$NPS_Type)

###############################
#ksvm model with c=5,cross=3
ksvm_model1<-ksvm(as.factor(NPS_Type)~ .,data=t_train,kernel = "rbfdot",
                  kpar = "automatic",C=5,cross=3,prob.model=TRUE)

#running model on test data
predict_model1<-predict(ksvm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
tt1<-c("ksvm","all 7",perct_predict_model1,"5","3")
tt<-rbind(tt,tt1)

############################SVM

svm_model1<-svm(as.factor(NPS_Type)~ .,data=t_train)

#running model on test data
predict_model1<-predict(ksvm_model1,t_test)
comp_model1<-t_test
comp_model1$Predict_NPS_Type<-predict_model1
comp_model1$goodPredict<-ifelse(comp_model1$Predict_NPS_Type == comp_model1$NPS_Type,"correct","wrong")
perct_predict_model1<-floor((nrow(comp_model1[which(comp_model1$goodPredict == "correct"),])/nrow(comp_model1))*100)
ttt1<-c("svm","all 7",perct_predict_model1)
ttt<-rbind(ttt,ttt1)

###visualing the prediction perctenage for ksvm
t_temp<-as.data.frame(t)
colnames(t_temp)<-c("model_used","Parameter","Correct_Predict_Pect","C Value","Cross Value")

t_temp$Correct_Predict_Pect <- as.numeric(as.character(t_temp$Correct_Predict_Pect))

plot1<-ggplot(data = t_temp, aes(x=reorder(Parameter, Correct_Predict_Pect),y=Correct_Predict_Pect)) + 
  geom_bar(aes(fill=Correct_Predict_Pect),stat = "identity") +
  ylim(0,100) +
  theme(axis.text.x=element_text(angle=45, hjust = 1))+
  scale_fill_gradient(low="yellow", high="darkgreen")+
  labs(x="",y="",fill = "Prediction %",title="ksvm Model")

plot1

###visualing the prediction perctenage for ksvm
tt_temp<-as.data.frame(tt)
colnames(tt_temp)<-c("model_used","Parameter","Correct_Predict_Pect","C Value","Cross Value")

tt_temp$Correct_Predict_Pect <- as.numeric(as.character(tt_temp$Correct_Predict_Pect))


###visualing the prediction perctenage for svm
ttt_temp<-as.data.frame(ttt)
colnames(ttt_temp)<-c("model_used","Parameter","Correct_Predict_Pect")

ttt_temp$Correct_Predict_Pect <- as.numeric(as.character(ttt_temp$Correct_Predict_Pect))

plot2<-ggplot(data = ttt_temp, aes(x=reorder(Parameter, Correct_Predict_Pect),y=Correct_Predict_Pect)) + 
  geom_bar(aes(fill=Correct_Predict_Pect),stat = "identity") +
  ylim(0,100) +
  theme(axis.text.x=element_text(angle=45, hjust = 1))+
  scale_fill_gradient(low="yellow", high="darkgreen")+
  labs(x="",y="",fill = "Prediction %",title="svm Model")

plot2

##plotting the two graphs together
grid.arrange(plot1,plot2)
