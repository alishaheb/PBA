
rm(list=ls())
cat("\014")
set.seed(1234)
print(paste("WORKING DIRECTORY: ",getwd()))
coursework<-read.csv("Hp treatment training.csv")
print(coursework)
coursework_test<-read.csv("Testing_set_advance (1).csv")
print(coursework_test)

##Coursework_final<-merge(coursework,coursework_test,by="ID_Patient_Care_Situation", "Diagnosed_Condition","Patient_ID" ,"Treated_with_drugs","Patient_Age","Patient_Body_Mass_Index" ,"Patient_Smoker","Patient_Rural_Urban","Patient_mental_condition","A" ,"B","C","D","E","F" ,"Number_of_prev_cond" )
##There are total 22 variables in Hp training dataset and 17 in Test dataset, so I have decided to proceed
##with adding one variable ie Survived_1_year in our testing dataset and will remove variables (X,X.1,X.2,X.3) from training datasets 

##For adding one variable ie Survived_1_year in our testing dataset
coursework_test$Survived_1_year<-sample(c(0:1),nrow(coursework_test),replace=TRUE)
print(coursework_test$Survived_1_year)

##For Deleting variables(("X","X.1","X.2","X.3")) from the training datasets these are NULL variables so if required we can add them later in our final data set.
drop <- c("X","X.1","X.2","X.3")
coursework=coursework[,!(names(coursework)%in% drop)]
names(coursework)
##For merging both training and testing dataset
Coursework_final<-rbind(coursework,coursework_test)

## Code for shuffling the final dataset
final_coursework_data<-Coursework_final[sample(1:nrow(Coursework_final)), ] 

## removing duplicate rows 

final_coursework_data1<-unique(final_coursework_data);



##unique(final_coursework_data1$Treated_with_drugs)
##unique(final_coursework_data1$Diagnosed_Condition)
##names(final_coursework_data1)
##unique(final_coursework_data1$Patient_mental_condition)
##unique(final_coursework_data1$Patient_Rural_Urban)

final_coursework_data1$Patient_Smoker[final_coursework_data1$Patient_Smoker== "NO "]<-"NO"
final_coursework_data1$Patient_Smoker[final_coursework_data1$Patient_Smoker==  "YES "]<-"YES"
final_coursework_data1$Patient_Smoker[final_coursework_data1$Patient_Smoker== "YESS" ]<-"YES"
final_coursework_data1$Patient_Smoker[final_coursework_data1$Patient_Smoker=="Yes"]<-"YES"

final_coursework_data1$Patient_Smoker[final_coursework_data1$Patient_Smoker=="Cannot say" ]<-"CANNOT SAY";
final_coursework_data1$Patient_Smoker[final_coursework_data1$Patient_Smoker=="Cannot say "]<-"CANNOT SAY";
final_coursework_data1$Patient_Smoker[final_coursework_data1$Patient_Smoker=="CANNOT SAY"] <-"CANNOT SAY";
final_coursework_data1$Patient_Smoker[ final_coursework_data1$Patient_Smoker=="CANNOT SAY "]<-"CANNOT SAY";



spec = c(train = .6, test = .3, validate = .1)

g = sample(cut(
  seq(nrow(final_coursework_data1)), 
  nrow(final_coursework_data1)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(final_coursework_data1, g)


