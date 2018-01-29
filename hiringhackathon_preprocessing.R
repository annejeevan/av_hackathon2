#to clear the global environmental
rm(list=ls(all=TRUE))


#############################################  loading the required libraries #######################################
#install.packages(('data.table'))
#install.packages(('tidyr'))
#install.packages(('dplyr'))
#install.packages(('stringr'))
#install.packages(('lubridate'))
#install.packages(('caret'))
#install.packages(('funModeling'))


library(funModeling)
library(caret)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)




#############################################  loading the datasets ##################################################

train <- fread("/home/jeevan/Documents/Competitions/AnalyticsVidhya/MCKinseyHiringHackathon2/train_5ZizJYZ/train.csv")
test <-  fread("/home/jeevan/Documents/Competitions/AnalyticsVidhya/MCKinseyHiringHackathon2/test_StcJUVI/test.csv")


train_1 <- train
test_1 <- test

############################################# summarization ######################################################
df_status(train)
df_status(test)

df_status(train_1)
df_status(test_1)

### target variable 
barplot(table(train$Approved))
prop.table(table(train$Approved))

head(train)
dim(train)


head(test)
dim(test)

######################################################### Preprocessing and Exploration ######################################################


###### converting the data types

## train

train_1$Approved <- as.factor(train_1$Approved)
train_1$Var1 <- as.factor(train_1$Var1)
train_1$DOB <- as.Date(train_1$DOB, format = "%d/%m/%y")
train_1$Lead_Creation_Date <- as.Date(train_1$Lead_Creation_Date, format = "%d/%m/%y")
year(train_1$DOB) <- 1900 + year(train_1$DOB) %% 100
train_1$Loan_Period[train_1$Loan_Period == 6] <- ''


###### profiling
city_code_table = table(train_1$City_Code)
city_code_rare = names(city_code_table[city_code_table<100])
train_1[train_1$City_Code %in% city_code_rare,]$City_Code <- 'Others'

employer_code_table = table(train_1$Employer_Code)
employer_code_rare = unique(names(employer_code_table[employer_code_table<30]))
train_1[train_1$Employer_Code %in% employer_code_rare,]$Employer_Code <- 'Others'


source_table = table(train_1$Source)
source_rare = names(source_table[source_table<10])
train_1[train_1$Source %in% source_rare,]$Source <- 'Others'
######################## test

test_1$Var1 <- as.factor(test_1$Var1)
test_1$DOB <- as.Date(test_1$DOB, format = "%d/%m/%y")
test_1$Lead_Creation_Date <- as.Date(test_1$Lead_Creation_Date, format = "%d/%m/%y")
year(test_1$DOB) <- 1900 + year(test_1$DOB) %% 100
test_1$Loan_Period[test_1$Loan_Period == 6] <- ''


test_1[test_1$City_Code %in% city_code_rare,]$City_Code <- 'Others'
test_1[test_1$City_Code %in% unique(setdiff(test_1$City_Code, train_1$City_Code)),]$City_Code <- ''



test_1[test_1$Employer_Code %in% employer_code_rare,]$City_Code <- 'Others'
test_1[test_1$Employer_Code %in% unique(setdiff(test_1$Employer_Code, train_1$Employer_Code)),]$Employer_Code <- ''


test_1[test_1$Source %in% source_rare,]$Source <- 'Others'
test_1[test_1$Source %in% unique(setdiff(test_1$Source, train_1$Source)),]$Source <- ''
################ treating some of the variables


#### 0) Creating Dummy Variable of class factors Gender
  #0.i) is_male
is_male <- function(x) {
  if(x == "Male") {
    y <- 1
  } else {
    y <- 0
  }
  return(y)
}

train_1 <- cbind(train_1,is_male = as.factor(mapply(is_male,train_1$Gender)))
test_1 <- cbind(test_1,is_male = as.factor(mapply(is_male,test_1$Gender)))

rm(is_male)


#### 1) Adding age by using DOB and Lead_Creation_Date column 

train_1 <- cbind(train_1,age = as.integer(round((train_1$Lead_Creation_Date - train_1$DOB)/365,digits =0)))
test_1 <- cbind(test_1,age = as.integer(round((test_1$Lead_Creation_Date - test_1$DOB)/365,digits = 0)))


#### 2) Extraction and Addition of DOB month and year
train_1 <- cbind(train_1,DOB_month = as.factor(format(train_1$DOB,'%m')))
train_1 <- cbind(train_1,DOB_year = as.factor(format(train_1$DOB,'%Y')))

test_1 <- cbind(test_1,DOB_month = as.factor(format(test_1$DOB,'%m')))
test_1 <- cbind(test_1,DOB_year = as.factor(format(test_1$DOB,'%Y')))


#### 3) Extraction and Addition of Lead_Creation_Date month
train_1 <- cbind(train_1,Lead_Creation_day = as.factor(format(train_1$Lead_Creation_Date,'%d')))
train_1 <- cbind(train_1,Lead_Creation_month = as.factor(format(train_1$Lead_Creation_Date,'%m')))
train_1 <- cbind(train_1,Lead_Creation_year = as.factor(format(train_1$Lead_Creation_Date,'%Y')))

test_1 <- cbind(test_1,Lead_Creation_day = as.factor(format(test_1$Lead_Creation_Date,'%d')))
test_1 <- cbind(test_1,Lead_Creation_month = as.factor(format(test_1$Lead_Creation_Date,'%m')))
test_1 <- cbind(test_1,Lead_Creation_year = as.factor(format(test_1$Lead_Creation_Date,'%Y')))



###### dropping 


cols <- c("City_Code", "City_Category", "Employer_Code", "Employer_Category1","Employer_Category2", "Customer_Existing_Primary_Bank_Code","Primary_Bank_Type","Contacted",
          "Source","Source_Category" )

for (i in cols) {
  train_1$i <- as.factor(train_1$i)
  test_1$i <- as.factor(test_1$i)
}


#saving to files

write.csv(test_1,file="test_1.csv",row.names=FALSE)
write.csv(train_1,file="train_1.csv",row.names=FALSE)
getwd()
