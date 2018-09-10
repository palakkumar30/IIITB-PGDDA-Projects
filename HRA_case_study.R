library(MASS)
library(car)
library(caTools)
library(caret)
library(cowplot)
library(ggplot2)
library(grid)
library(gridExtra)
library(lubridate) # ymd_hms 
library(dplyr)
library(tidyr)
library(ROCR)
library(Information) # IV table
library(scales)
library(ggcorrplot)
library(corrplot)


# Load all datasets
emp_survey<-read.csv("employee_survey_data.csv", stringsAsFactors = F)
emp_gen<-read.csv("general_data.csv", stringsAsFactors = F)
in_time<-read.csv("in_time.csv", stringsAsFactors = F)
mgr_survey<-read.csv("manager_survey_data.csv", stringsAsFactors = F)
out_time<-read.csv("out_time.csv", stringsAsFactors = F)

# structure of the datasets
str(in_time) #4410 obs of 262 variables
str(out_time) #4410 obs of 262 variables
str(emp_survey) #4410 obs of 4 variables
str(mgr_survey) #4410 obs of 3 variables
str(emp_gen) # 4410 obs of 24 variables

# first we will check for the unique ids of the datasets
length(unique(in_time$X)) # 4410, so employeeID stored in 1st column is the unique ID
length(unique(out_time$X)) # 4410,  employeeID in the 1st column is the key
length(unique(emp_survey$EmployeeID)) # 4410, EmployeeID is the primary key
length(unique(mgr_survey$EmployeeID)) # 4410, EmployeeID is the primary key
length(unique(emp_gen$EmployeeID)) # 4410, EmployeeID is the primary key

#Checking duplicates in unique field
sum(duplicated(emp_gen$EmployeeID))    #Return 0
sum(duplicated(emp_survey$EmployeeID)) #Return 0
sum(duplicated(mgr_survey$EmployeeID))  #Return 0
sum(duplicated(in_time$EmployeeID))         #Return 0
sum(duplicated(out_time$EmployeeID))


# checking for NA's in different datasets
sum(is.na(emp_gen)) # 28 
sum(is.na(emp_survey)) # 83 
sum(is.na(mgr_survey))  # 83

## ---------Treating NAs for each dataset -----------

#  1). For emp_gen$NumCompaniesWorked
str(emp_gen)
sum(is.na(emp_gen$NumCompaniesWorked)) #  19
# There are 19 NAs for "NumCompaniesWorked"
# If difference b/w TotalWorkingYears and YearsAtCompany = 0, then NAs can be repalced by "1"
# If difference b/w TotalWorkingYears and YearsAtCompany <= 1, then NAs can be repalced by "2"
# considering that employess do not change teh company usually before completing one year 
# where one of the reasons may be probationary period of minimum 1 year
# other reason may be a contract

# Calculating difference b/w TotalWorkingYears and YearsAtCompany
emp_gen$diff_work_yr<-emp_gen$TotalWorkingYears-emp_gen$YearsAtCompany

emp_gen$NumCompaniesWorked[which(is.na(emp_gen$NumCompaniesWorked))]<-ifelse(emp_gen$diff_work_yr [which(is.na(emp_gen$NumCompaniesWorked))]== 0, 1,
                                                                             (ifelse( emp_gen$diff_work_yr [which(is.na(emp_gen$NumCompaniesWorked))]<=1,2,NA)))

sum(is.na(emp_gen$NumCompaniesWorked)) # NAs are reduced to 9 now

# 1). For emp$TotalWorkingYears
# NAs for TotalWorkingYears can be replaced by YearsAtCompany if NumCompaniesWorked is 0 or 1
sum(is.na(emp_gen$TotalWorkingYears)) # Returns 9
emp_gen$TotalWorkingYears[which(is.na(emp_gen$TotalWorkingYears))]<-ifelse(emp_gen$NumCompaniesWorked[which(is.na(emp_gen$TotalWorkingYears))]<=1,emp_gen$YearsAtCompany[which(is.na(emp_gen$TotalWorkingYears))],NA)
# sum(is.na(emp_gen$NumCompaniesWorked)) # NAs are reduced to 5 now

# Removing column emp_gen$diff_work_yr that was created only for above NA treatment
emp_gen<-emp_gen[, -25]

# Removing columns with constant values 
emp_gen<- emp_gen[, -which(colnames(emp_gen) %in% c('EmployeeCount', 'Over18', 'StandardHours'))]

# checking NA values
sum(is.na(emp_gen))

#-----Working with in_time and out_time files to derive new vriables------
# Checking if all column names match in both in_time and out_time are same
which(!colnames(in_time) == colnames(out_time))
# 0
# creating a dataframe excluding columns having all values as NAs
leave<-in_time[,colSums(is.na(in_time)) != nrow(in_time)]

# ----------Calculating actual leaves------
leave <- as.data.frame(ifelse(is.na(leave[,-1]), 1, 0))
# Total leaves
emp.leaves <- data.frame(EmployeeID = in_time$X, TotLeaves = (rowSums(leave)))
date_df<- data.frame(date=as.Date(gsub('X','', names(leave)), format = '%Y.%m.%d'))
date_df$month <- months(date_df$date, abbreviate = T)
date_df$weekday <- weekdays(date_df$date, abbreviate = T)
date_df$quarter <- quarters(date_df$date)

# leave count calculations
leave_count<- c('weekday', 'month', 'quarter')
for(l in leave_count){
  colnames(leave) <- date_df[, c(l)]
  leave_status<- as.data.frame(sapply(unique(colnames(leave)), 
                                      function(x) rowSums(leave[, colnames(leave)==x, drop=F], na.rm = T)))
  emp.leaves <- cbind(emp.leaves, leave_status)
}

# Plotting the leaves -- Highlights the quarter, weekday and month when leaves taken are more
ggcorrplot(cor(emp.leaves[,-1]), insig = "blank", type="lower")

#--------Calculating Working hours--------
# excluding columns having all values as NAs
in_time<-in_time[,colSums(is.na(in_time)) != nrow(in_time)]
out_time<-out_time[,colSums(is.na(out_time)) != nrow(out_time)]

# Converting the time variables to date format and finding the time difference variable
#in_time[,-1] <- sapply(in_time[,-1], as.POSIXlt)
#out_time[,-1] <- sapply(out_time[,-1], as.POSIXlt)
in_time[,-1] <- sapply(in_time[,-1], ymd_hms)
out_time[,-1] <- sapply(out_time[,-1], ymd_hms)

# Calculating total working hours
work_hrs <- (out_time[,-1] - in_time[,-1])/3600  

# emp_hrs df to store employee working hours
# average working hours calculations
hrs_worked <- data.frame(EmployeeID=as.numeric(in_time$X), Avg.hrs = round(rowMeans(work_hrs, na.rm = T),2))

# working hours by day of week
colnames(work_hrs) <- paste(date_df$weekday, '_hrs', sep = '')
weekday_hrs<- as.data.frame(sapply(unique(colnames(work_hrs)),function(x) round(rowSums(work_hrs[, colnames(work_hrs)==x, drop=F], na.rm = T)/(365/7),2)))
hrs_worked<- cbind(hrs_worked, weekday_hrs)
# outlier treatment
sapply(emp_gen[,c(1,5:6,10,13:21)], function(x) boxplot.stats(x)$out)

##  Missing value treatment  ##
sum(is.na(emp_survey)) # 83 missing values 
sum(is.na(mgr_survey)) # 83 missing values
sum(is.na(emp_gen)) # 14 missing values

sapply(emp_survey, function(x) sum(is.na(x))) # (worklife:38, env:25, jobs:20)

# omit missing values
emp.survey <- na.omit(emp_survey)

## master file creation  ##
master_data<- merge(emp_gen, emp_survey, by="EmployeeID", all = F)
master_data <- merge(master_data, mgr_survey, by = "EmployeeID", all = F)
master_data <- merge(master_data, hrs_worked, by = "EmployeeID", all = F)
master_data <- merge(master_data, emp.leaves, by = "EmployeeID", all = F)
str(master_data) # master file 4300 obs of 57 variables
sapply(master_data[,-c(1:2,13, 27:54)], table)

##----------EDA---------
# Outlier treatment

boxplot(master_data$Age)
# No Outliers 

boxplot(master_data$DistanceFromHome)
# No Outliers 

boxplot(master_data$JobLevel)
# No Outliers 

boxplot(master_data$MonthlyIncome)
# Outliers Found
quantile(as.numeric(master_data$MonthlyIncome,seq(0,1,0.01)))
# Excepting the values as it is as the income is variable in industry and can be a factor for attrition

boxplot(na.omit(master_data$NumCompaniesWorked))
# Outliers Found
quantile(master_data$NumCompaniesWorked,seq(0,1,0.01),na.rm = T)
# Excepting the values as it is as difference is not significant

boxplot(master_data$PercentSalaryHike)
# No Outliers 

boxplot(master_data$StockOptionLevel)
# Outliers Found
quantile(master_data$StockOptionLevel,seq(0,1,0.01))
# Excepting the values as it is as difference is not significant

boxplot(as.numeric(master_data$TotalWorkingYears))
# Outliers Found
quantile(master_data$TotalWorkingYears,seq(0,1,0.01), na.rm = T)
master_data[which(master_data$TotalWorkingYears > 35),]$TotalWorkingYears <- 35
# Outlier correction done for TotalWorkingYears > 35

boxplot(master_data$TrainingTimesLastYear)
# Outliers Found
quantile(master_data$TrainingTimesLastYear,seq(0,1,0.01))
# Excepting the values as it is as difference is not significant

boxplot(master_data$YearsAtCompany)
# Outliers Found
quantile(master_data$YearsAtCompany,seq(0,1,0.01))
master_data[which(master_data$YearsAtCompany > 24),]$YearsAtCompany <- 24
# Outlier correction done for YearsAtCompany > 24

boxplot(master_data$YearsSinceLastPromotion)
# Outliers Found
quantile(master_data$YearsSinceLastPromotion,seq(0,1,0.01))
master_data[which(master_data$YearsSinceLastPromotion > 9),]$YearsSinceLastPromotion <- 9
# Outlier correction done for YearsSinceLastPromotion > 9
# Assuming that the a company will give a promotion atleast once in 10 years

boxplot(master_data$YearsWithCurrManager)
# Outliers Found
quantile(master_data$YearsWithCurrManager,seq(0,1,0.01))
master_data[which(master_data$YearsWithCurrManager > 14),]$YearsWithCurrManager <- 14
# Outlier correction done for YearsWithCurrManager > 14

boxplot(master_data$Avg.hrs)
# Outliers Found
quantile(master_data$Avg.hrs,seq(0,1,0.01))
# Excepting the values as it is as difference is not significant

boxplot(master_data$TotLeaves)
# No Outliers   

#--------------------Bivariate Anallysis----------------
#1.MonthlyIncome vs Attrition
## binning monthly income
master_data$MonthlyIncome <- cut(master_data$MonthlyIncome, breaks = c(10000, 40000, 70000, 100000, 150000, 200000), 
                                 labels = c('10-40k','40-70k','70-100k','100-150k', '150-200k'))
plot1<-ggplot(data= master_data, aes(x= Attrition,  group=MonthlyIncome)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~MonthlyIncome) + scale_y_continuous(labels = scales::percent) + xlab("MonthlyIncome")+ylab("Percentage of Employees") + ggtitle("Fig.1. MonthlyIncome V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No & 2=Yes")

#2. BusinessTravel vs Attrition
plot2<-ggplot(data= master_data, aes(x= Attrition,  group=BusinessTravel)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~BusinessTravel) + scale_y_continuous(labels = scales::percent) + xlab("BusinessTravel")+ylab("Percentage of Employees") + ggtitle("Fig.2. BusinessTravel V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No & 2=Yes")

#3. Department vs Attrition
plot3<-ggplot(data= master_data, aes(x= Attrition,  group=Department)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~Department) + scale_y_continuous(labels = scales::percent) + xlab("Department")+ylab("Percentage of Employees") + ggtitle("Fig.3. Department V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No & 2=Yes")

#4. Education vs. Attrition
plot4<-ggplot(data= master_data, aes(x= Attrition,  group=Education)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~Education) + scale_y_continuous(labels = scales::percent) + xlab("Education")+ylab("Percentage of Employees") + ggtitle("Fig.4. Education V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No & 2=Yes")

#5. Gender vs. Attrition
plot5<-ggplot(data= master_data, aes(x= Attrition,  group=Gender)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~Gender) + scale_y_continuous(labels = scales::percent) + xlab("Gender")+ylab("Percentage of Employees") + ggtitle("Fig.5. Gender V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No & 2=Yes")

#6. Marital Status vs. Attrition
plot6<-ggplot(data= master_data, aes(x= Attrition,  group=MaritalStatus)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~MaritalStatus) + scale_y_continuous(labels = scales::percent) + xlab("MaritalStatus")+ylab("Percentage of Employees") + ggtitle("Fig.6. MaritalStatus V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No & 2=Yes")

#6. EnvironmentSatisfaction vs. Attrition
plot6<-ggplot(data= na.omit(master_data), aes(x= Attrition,  group=EnvironmentSatisfaction)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~EnvironmentSatisfaction) + scale_y_continuous(labels = scales::percent) + xlab("EnvironmentSatisfaction")+ylab("Percentage of Employees") + ggtitle("Fig.7. EnvironmentSatisfaction V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No & 2=Yes")

#7. WorkLifeBalance vs. Attrition
plot7<-ggplot(data= na.omit(master_data), aes(x= Attrition,  group=WorkLifeBalance)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~WorkLifeBalance) + scale_y_continuous(labels = scales::percent) + xlab("WorkLifeBalance")+ylab("Percentage of Employees") + ggtitle("Fig.8. WorkLifeBalance V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No & 2=Yes")

#8. PercentSalaryHike vs. Attrition
plot8<-ggplot(data= master_data, aes(x= Attrition,  group=PercentSalaryHike)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~PercentSalaryHike) + scale_y_continuous(labels = scales::percent) + xlab("PercentSalaryHike")+ylab("Percentage of Employees") + ggtitle("Fig.9. PercentSalaryHike V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No & 2=Yes")

#9. JobRole vs. Attrition
plot9<-ggplot(data= master_data, aes(x= Attrition,  group=JobRole)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~JobRole) + scale_y_continuous(labels = scales::percent) + xlab("JobRole")+ylab("Percentage of Employees") + ggtitle("Fig.10. JobRole V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No & 2=Yes")

#10. StockOptionLevel vs. Attrition
plot10<-ggplot(data= master_data, aes(x= Attrition,  group=StockOptionLevel)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~StockOptionLevel) + scale_y_continuous(labels = scales::percent) + xlab("StockOptionLevel")+ylab("Percentage of Employees") + ggtitle("Fig.11. StockOptionLevel V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No & 2=Yes")

#11. JobSatisfaction vs. Attrition
plot11<-ggplot(data= na.omit(master_data), aes(x= Attrition,  group=JobSatisfaction)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~JobSatisfaction) + scale_y_continuous(labels = scales::percent) + xlab("JobSatisfaction")+ylab("Percentage of Employees") + ggtitle("Fig.12. JobSatisfaction V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No & 2=Yes")

#12. NumCompaniesWorked vs. Attrition
plot12<-ggplot(data= na.omit(master_data), aes(x= Attrition,  group=NumCompaniesWorked)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Attrition") + facet_grid(~NumCompaniesWorked) + scale_y_continuous(labels = scales::percent) + xlab("NumCompaniesWorked")+ylab("Percentage of Employees") + ggtitle("Fig.13. NumCompaniesWorked V/s Attrition Status") + scale_fill_discrete(name = "Attrition 1=No & 2=Yes")
grid.arrange(plot1,plot2,ncol=2, top=textGrob("Bivariate Analysis", gp=gpar(fontsize=12, font = 2)))
grid.arrange(plot3,plot4,ncol=2, top=textGrob("Bivariate Analysis", gp=gpar(fontsize=12, font = 2)))
grid.arrange(plot5,plot6,ncol=2, top=textGrob("Bivariate Analysis", gp=gpar(fontsize=12, font = 2)))
grid.arrange(plot7,plot8,ncol=2, top=textGrob("Bivariate Analysis", gp=gpar(fontsize=12, font = 2)))
plot9
plot12
grid.arrange(plot10,plot11,ncol=2, top=textGrob("Bivariate Analysis", gp=gpar(fontsize=12, font = 2)))
grid.arrange(plot11,ncol=2, top=textGrob("Univariate Analysis", gp=gpar(fontsize=12, font = 2)))
ggcorrplot(cor(master_data[,-c(1,3:5,8:9,11:13)]), insig = "blank")
# Findings: YearsSinceLastPromotion & YearsAtCompany are highly correlated so does yearsWithCurrManager
# Yearsatcompany is directly correlated with age

# Scaling continuous features
master_data[,-c(1,3:5,8:9,11:14, 17, 19, 21, 27:32)] <- as.data.frame(scale(master_data[,-c(1,3:5,8:9,11:14, 17, 19, 21, 27:32)]))

# converting target variable Attrition from No/Yes character to factorwith levels 0/1 
master_data$Attrition<- ifelse(master_data$Attrition=="Yes",1,0)

# Checking attrition rate of prospect customer
Attrition <- sum(master_data$Attrition)/nrow(master_data)
Attrition # 16.12% attrition rate which is near to 15% as mentioned in the case study

# creating a dataframe of categorical features
master_data_chr<- master_data[,c(4:5,8,11:13)]
# converting categorical attributes to factor
master_data_fact<- data.frame(sapply(master_data_chr, function(x) factor(x)))
str(master_data_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(master_data_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =master_data_fact))[,-1]))
# for gender male is 1 and female is 0
master_data$Gender <- ifelse(master_data$Gender=='Male', 1, 0)
# Final dataset
master_final<- cbind(master_data[,-c(1,4:5,8, 11:13)],dummies) # removed the ID field: EmployeeID
View(master_final) # 4410 obs. of  71 variables

# splitting the data between train and test
set.seed(123) # for reproducibility

ntrain = sample.split(master_final$Attrition, SplitRatio = 0.7) #splitting indices
train = master_final[ntrain, ] # train data consisting of 70% of original obs.
test = master_final[!ntrain, ] # test data consisting of 30% of original obs.

# Logistic Regression: 
#Initial model
model = glm(Attrition ~ ., data = train, family = "binomial")
summary(model) #AIC 2121 coeff..nullDev 2658.8 ...resDev 1993

# Stepwise selection
model2 <- stepAIC(model, direction="both")
summary(model2) # AIC: 2065.6

##################################################
## Removing Multicollinearity through VIF check #
#################################################
vif(model2)

# Removing BusinessTravel.xTravel_Rarely with high VIF 3.860808 value and insignificant p value
model3 <- glm(formula = Attrition ~ Age + JobLevel + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + JobInvolvement + Avg.hrs + Fri + Sep + 
                BusinessTravel.xTravel_Frequently +  
                Department.xResearch...Development + Department.xSales + 
                JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + Apr, family = "binomial", 
              data = train)

summary(model3) # AIC: 2072
vif(model3) 
  
# Removing Apr having insignificant p value
model4 <- glm(formula = Attrition ~ Age + JobLevel + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + JobInvolvement + Avg.hrs + Fri + Sep + 
                BusinessTravel.xTravel_Frequently +  
                Department.xResearch...Development + Department.xSales + 
                JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model4) # AIC: 2072.4
vif(model4) 

# Removing Fri having insignificant p value insignificant p value
model5 <- glm(formula = Attrition ~ Age + JobLevel + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + JobInvolvement + Avg.hrs + Sep + 
                BusinessTravel.xTravel_Frequently +  
                Department.xResearch...Development + Department.xSales + 
                JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model5) # AIC: 2072.6
vif(model5) 

# Removing JobRole.xResearch.Scientist having insignificant p value insignificant p value
model6<- glm(formula = Attrition ~ Age + JobLevel + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + JobInvolvement + Avg.hrs + Sep + 
                BusinessTravel.xTravel_Frequently +  
                Department.xResearch...Development + Department.xSales + 
                JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model6) # AIC: 2074.4
vif(model6) 

# Removing Sep having insignificant p value insignificant p value
model7<- glm(formula = Attrition ~ Age + JobLevel + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
               WorkLifeBalance + JobInvolvement + Avg.hrs + 
               BusinessTravel.xTravel_Frequently +  
               Department.xResearch...Development + Department.xSales + 
               JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + 
               JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
             data = train)

summary(model7) # AIC: 2075.7
vif(model7) 

# Removing JobRole.xSales.Executive having insignificant p value
model8<- glm(formula = Attrition ~ Age + JobLevel + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
               WorkLifeBalance + JobInvolvement + Avg.hrs + 
               BusinessTravel.xTravel_Frequently +  
               Department.xResearch...Development + Department.xSales + 
               JobRole.xLaboratory.Technician + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + MaritalStatus.xSingle, family = "binomial", 
             data = train)

summary(model8) # AIC: 2079.4
vif(model8) 


# Removing JobRole.xLaboratory.Technician having insignificant p value 
model9<- glm(formula = Attrition ~ Age + JobLevel + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
               WorkLifeBalance + JobInvolvement + Avg.hrs + 
               BusinessTravel.xTravel_Frequently +  
               Department.xResearch...Development + Department.xSales + 
               JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + MaritalStatus.xSingle, family = "binomial", 
             data = train)

summary(model9) # AIC: 2079.3
vif(model9) 

# Removing JobInvolvement having insignificant p value 
model10<- glm(formula = Attrition ~ Age + JobLevel + NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
               WorkLifeBalance + Avg.hrs + 
               BusinessTravel.xTravel_Frequently +  
               Department.xResearch...Development + Department.xSales + 
               JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + MaritalStatus.xSingle, family = "binomial", 
             data = train)

summary(model10) # AIC: 2080.5
vif(model10) 

# Removing JobRole.xResearch.Director having insignificant p value 
model11<- glm(formula = Attrition ~ Age + JobLevel + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + Avg.hrs + 
                BusinessTravel.xTravel_Frequently +  
                Department.xResearch...Development + Department.xSales + 
                JobRole.xManufacturing.Director + MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model11) # AIC: 2082.6
vif(model11)

# Removing JobLevel having insignificant p value 
model12<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + Avg.hrs + 
                BusinessTravel.xTravel_Frequently +  
                Department.xResearch...Development + Department.xSales + 
                JobRole.xManufacturing.Director + MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model12) # AIC: 2084.7
vif(model12)

# Removing TrainingTimesLastYear having insignificant p value 
model13<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + Avg.hrs + 
                BusinessTravel.xTravel_Frequently +  
                Department.xResearch...Development + Department.xSales + 
                JobRole.xManufacturing.Director + MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model13) # AIC: 2084.7
vif(model13)

# Removing Department.xResearch...Development having high VIF value 4.098017
model14<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + Avg.hrs + 
                BusinessTravel.xTravel_Frequently + Department.xSales + 
                JobRole.xManufacturing.Director + MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model14) # AIC: 2112.1
vif(model14)

# Removing Department.xResearch...Development having high VIF value 4.098017
model15<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + Avg.hrs + 
                BusinessTravel.xTravel_Frequently + Department.xSales + 
                JobRole.xManufacturing.Director + MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model15) # AIC: 2112.1
vif(model15)

# Removing Department.xSales having insignificant p value
model16<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + Avg.hrs + BusinessTravel.xTravel_Frequently + 
                JobRole.xManufacturing.Director + MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model16) # AIC: 2111.7
vif(model16)


# Removing WorkLifeBalance having insignificant p value
model17<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                Avg.hrs + BusinessTravel.xTravel_Frequently + 
                JobRole.xManufacturing.Director + MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model17) # AIC: 2134.7
vif(model17)


# Final model17 inludes below variables
#Age
#NumCompaniesWorked
#TotalWorkingYears
#YearsSinceLastPromotion
#YearsWithCurrManager
#EnvironmentSatisfaction
#JobSatisfaction
#Avg.hrs
#BusinessTravel.xTravel_Frequently
#JobRole.xManufacturing.Director
#MaritalStatus.xSingle

##-------- Model Evaluation ----------

### Test Data ####
#predicted probabilities of attrition '1' for test data
test.pred = predict(model17, type = "response", 
                    newdata = test)
summary(test.pred) # summary of the predicted values

test$prob <- test.pred
View(test)
# Let's use the probability cutoff of 50%.
test.pred.attrition <- factor(ifelse(test.pred >= 0.50, "Yes", "No"))
test.actual.attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

# Creating the confusion Matrix
table(test.actual.attrition, test.pred.attrition)
# although 86% accuracy; 98% specificity & a low Sensitivity with vaue as 76%

# when the probability cutoff of is 40%. 
test.pred.attrition <- factor(ifelse(test.pred >= 0.40, "Yes", "No"))
test.conf <- confusionMatrix(test.pred.attrition, test.actual.attrition, positive = "Yes")
test.conf
# although 86% accuracy; 96% specificity & a low Sensitivity with vaue as 34%


# when the probability cutoff of is 25%. 
test.pred.attrition <- factor(ifelse(test.pred >= 0.25, "Yes", "No"))
test.conf <- confusionMatrix(test.pred.attrition, test.actual.attrition, positive = "Yes")
test.conf
#  82% Accuracy; 86% specificity & a low Sensitivity with vaue as 59%


#############################
## Optimal cut off value ##
#############################
# Let's find out the optimal probalility cutoff 
# helper function to seperate model performance data from confusion matrix
optim_cut <- function(cutoff)  {
  predicted.attrition <- factor(ifelse(test.pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted.attrition, test.actual.attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) #transpose the matrix
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.
# Summary of test probability
summary(test.pred)

cutoff.data = seq(.01,.80,length=100)
cmdata = matrix(0,100,3) # matrix to hold model performance data

for(i in 1:100){
  cmdata[i,] = optim_cut(cutoff.data[i])
} 
(cutoff <- cutoff.data[which(abs(cmdata[,1]-cmdata[,2]) < 0.01)])
plot(cutoff.data, cmdata[,1], xlab="Cutoff", ylab="Value", cex.lab=1,
     cex.axis=1, ylim=c(0,1), type="l", lwd=2, axes=FALSE, col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoff.data,cmdata[,2],col="darkgreen",lwd=2)
lines(cutoff.data,cmdata[,3],col=4,lwd=2)
box()
legend(0.10,0.25,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),
       c("Sensitivity","Specificity","Accuracy"))

# Let's choose a cutoff value of 0.17 for final model
test.cutoff.attrition <- factor(ifelse(test.pred >= 0.17, "Yes", "No"))
conf.final <- confusionMatrix(test.cutoff.attrition, test.actual.attrition, positive = "Yes")
acc <- conf.final$overall[1] # 73%
acc
sens <- conf.final$byClass[1] # 72%
sens
spec <- conf.final$byClass[2] # 74%
spec

### KS -statistic - Test Data ######
test.cutoff.attrition <- ifelse(test.cutoff.attrition=="Yes",1,0)
test.actual.attrition <- ifelse(test.actual.attrition=="Yes",1,0)
#on testing  data
pred.object.test<- prediction(test.cutoff.attrition, test.actual.attrition)
performance.measures.test<- performance(pred.object.test, measure = "tpr", x.measure = "fpr")

#----- Plotting Receiver Operating Characteristics (ROC) Curve: AUC calculation ------

plot(performance.measures.test, type = "b", col = "red", lwd=1.5,
     main = "ROC Curve",
     ylab = "Sensitivity:TPR", 
     xlab = "(1 - Specificity):FPR")
abline(0,1, lty = 8, col = "grey", untf = T)
auc<-performance(pred.object.test,"auc")
auc.value <- unlist(auc@y.values)
text(0.8, 0.23, labels=sprintf("AUC: %0.3f", auc.value))

ks.table.test <- attr(performance.measures.test, "y.values")[[1]] - 
  (attr(performance.measures.test, "x.values")[[1]])
max(ks.table.test) # 0.4523927


#---------- Lift & Gain Chart --------------

# Creating function to calculate gain & lift
calcLift <- function(labels , predicted.prob, groups=10) {
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted.prob)) predicted.prob <- as.integer(as.character(predicted.prob))
  helper = data.frame(cbind(labels , predicted.prob))
  helper[,"bucket"] = ntile(-helper[,"predicted.prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}
attrition.decile  <-  calcLift(test.actual.attrition, test.pred, groups = 10)

#Plotting the lift chart
plot(attrition.decile$Cumlift, type="l", lwd=2, col="red",# lty=4,
     xlim = c(0,10),
     ylim = c(0,4),
     main = "Lift Chart",
     xlab = "Decile",
     ylab = "Lift")
abline(h=1, col="brown")
axis(1, 1:10)
abline(h=0:10, v=0:10, lty=3)

# Plotting Gain Chart 
#library(InformationValue)
#ks_plot(test.actual.attrition, test.cutoff.attrition) # Gain chart plot
