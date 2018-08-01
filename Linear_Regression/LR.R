library(tidyr)
library(dplyr)
library(MASS) # stepAIC
library(car) # VIF 
library(ggplot2)
library(grid)
library(gridExtra)
library(ggcorrplot)

# Import the dataset in R 
cars <- read.csv("CarPrice_assignment.csv", stringsAsFactors = F)
str(cars)
# Data Prepararion
# Remove car_id
cars<-cars[-1]
# Convert all columns' values to lower case  
colnames(cars)<-tolower(colnames(cars))

# Missing value checks
sum(is.na(cars))

# Checking Duplicates 
cars <- cars[!duplicated(cars),]

# Split the carname column in to 2
cars <- separate(cars, carname, c('company', 'model'), sep = '[[:blank:]]', extra = 'merge', fill = 'right')

# Remove column "model" from the dataset
cars<-cars[,-3]

#Correcting misspelled company names
table(cars$company)
cars$company[which(cars$company=='porcshce')]<-'porsche'
cars$company[which(cars$company=='toyouta')]<-'toyota'
cars$company[which(cars$company=='vokswagen')]<-'volkswagen'
cars$company[which(cars$company=='vw')]<-'volkswagen'
cars$company[which(cars$company=='Nissan')]<-'nissan'
cars$company[which(cars$company=='maxda')]<-'mazda'
table(cars$company)

# -----EDA------
# Univariate Analysis on Categorical variables
plot1<-ggplot(cars) +geom_bar(aes(x=symboling), fill="gold", col="red") + scale_x_continuous(breaks = seq(-3,3,1)) + scale_y_continuous(breaks = seq(0,100,5)) + labs(title="Fig.1 - Univariate Analysis on Symboling",x="Symboling",y="Count")

plot2<-ggplot(cars) +geom_bar(aes(x=fueltype), fill="gold", col="red")+ scale_y_continuous(breaks = seq(0,250,10)) + labs(title="Fig.2 - Univariate Analysis on Fuel Type",x="Fuel Type",y="Count")

plot3<-ggplot(cars) +geom_bar(aes(x=aspiration), fill="gold",col="red")+ scale_y_continuous(breaks = seq(0,250,10)) + labs(title="Fig.3 - Univariate Analysis on Aspiration",x="Aspiration",y="Count")

plot4<-ggplot(cars) +geom_bar(aes(x=doornumber), fill="gold",col="red")+ scale_y_continuous(breaks = seq(0,250,10)) + labs(title="Fig.4 - Univariate Analysis on Doornumber",x="Doornumber",y="Count")

grid.arrange(plot1,plot2,plot3,plot4,ncol=2, top=textGrob("Univariate Analysis on Categorical attributes", gp=gpar(fontsize=12, font = 2)))

plot5<-ggplot(cars) +geom_bar(aes(x=carbody), fill="blue")+ scale_y_continuous(breaks = seq(0,250,10)) + labs(title="Fig.5 - Univariate Analysis on Carbody",x="Carbody",y="Count")

plot6<-ggplot(cars) +geom_bar(aes(x=drivewheel), fill="blue")+ scale_y_continuous(breaks = seq(0,250,10)) + labs(title="Fig.6 - Univariate Analysis on Drivewheel",x="Drivewheel",y="Count")

plot7<-ggplot(cars) +geom_bar(aes(x=enginelocation), fill="blue")+ scale_y_continuous(breaks = seq(0,250,10)) + labs(title="Fig.7 - Univariate Analysis on Enginelocation",x="Enginelocation",y="Count")

grid.arrange(plot5,plot6,plot7,ncol=2, top=textGrob("Univariate Analysis on Categorical attributes", gp=gpar(fontsize=12, font = 2)))

plot8<-ggplot(cars) +geom_bar(aes(x=company), fill="blue")+ scale_y_continuous(breaks = seq(0,50,5)) + labs(title="Fig.8 - Univariate Analysis on Company",x="Company",y="Count")

plot9<-ggplot(cars) +geom_bar(aes(x=cylindernumber), fill="blue")+ scale_y_continuous(breaks = seq(0,250,10)) + labs(title="Fig.9 - Univariate Analysis on Cylindernumber",x="Cylindernumber",y="Count")

plot10<-ggplot(cars) +geom_bar(aes(x=fuelsystem), fill="blue")+ scale_y_continuous(breaks = seq(0,250,10)) + labs(title="Fig.10 - Univariate Analysis on fuelsystem",x="fuelsystem",y="Count")

plot11<-ggplot(cars) +geom_bar(aes(x=enginetype), fill="blue")+ scale_y_continuous(breaks = seq(0,250,10)) + labs(title="Fig.11 - Univariate Analysis on enginetype",x="enginetype",y="Count")

grid.arrange(plot9,plot10,plot11,ncol=2, top=textGrob("Univariate Analysis on Categorical attributes", gp=gpar(fontsize=12, font = 2)))


# Univariate Analysis on Numeric variables
plot12<-ggplot(cars) + geom_boxplot(aes(x=1,y=wheelbase))+labs(title="Fig.12 - WheelBase distribution",x="",y="WheelBase")
plot13 <- ggplot(cars) + geom_boxplot(aes(x=1,y=carlength)) +labs(title="Fig.13 - CarLength distribution",x="",y="Car Length")
plot14 <- ggplot(cars) + geom_boxplot(aes(x=1,y=carwidth)) +labs(title="Fig.14 - Car Width distribution",x="",y="Car Width")
plot15 <- ggplot(cars) + geom_boxplot(aes(x=1,y=carheight)) +labs(title="Fig.15 - Car Height distribution",x="",y="Car Height")
plot16 <- ggplot(cars) + geom_boxplot(aes(x=1,y=curbweight)) +labs(title="Fig.16 - Curb Weight distribution",x="",y="Curb Weight")
plot17 <- ggplot(cars) + geom_boxplot(aes(x=1,y=enginesize)) +labs(title="Fig.17 - Engine Size distribution",x="",y="Engine Size")
plot18 <- ggplot(cars) + geom_boxplot(aes(x=1,y=boreratio)) +labs(title="Fig.18 - Bore Ratio distribution",x="",y="Bore Ratio")
plot19 <- ggplot(cars) + geom_boxplot(aes(x=1,y=stroke)) +labs(title="Fig.19 - Stroke distribution",x="",y="Stroke")
plot20 <- ggplot(cars) + geom_boxplot(aes(x=1,y=compressionratio)) + labs(title="Fig.20 - Compression Ratio\ndistribution",x="",y="Compression Ratio")
plot21 <- ggplot(cars) + geom_boxplot(aes(x=1,y=horsepower)) +labs(title="Fig.21 - Hoursepower\ndistribution",x="",y="horsepower")
plot22 <- ggplot(cars) + geom_boxplot(aes(x=1,y=peakrpm)) +labs(title="Fig.22 - Peak RPM\ndistribution",x="",y="Peak RPM")
plot23 <- ggplot(cars) + geom_boxplot(aes(x=1,y=citympg)) +labs(title="Fig.23 - City Mileage\ndistribution",x="",y="City Mileage")
plot24 <- ggplot(cars) + geom_boxplot(aes(x=1,y=highwaympg)) + labs(title="Fig.24 - Highway Mileage\ndistribution",x="",y="Highway Mileage")

grid.arrange(plot12,plot13,plot14,plot15,plot16,plot17,ncol=3)
grid.arrange(plot18,plot19,plot20,plot21,plot22,plot23,plot24,ncol=4)

# From the above analysis, it is clearly visible that some attributes have outliers such as: 
# enginesize, compression ratio, hoursepower, peakrpm

##  ---------------Treating Outliers-------------------
quantile(cars$horsepower,seq(0,1,0.01))
# Huge difference in the value is observed after 99% percentile.
# hence for all the values above 99% we will mark the value as 207
cars$horsepower[which(cars$horsepower>207)]<-207

quantile(cars$enginesize,seq(0,1,0.01))
# Huge difference in the value is observed after 96% percentile.
# hence for all the values above 96% we will mark the value as 209
cars$enginesize[which(cars$enginesize>209)]<-209

quantile(cars$compressionratio,seq(0,1,0.01))
# Huge difference in the value is observed after 90% percentile.
# hence for all the values above 90% we will mark the value as 10.94
cars$compressionratio[which(cars$compressionratio>10.94)]<-10.94

##------------- Analysing highly corelated numeric variables (Identifying Multicollinear variables)-----------------
car_corelation_matrix <- cor(cars%>% dplyr::select(peakrpm,wheelbase,carlength,carwidth,carheight,curbweight,enginesize,boreratio,stroke,compressionratio,horsepower,citympg,highwaympg,price,symboling))
ggcorrplot(car_corelation_matrix, lab=TRUE, insig = "blank", type="lower")
# Form the results we observe that many variables are highly correlated such as 
# citympg and highwaympg, enginesize and horsepower,wheelbase and carlength, carlength and curbweight
# Only one of the above pairs of highly correlated variables will be kept and the other one has to be dropped off from the model else they will casuse multicollinearity resulting in a weak model

## ------Analysing categorical variables by creating Dummy Variables--------
# We will use model.matrix function for levels more than 2 in a variable else we will assign the levels as 0 and 1
# 1.Converting company
dummy_company <- model.matrix(~company,data=cars)
#View(dummy_company)
dummy_company<-dummy_company[,-1]
cars_1<-cbind(cars[-2],dummy_company)
#View(cars_1)

# 2. Converting fueltype -0,1
# table(cars$fueltype)   #to check whether to apply model matrix or assign 0,1 as per number of levels
cars_1$fueltype<-ifelse(cars_1$fueltype=="diesel",1,0)

# 3. Converting aspiration - 0,1
# table(cars$aspiration)
cars_1$aspiration<-ifelse(cars_1$aspiration=="std",1,0)

# 4. Converting carbody
dummy_carbody<- model.matrix(~carbody,data=cars)
dummy_carbody<-dummy_carbody[,-1]
cars_1<-cbind(cars_1 %>% dplyr:: select(-carbody), dummy_carbody)

# 5. Converting drivewheel
dummy_drivewheel<- model.matrix(~drivewheel,data=cars)
cars_1<-cbind(cars_1 %>% dplyr:: select(-drivewheel), dummy_drivewheel[,-1])

# 6. Converting enginelocation - 0,1
# table(cars$enginelocation)
cars_1$enginelocation<-ifelse(cars_1$enginelocation=="front",1,0)

# 7. Converting enginetype
dummy_enginetype<- model.matrix(~enginetype,data=cars)
cars_1<-cbind(cars_1 %>% dplyr:: select(-enginetype), dummy_enginetype[,-1])

# 8. Converting cylindernumber
dummy_cylindernumber<- model.matrix(~cylindernumber,data=cars)
cars_1<-cbind(cars_1 %>% dplyr:: select(-cylindernumber), dummy_cylindernumber[,-1])

# 9. Converting fuelsystem
dummy_fuelsystem<- model.matrix(~fuelsystem,data=cars)
cars_1<-cbind(cars_1 %>% dplyr:: select(-fuelsystem), dummy_fuelsystem[,-1])

# 10.Converting doornumber - 0,1
# table(cars$doornumber)
cars_1$doornumber<-ifelse(cars_1$doornumber=="four",1,0)

## ----------------Data Modeling-----------------
# Divide the dataset into training and test data set
# set the seed to 100 for the reproducibility of random numbers generated
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(cars_1), 0.7*nrow(cars_1))
# generate the train data set
train = cars_1[trainindices,]

# Similarly store the rest of the observations into an object "test".
test = cars_1[-trainindices,]

model_1 <- lm(price ~ ., data = train) #R-squared:  0.8798
summary(model_1)

# First let us filter the significant varibles one shot using StepAIC function 
step <- stepAIC(model_1, direction = 'both')
step

model_2<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
     carwidth + curbweight + enginesize + stroke + peakrpm + citympg + 
     companyaudi + companybmw + companybuick + companychevrolet + 
     companydodge + companyhonda + companyisuzu + companyjaguar + 
     companymazda + companymercury + companymitsubishi + companynissan + 
     companypeugeot + companyplymouth + companyporsche + companyrenault + 
     companysaab + companysubaru + companytoyota + companyvolkswagen + 
     companyvolvo + carbodywagon + drivewheelrwd + cylindernumberfive + 
     cylindernumberfour + cylindernumbersix + fuelsystem2bbl + 
     fuelsystemmpfi, data = train)

summary(model_2)  # Multiple R-squared:  0.9764,	Adjusted R-squared:  0.9681 

## check for multicollinearity an alternative to correlation analysis
vif(model_2)

# remove citympg: it is not significant having p-value=0.135758  > 0.05 and VIF=7.861817
model_3<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carwidth + curbweight + enginesize + stroke + peakrpm + 
              companyaudi + companybmw + companybuick + companychevrolet + 
              companydodge + companyhonda + companyisuzu + companyjaguar + 
              companymazda + companymercury + companymitsubishi + companynissan + 
              companypeugeot + companyplymouth + companyporsche + companyrenault + 
              companysaab + companysubaru + companytoyota + companyvolkswagen + 
              companyvolvo + carbodywagon + drivewheelrwd + cylindernumberfive + 
              cylindernumberfour + cylindernumbersix + fuelsystem2bbl + 
              fuelsystemmpfi, data = train)

summary(model_3)  # Multiple R-squared:  0.9759,	Adjusted R-squared:  0.9677 

## check for multicollinearity an alternative to correlation analysis
vif(model_3)

# remove companyaudi: it is not significant having p-value=0.277918 > 0.05 and VIF=6.544981
model_4<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carwidth + curbweight + enginesize + stroke + peakrpm + 
              companybmw + companybuick + companychevrolet + 
              companydodge + companyhonda + companyisuzu + companyjaguar + 
              companymazda + companymercury + companymitsubishi + companynissan + 
              companypeugeot + companyplymouth + companyporsche + companyrenault + 
              companysaab + companysubaru + companytoyota + companyvolkswagen + 
              companyvolvo + carbodywagon + drivewheelrwd + cylindernumberfive + 
              cylindernumberfour + cylindernumbersix + fuelsystem2bbl + 
              fuelsystemmpfi, data = train)

summary(model_4)  # Multiple R-squared:  0.9756,	Adjusted R-squared:  0.9677 

## check for multicollinearity an alternative to correlation analysis
vif(model_4)

# remove wheelbase: it is not significant having p-value=0.128792 > 0.05 and VIF=7.936957
model_5<-lm(formula = price ~ aspiration + enginelocation + 
              carwidth + curbweight + enginesize + stroke + peakrpm + 
              companybmw + companybuick + companychevrolet + 
              companydodge + companyhonda + companyisuzu + companyjaguar + 
              companymazda + companymercury + companymitsubishi + companynissan + 
              companypeugeot + companyplymouth + companyporsche + companyrenault + 
              companysaab + companysubaru + companytoyota + companyvolkswagen + 
              companyvolvo + carbodywagon + drivewheelrwd + cylindernumberfive + 
              cylindernumberfour + cylindernumbersix + fuelsystem2bbl + 
              fuelsystemmpfi, data = train)

summary(model_5)  # Multiple R-squared:  0.9751,	Adjusted R-squared:  0.9673

## check for multicollinearity an alternative to correlation analysis
vif(model_5)


# remove fuelsystemmpfi: it is not significant having p-value=0.164054> 0.05 and VIF=5.042133
model_6<-lm(formula = price ~ aspiration + enginelocation + 
              carwidth + curbweight + enginesize + stroke + peakrpm + 
              companybmw + companybuick + companychevrolet + 
              companydodge + companyhonda + companyisuzu + companyjaguar + 
              companymazda + companymercury + companymitsubishi + companynissan + 
              companypeugeot + companyplymouth + companyporsche + companyrenault + 
              companysaab + companysubaru + companytoyota + companyvolkswagen + 
              companyvolvo + carbodywagon + drivewheelrwd + cylindernumberfive + 
              cylindernumberfour + cylindernumbersix + fuelsystem2bbl, data = train)

summary(model_6)  # Multiple R-squared:  0.9746,	Adjusted R-squared:  0.967

## check for multicollinearity an alternative to correlation analysis
vif(model_6)

# remove fuelsystem2bbl: it is not significant p-value=0.196370    > 0.05 and VIF=3.350978
model_7<-lm(formula = price ~ aspiration + enginelocation + 
              carwidth + curbweight + enginesize + stroke + peakrpm + 
              companybmw + companybuick + companychevrolet + 
              companydodge + companyhonda + companyisuzu + companyjaguar + 
              companymazda + companymercury + companymitsubishi + companynissan + 
              companypeugeot + companyplymouth + companyporsche + companyrenault + 
              companysaab + companysubaru + companytoyota + companyvolkswagen + 
              companyvolvo + carbodywagon + drivewheelrwd + cylindernumberfive + 
              cylindernumberfour + cylindernumbersix, data = train)

summary(model_7)  # Multiple R-squared:  0.9742,	Adjusted R-squared:  0.9668

## check for multicollinearity an alternative to correlation analysis
vif(model_7)

# remove companyvolvo: it is not significant having p-value=0.062366 > 0.05 and VIF=3.679009
model_8<-lm(formula = price ~ aspiration + enginelocation + 
              carwidth + curbweight + enginesize + stroke + peakrpm + 
              companybmw + companybuick + companychevrolet + 
              companydodge + companyhonda + companyisuzu + companyjaguar + 
              companymazda + companymercury + companymitsubishi + companynissan + 
              companypeugeot + companyplymouth + companyporsche + companyrenault + 
              companysaab + companysubaru + companytoyota + companyvolkswagen + 
              carbodywagon + drivewheelrwd + cylindernumberfive + 
              cylindernumberfour + cylindernumbersix, data = train)

summary(model_8)  # Multiple R-squared:  0.9734,	Adjusted R-squared:  0.966

## check for multicollinearity an alternative to correlation analysis
vif(model_8)

# remove companyporsche: it is not significant having p-value=0.240488 > 0.05 and VIF=6.937892
model_9<-lm(formula = price ~ aspiration + enginelocation + 
              carwidth + curbweight + enginesize + stroke + peakrpm + 
              companybmw + companybuick + companychevrolet + 
              companydodge + companyhonda + companyisuzu + companyjaguar + 
              companymazda + companymercury + companymitsubishi + companynissan + 
              companypeugeot + companyplymouth + companyrenault + 
              companysaab + companysubaru + companytoyota + companyvolkswagen + 
              carbodywagon + drivewheelrwd + cylindernumberfive + 
              cylindernumberfour + cylindernumbersix, data = train)

summary(model_9)  # Multiple R-squared:  0.9731,	Adjusted R-squared:  0.9659

## check for multicollinearity an alternative to correlation analysis
vif(model_9)

# remove curbweight: VIF=20.407845
model_10<-lm(formula = price ~ aspiration + enginelocation + 
              carwidth + enginesize + stroke + peakrpm + 
              companybmw + companybuick + companychevrolet + 
              companydodge + companyhonda + companyisuzu + companyjaguar + 
              companymazda + companymercury + companymitsubishi + companynissan + 
              companypeugeot + companyplymouth + companyrenault + 
              companysaab + companysubaru + companytoyota + companyvolkswagen + 
              carbodywagon + drivewheelrwd + cylindernumberfive + 
              cylindernumberfour + cylindernumbersix, data = train)

summary(model_10)  # Multiple R-squared:  0.9701,	Adjusted R-squared:  0.9625

## check for multicollinearity an alternative to correlation analysis
vif(model_10)

# remove stroke: VIF=3.985262
model_11<-lm(formula = price ~ aspiration + enginelocation + 
               carwidth + enginesize + peakrpm + 
               companybmw + companybuick + companychevrolet + 
               companydodge + companyhonda + companyisuzu + companyjaguar + 
               companymazda + companymercury + companymitsubishi + companynissan + 
               companypeugeot + companyplymouth + companyrenault + 
               companysaab + companysubaru + companytoyota + companyvolkswagen + 
               carbodywagon + drivewheelrwd + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix, data = train)

summary(model_11)  # Multiple R-squared:  0.9687,	Adjusted R-squared:  0.961

## check for multicollinearity an alternative to correlation analysis
vif(model_11)

# remove peakrpm: VIF=2.970673
model_12<-lm(formula = price ~ aspiration + enginelocation + 
               carwidth + enginesize +
               companybmw + companybuick + companychevrolet + 
               companydodge + companyhonda + companyisuzu + companyjaguar + 
               companymazda + companymercury + companymitsubishi + companynissan + 
               companypeugeot + companyplymouth + companyrenault + 
               companysaab + companysubaru + companytoyota + companyvolkswagen + 
               carbodywagon + drivewheelrwd + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix, data = train)

summary(model_12)  # Multiple R-squared:  0.9661,	Adjusted R-squared:  0.9582

## check for multicollinearity an alternative to correlation analysis
vif(model_12)

# remove drivewheelrwd: VIF=3.493607
model_13<-lm(formula = price ~ aspiration + enginelocation + 
               carwidth + enginesize +
               companybmw + companybuick + companychevrolet + 
               companydodge + companyhonda + companyisuzu + companyjaguar + 
               companymazda + companymercury + companymitsubishi + companynissan + 
               companypeugeot + companyplymouth + companyrenault + 
               companysaab + companysubaru + companytoyota + companyvolkswagen + 
               carbodywagon + cylindernumberfive + 
               cylindernumberfour + cylindernumbersix, data = train)

summary(model_13)  # Multiple R-squared:  0.9651,	Adjusted R-squared:  0.9573

## check for multicollinearity an alternative to correlation analysis
vif(model_13)

# remove carbodywagon: it is not significant having p-value=0.637704> 0.05
model_14<-lm(formula = price ~ aspiration + enginelocation + 
               carwidth + enginesize +
               companybmw + companybuick + companychevrolet + 
               companydodge + companyhonda + companyisuzu + companyjaguar + 
               companymazda + companymercury + companymitsubishi + companynissan + 
               companypeugeot + companyplymouth + companyrenault + 
               companysaab + companysubaru + companytoyota + companyvolkswagen + 
               cylindernumberfive + 
               cylindernumberfour + cylindernumbersix, data = train)

summary(model_14)  # Multiple R-squared:  0.9651,	Adjusted R-squared:  0.9576

## check for multicollinearity an alternative to correlation analysis
vif(model_14)

# remove cylindernumberfour: VIF=7.302923
model_15<-lm(formula = price ~ aspiration + enginelocation + 
               carwidth + enginesize +
               companybmw + companybuick + companychevrolet + 
               companydodge + companyhonda + companyisuzu + companyjaguar + 
               companymazda + companymercury + companymitsubishi + companynissan + 
               companypeugeot + companyplymouth + companyrenault + 
               companysaab + companysubaru + companytoyota + companyvolkswagen + 
               cylindernumberfive + cylindernumbersix, data = train)

summary(model_15)  # Multiple R-squared:  0.9447,	Adjusted R-squared:  0.9334

## check for multicollinearity an alternative to correlation analysis
vif(model_15)

# remove enginesize: VIF=7.934179
model_16<-lm(formula = price ~ aspiration + enginelocation + 
               carwidth +
               companybmw + companybuick + companychevrolet + 
               companydodge + companyhonda + companyisuzu + companyjaguar + 
               companymazda + companymercury + companymitsubishi + companynissan + 
               companypeugeot + companyplymouth + companyrenault + 
               companysaab + companysubaru + companytoyota + companyvolkswagen + 
               cylindernumberfive + cylindernumbersix, data = train)

summary(model_16)  # Multiple R-squared:  0.9398,	Adjusted R-squared:  0.9282

## check for multicollinearity an alternative to correlation analysis
vif(model_16)

# remove companychevrolet:  it is not significant having p-value=0.939836 > 0.05
model_17<-lm(formula = price ~ aspiration + enginelocation + 
               carwidth +
               companybmw + companybuick +
               companydodge + companyhonda + companyisuzu + companyjaguar + 
               companymazda + companymercury + companymitsubishi + companynissan + 
               companypeugeot + companyplymouth + companyrenault + 
               companysaab + companysubaru + companytoyota + companyvolkswagen + 
               cylindernumberfive + cylindernumbersix, data = train)

summary(model_17)  # Multiple R-squared:  0.9398,	Adjusted R-squared:  0.9288

## check for multicollinearity an alternative to correlation analysis
vif(model_17)

# remove companyisuzu:  it is not significant having p-value=0.666092 > 0.05
model_18<-lm(formula = price ~ aspiration + enginelocation + 
               carwidth +
               companybmw + companybuick +
               companydodge + companyhonda + companyjaguar + 
               companymazda + companymercury + companymitsubishi + companynissan + 
               companypeugeot + companyplymouth + companyrenault + 
               companysaab + companysubaru + companytoyota + companyvolkswagen + 
               cylindernumberfive + cylindernumbersix, data = train)

summary(model_18)  # Multiple R-squared:  0.9397,	Adjusted R-squared:  0.9293

## check for multicollinearity an alternative to correlation analysis
vif(model_18)

# remove companysaab:  it is not significant having p-value=0.406498 > 0.05
model_19<-lm(formula = price ~ aspiration + enginelocation + 
               carwidth +
               companybmw + companybuick +
               companydodge + companyhonda + companyjaguar + 
               companymazda + companymercury + companymitsubishi + companynissan + 
               companypeugeot + companyplymouth + companyrenault + 
               companysubaru + companytoyota + companyvolkswagen + 
               cylindernumberfive + cylindernumbersix, data = train)

summary(model_19)  # Multiple R-squared:  0.9394,	Adjusted R-squared:  0.9295

## check for multicollinearity an alternative to correlation analysis
vif(model_19)

# remove companymercury:  it is not significant having p-value=0.138498 > 0.05
model_20<-lm(formula = price ~ aspiration + enginelocation + 
               carwidth +
               companybmw + companybuick +
               companydodge + companyhonda + companyjaguar + 
               companymazda + companymitsubishi + companynissan + 
               companypeugeot + companyplymouth + companyrenault + 
               companysubaru + companytoyota + companyvolkswagen + 
               cylindernumberfive + cylindernumbersix, data = train)

summary(model_20)  # Multiple R-squared:  0.9383,	Adjusted R-squared:  0.9288

## check for multicollinearity an alternative to correlation analysis
vif(model_20)

# remove aspiration:  it is not significant having p-value=0.253778 > 0.05
model_21<-lm(formula = price ~ carwidth + enginelocation + 
               companybmw + companybuick +
               companydodge + companyhonda + companyjaguar + 
               companymazda + companymitsubishi + companynissan + 
               companypeugeot + companyplymouth + companyrenault + 
               companysubaru + companytoyota + companyvolkswagen + 
               cylindernumberfive + cylindernumbersix, data = train)

summary(model_21)  # Multiple R-squared:  0.9376,	Adjusted R-squared:  0.9286

## check for multicollinearity an alternative to correlation analysis
vif(model_21)

# remove companydodge:  it is not significant having p-value=0.001720  > 0.05
model_22<-lm(formula = price ~ carwidth + enginelocation + 
               companybmw + companybuick + companyhonda + companyjaguar + 
               companymazda + companymitsubishi + companynissan + 
               companypeugeot + companyplymouth + companyrenault + 
               companysubaru + companytoyota + companyvolkswagen + 
               cylindernumberfive + cylindernumbersix, data = train)

summary(model_22)  # Multiple R-squared:  0.9325,	Adjusted R-squared:  0.9233

## check for multicollinearity an alternative to correlation analysis
vif(model_22)

# remove cylindernumbersix:  it is not significant having p-value=0.001869 > 0.05
model_23<-lm(formula = price ~ carwidth + enginelocation + 
               companybmw + companybuick + companyhonda + companyjaguar + 
               companymazda + companymitsubishi + companynissan + 
               companypeugeot + companyplymouth + companyrenault + 
               companysubaru + companytoyota + companyvolkswagen + 
               cylindernumberfive, data = train)

summary(model_23)  # Multiple R-squared:  0.927,	Adjusted R-squared:  0.9177

## check for multicollinearity an alternative to correlation analysis
vif(model_23)

# remove companyplymouth:  it is not significant having p-value=0.003902 > 0.05
model_24<-lm(formula = price ~ carwidth + enginelocation + 
               companybmw + companybuick + companyhonda + companyjaguar + 
               companymazda + companymitsubishi + companynissan + 
               companypeugeot + companyrenault + 
               companysubaru + companytoyota + companyvolkswagen + 
               cylindernumberfive, data = train)

summary(model_24)  # Multiple R-squared:  0.922,	Adjusted R-squared:  0.9128

## check for multicollinearity an alternative to correlation analysis
vif(model_24)

# remove companynissan:  it is not significant having p-value=0.034854 > 0.05
model_25<-lm(formula = price ~ carwidth + enginelocation + 
               companybmw + companybuick + companyhonda + companyjaguar + 
               companymazda + companymitsubishi +  
               companypeugeot + companyrenault + 
               companysubaru + companytoyota + companyvolkswagen + 
               cylindernumberfive, data = train)

summary(model_25)  # Multiple R-squared:  0.9192,	Adjusted R-squared:  0.9104

## check for multicollinearity an alternative to correlation analysis
vif(model_25)

# remove companyhonda:  it is not significant having p-value=0.054056  > 0.05
model_26<-lm(formula = price ~ carwidth + enginelocation + 
               companybmw + companybuick + companyjaguar + 
               companymazda + companymitsubishi +  
               companypeugeot + companyrenault + 
               companysubaru + companytoyota + companyvolkswagen + 
               cylindernumberfive, data = train)

summary(model_26)  # Multiple R-squared:  0.9168,	Adjusted R-squared:  0.9084

## check for multicollinearity an alternative to correlation analysis
vif(model_26)

# remove companytoyota:  it is not significant having p-value=0.031756 > 0.05
model_27<-lm(formula = price ~ carwidth + enginelocation + 
               companybmw + companybuick + companyjaguar + 
               companymazda + companymitsubishi +  
               companypeugeot + companyrenault + 
               companysubaru + companyvolkswagen + 
               cylindernumberfive, data = train)

summary(model_27)  # Multiple R-squared:  0.9138,	Adjusted R-squared:  0.9058

## check for multicollinearity an alternative to correlation analysis
vif(model_27)

# remove companysubaru:  it is not significant having p-value=0.061236 > 0.05
model_28<-lm(formula = price ~ carwidth + enginelocation + 
               companybmw + companybuick + companyjaguar + 
               companymazda + companymitsubishi +  
               companypeugeot + companyrenault + companyvolkswagen + 
               cylindernumberfive, data = train)

summary(model_28)  # Multiple R-squared:  0.9114,	Adjusted R-squared:  0.904

## check for multicollinearity an alternative to correlation analysis
vif(model_28)

# remove companyvolkswagen:  it is not significant p-value=0.05569    > 0.05
model_29<-lm(formula = price ~ carwidth + enginelocation + 
               companybmw + companybuick + companyjaguar + 
               companymazda + companymitsubishi +  
               companypeugeot + companyrenault + 
               cylindernumberfive, data = train)

summary(model_29)  # Multiple R-squared:  0.9089,	Adjusted R-squared:  0.902

## check for multicollinearity an alternative to correlation analysis
vif(model_29)

# remove companymitsubishi:  it is not significant having p-value=0.03336  > 0.05
model_30<-lm(formula = price ~ carwidth + enginelocation + 
               companybmw + companybuick + companyjaguar + 
               companymazda + 
               companypeugeot + companyrenault + 
               cylindernumberfive, data = train)

summary(model_30)  # Multiple R-squared:  0.9057,	Adjusted R-squared:  0.8993

## check for multicollinearity an alternative to correlation analysis
vif(model_30)

# remove companyrenault:  it is not significant having p-value=0.02133  > 0.05
model_31<-lm(formula = price ~ carwidth + enginelocation + 
               companybmw + companybuick + companyjaguar + 
               companymazda + 
               companypeugeot + 
               cylindernumberfive, data = train)

summary(model_31)  # Multiple R-squared:  0.9019,	Adjusted R-squared:  0.896

## check for multicollinearity an alternative to correlation analysis
vif(model_31)

# remove companymazda:  it is not significant having p-value=0.02290   > 0.05
model_32<-lm(formula = price ~ carwidth + enginelocation + 
               companybmw + companybuick + companyjaguar +  
               companypeugeot + 
               cylindernumberfive, data = train)

summary(model_32)  # Multiple R-squared:  0.898,	Adjusted R-squared:  0.8927

## check for multicollinearity an alternative to correlation analysis
vif(model_32)

# remove cylindernumberfive:  it is not significant p-value=0.0156    > 0.05
model_33<-lm(formula = price ~ carwidth + enginelocation + 
               companybmw + companybuick + companyjaguar +  
               companypeugeot, data = train)

summary(model_33)  # Multiple R-squared:  0.8934,	Adjusted R-squared:  0.8887

## check for multicollinearity an alternative to correlation analysis
vif(model_33)

# remove companypeugeot:  it is not significant having p-value=0.0243  > 0.05
model_34<-lm(formula = price ~ carwidth + enginelocation + 
               companybmw + companybuick + companyjaguar, data = train)

summary(model_34)  # Multiple R-squared:  0.8894,	Adjusted R-squared:  0.8853

## check for multicollinearity an alternative to correlation analysis
vif(model_34)

## ------------------Accessing the Model----------------------
# Predict the car prices in the testing dataset
test$test_price <- predict(model_34,test[,-19])  #also removing the price variable as we will use the test_price(price of train dataset)
actual_pred_corr<- cor(test$price, test$test_price , use = "everything")
actual_pred_corr
# [1] 0.9055584

# Checking accuracy of the predictions( checking r^2 value)
# Calculate correlation
r <- cor(test$price,test$test_price)

# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2
# check R-squared
rsquared
# [1] 0.8200359
# This implies that the model can explain 82% variablility of the dependent variable.

# calculate error for Actual-Predicted Price
test$error_pred<- test$price - test$test_price

#Create a carid variable to assign a random unique ID to each row in the dataset  
test$carid<- sample(1:nrow(test), nrow(test), replace = FALSE)
train$carid<- sample(1:nrow(train), nrow(train), replace = FALSE)

#Plotting the Error in Prediction versus the carid. This is to show the randomness in the prediction error generated.
error_noise<- ggplot(test, aes(x=test$carid, y=test$error_pred))+geom_point()+ xlab("Car ID")+ ylab("Predicted Price Error") + ggtitle("Fig 25. Plot Showing Random distribution of Predicted Error")
error_noise
# Since no patterns are formed in the above plot, it suggests that there are no variables that can be added to the model
# And can be connsidered just a white noise.
# Hence model_34 can be taken as the final model

# Plot actual vs predicted car price

Actual_vs_Pred_price<-ggplot()+
  geom_line(data=test,aes(y=test$price,x=test$carid, color = "Actual Price"),size=0.5 )+
  geom_line(data=test,aes(y=test$test_price,x=test$carid, color = "Predicted Price"),size=0.5) + 
  scale_color_manual(name = "Actual vs Predicted Price", values = c("Actual Price" = "turquoise", "Predicted Price" = "tan1")) +
  ggtitle("Fig 26. Plot Showing Random distribution of actual and predicted car price")+ labs(fill=test$test_price)
Actual_vs_Pred_price

##---------------Inferences derived and recommendations for the company--------------------
# Hence we infer from the final model that Driver factors are:
# 1. carwidth
# 2. enginelocation
# 3. companybmw
# 4. companybuick
# 5. companyjaguar

# Recommendations for the automobile company Geely Auto are :
# The US automobile market seems to be strongly influenced by 3 brands, BMW, BUICK & JAGUAR
# Apart from the brands, carwidth and engine location play a significant role when it comes to pricing.
