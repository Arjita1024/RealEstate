---
Title: "Real Estate Price Prediction"
Author: "Executed by Arjita Srivastava"
---
  
##Problem Statement:
Price of a property is one of the most important decision criterion when people buy homes. Real state firms need to be consistent in their pricing in order to attract buyers . Having a predictive model for the same will be great tool to have , which in turn can also be used to tweak development of properties , putting more emphasis on qualities which increase the value of the property.

##Aim:
To Build a machine learning predictive model and predict the accurate prices of the proterties.

The evalution metric will be RMSE.

##Data:
There exist two datasets, housing_train.csv and housing_test.csv . We will use data housing_train to build predictive model for response variable "Price". Housing_test data contains all other factors except "Price" which we can use for testing purpose.

##Data dictionary:
###Variables : Type ::   Definition

Suburb : categorical :: Which subsurb the property is located in

Address : categorical :: short address

Rooms : numeric :: Number of Rooms

Type : categorical :: type of the property

*Price : numeric :: This is the target variable, price of the property *
  
Method : categorical :: method for selling 

SellerG : categorical :: Name of the seller 

Distance : numeric :: distance from the city center

Postcode : categorical :: postcode of the property

Bedroom2 : Numeric :: numbers of secondary bedrooms (this is different from rooms)

Bathroom : numeric :: number of bathrooms

Car : numeric :: number of parking spaces

Landsize : numeric :: landsize

BuildingArea : numeric :: buildup area

YearBuilt : numeric :: year of building 

CouncilArea : numeric :: council area to which the propery belongs

##Methodology:
We will build a linear regression model to predict the response variable "Price" 

1.Imputing NA values in the datasets.

2.Data Preparation.

3.Model Building.

4.Perfomance measurement of the model.

5:Predicting Real Estate Prices for the final Test Dataset.


##Code -



# Setting of Directories
setwd("C:\\Users\\lenovo\\Documents\\Edvancer_Projects\\Real_Estate Project")
getwd()

# Reading train and test datasets:

raw_df=read.csv("housing_train.csv",stringsAsFactors = FALSE,header = T )
df=raw_df
glimpse(df)
# 7536 obs,16 variables

raw_dftest=read.csv("housing_test.csv",stringsAsFactors = FALSE,header = T )
dftest=raw_dftest
glimpse(dftest)
# 1885 obs,15 variables

# Loading library ggplot
library(ggplot2)
ggplot(data=df) +
  geom_histogram(mapping=aes(x=Price))

# Overview of relationship between the variables - Scatterplot
pairs(~Price+Distance+Bathroom+Bedroom2+YearBuilt+Landsize, data = df)

# Loading library Hmisc - Descriptive stats of all the variables
library(Hmisc)
describe(df)

# Checking NA values of all variables
apply(df,2,function(x)sum(is.na(x)))

# Start removing NA values of all variables both in test and train data individually
df$Bedroom2[is.na(df$Bedroom2)]=median(df$Bedroom2,na.rm = T)
df$Bathroom[is.na(df$Bathroom)]=round(mean(df$Bathroom,na.rm = T),0)
df$Car[is.na(df$Car)]=median(df$Car,na.rm = T)
df$Landsize[is.na(df$Landsize)]=round(mean(df$Landsize,na.rm = T),0)
df$BuildingArea[is.na(df$BuildingArea)]=round(mean(df$BuildingArea,na.rm = T),0)
df$YearBuilt[is.na(df$YearBuilt)]=mean(df$YearBuilt,na.rm = T)
apply(df,2,function(x)sum(is.na(x))) # No more variables have NA as a value
summary(df)
summary(dftest)
apply(dftest,2,function(x)sum(is.na(x)))
dftest$Bedroom2[is.na(dftest$Bedroom2)]=median(dftest$Bedroom2,na.rm = T)
dftest$Bathroom[is.na(dftest$Bathroom)]=round(mean(dftest$Bathroom,na.rm = T),0)
dftest$Car[is.na(dftest$Car)]=median(dftest$Car,na.rm = T)
dftest$Landsize[is.na(dftest$Landsize)]=round(mean(dftest$Landsize,na.rm = T),0)
dftest$BuildingArea[is.na(dftest$BuildingArea)]=mean(dftest$BuildingArea,na.rm = T)
dftest$YearBuilt[is.na(dftest$YearBuilt)]=mean(dftest$YearBuilt,na.rm = T)
apply(df,2,function(x)sum(is.na(x)))

# Creating a new column Price and inputting it with NA value in Test data
dftest$Price = NA

# Test data has got another column as Price with NA values
dftest

# Combining Test and Train Data


all_data=rbind(df,dftest)

apply(all_data,2,function(x)sum(is.na(x)))

# Loading Library dplyr
library(dplyr)

# Creating dummies based on mean price for particular suburbs. 
# Mean price for the suburbs are noted and then suburbs are clubbed based on similar prices
t=table(all_data$Suburb)
t
t1=round(tapply(all_data$Price,all_data$Suburb,mean,na.rm=T),0)
t1
View(t1)
t1=sort(t1)
t1
all_data=all_data %>% 
  mutate(
    sub_1=as.numeric(Suburb%in%c("Campbellfield","Jacana")),
    sub_2=as.numeric(Suburb%in%c("Kealba","Brooklyn","Albion","Sunshine West","Ripponlea","Fawkner")),
    sub_3=as.numeric(Suburb%in%c("Glenroy","Southbank","Sunshine North","Keilor Park","Heidelberg West","Reservoir","Braybrook","Kingsbury","Gowanbrae","Hadfield","Watsonia","Footscray","South Kingsville","Balaclava","Melbourne","Maidstone","Sunshine")),
    sub_4=as.numeric(Suburb%in%c("Airport West","Heidelberg Heights","Pascoe Vale","West Footscray","Altona North","Williamstown North","Brunswick West","Keilor East","Oak Park","Maribyrnong","Altona","Flemington","Coburg North","Yallambie","Avondale Heights","Bellfield")),
    sub_5=as.numeric(Suburb%in%c("Strathmore Heights","Glen Huntly","Kensington","Essendon North","St Kilda","Preston","North Melbourne","Coburg","Kingsville","Collingwood","Brunswick East","Gardenvale","Thornbury","Niddrie","West Melbourne","Viewbank")),
    sub_6=as.numeric(Suburb%in%c("Spotswood","Carnegie","Elwood","Heidelberg","Moorabbin","Oakleigh","Rosanna","Docklands","Yarraville","Cremorne","Seddon","Brunswick","Oakleigh South","Ascot Vale","Windsor","Caulfield","Essendon West","Newport")),
    sub_7=as.numeric(Suburb%in%c("Chadstone","South Yarra","Essendon","Bentleigh East","Murrumbeena","Hughesdale","Fairfield","Ashwood","Clifton Hill","Caulfield North","Abbotsford","Carlton","Prahran","Fitzroy","Ivanhoe","Hampton East","Caulfield East")),
    sub_8=as.numeric(Suburb%in%c("Richmond","Travancore","Templestowe Lower","Ormond","Caulfield South","Moonee Ponds","Hawthorn","Box Hill","Bulleen","Burnley","Burwood","Strathmore","Port Melbourne","Fitzroy North","Alphington")),
    sub_9=as.numeric(Suburb%in%c("Doncaster","South Melbourne","Northcote","Aberfeldie","Elsternwick","Bentleigh","Kooyong","Parkville")),
    sub_10=as.numeric(Suburb%in%c("Williamstown","East Melbourne","Seaholme")),
    sub_11=as.numeric(Suburb%in%c("Malvern East","Carlton North","Hawthorn East","Surrey Hills")),
    sub_12=as.numeric(Suburb%in%c("Princes Hill","Mont Albert","Armadale","Kew East","Glen Iris","Ashburton")),
    sub_13=as.numeric(Suburb%in%c("Brighton East","Eaglemont","Hampton")),
    sub_14=as.numeric(Suburb%in%c("Toorak","Ivanhoe East","Camberwell","Balwyn North","Kew")),
    sub_15=as.numeric(Suburb%in%c("Brighton","Middle Park")),
    sub_16=as.numeric(Suburb%in%c("Balwyn","Albert Park","Malvern"))
  ) %>% 
  select(-Suburb) # Dropping variable 'Suburb' from the data since we have already created dummies
glimpse(all_data)


# Dropping variable Address as it is unique
all_data=all_data %>% 
  select(-Address)

# Dropping variable Postcode as it is correlated
all_data=all_data %>% 
  select(-Postcode)

# Creating dummies for variable 'Type':2 (Third dummy will be automatically considered)
table(all_data$Type)
all_data=all_data %>%
  mutate(Type_t=as.numeric(Type=="t"),
         type_u=as.numeric(Type=="u"))
all_data=all_data %>% 
  select(-Type) # Dropping variable 'Type' from the data since we have already created dummies
glimpse(all_data)

# Creating dummies for variable 'Method':4 and drop variable 'Method'
table(all_data$Method)
all_data=all_data %>% 
  mutate(Method_PI=as.numeric(Method=="PI"),
         Method_SA=as.numeric(Method=="SA"),
         Method_SP=as.numeric(Method=="SP"),
         Method_VB=as.numeric(Method=="VB")) %>% 
  select(-Method)
glimpse(all_data)

# Creating dummies for variable 'SellerG' and dropping it
t=table(all_data$SellerG)
sort(t)
all_data=all_data %>%
  mutate(Gnelson=as.numeric(SellerG=="Nelson"),
         GJellis=as.numeric(SellerG=="Jellis"),
         Ghstuart=as.numeric(SellerG=="hockingstuart"),
         Gbarry=as.numeric(SellerG=="Barry"),
         GMarshall=as.numeric(SellerG=="Marshall"),
         GWoodards=as.numeric(SellerG=="Woodards"),
         GBrad=as.numeric(SellerG=="Brad"),
         GBiggin=as.numeric(SellerG=="Biggin"),
         GRay=as.numeric(SellerG=="Ray"),
         GFletchers=as.numeric(SellerG=="Fletchers"),
         GRT=as.numeric(SellerG=="RT"),
         GSweeney=as.numeric(SellerG=="Sweeney"),
         GGreg=as.numeric(SellerG=="Greg"),
         GNoel=as.numeric(SellerG=="Noel"),
         GGary=as.numeric(SellerG=="Gary"),
         GJas=as.numeric(SellerG=="Jas"),
         GMiles=as.numeric(SellerG=="Miles"),
         GMcGrath=as.numeric(SellerG=="McGrath"),
         GHodges=as.numeric(SellerG=="Hodges"),
         GKay=as.numeric(SellerG=="Kay"),
         GStockdale=as.numeric(SellerG=="Stockdale"),
         GLove=as.numeric(SellerG=="Love"),
         GDouglas=as.numeric(SellerG=="Douglas"),
         GWilliams=as.numeric(SellerG=="Williams"),
         GVillage=as.numeric(SellerG=="Village"),
         GRaine=as.numeric(SellerG=="Raine"),
         GRendina=as.numeric(SellerG=="Rendina"),
         GChisholm=as.numeric(SellerG=="Chisholm"),
         GCollins=as.numeric(SellerG=="Collins"),
         GLITTLE=as.numeric(SellerG=="LITTLE"),
         GNick=as.numeric(SellerG=="Nick"),
         GHarcourts=as.numeric(SellerG=="Harcourts"),
         GCayzer=as.numeric(SellerG=="Cayzer"),
         GMoonee=as.numeric(SellerG=="Moonee"),
         GYPA=as.numeric(SellerG=="YPA")) %>% 
  select(-SellerG)
glimpse(all_data)

# Creating dummies for variable 'Council Area' and dropping it
table(all_data$CouncilArea)
all_data=all_data %>% 
  mutate(CA_Banyule=as.numeric(CouncilArea=="Banyule"),
         CA_Bayside=as.numeric(CouncilArea=="Bayside"),
         CA_Boroondara=as.numeric(CouncilArea=="Boroondara"),
         CA_Brimbank=as.numeric(CouncilArea=="Brimbank"),
         CA_Darebin=as.numeric(CouncilArea=="Darebin"),
         CA_Glen_Eira=as.numeric(CouncilArea=="Glen Eira"),
         CA_Hobsons_Bay=as.numeric(CouncilArea=="Hobsons Bay"),
         CA_Hume=as.numeric(CouncilArea=="Hume"),
         CA_Kingston=as.numeric(CouncilArea=="Kingston"),
         CA_Manningha=as.numeric(CouncilArea=="Manningha"),
         CA_Maribyrnong=as.numeric(CouncilArea=="Maribyrnong"),
         CA_Melbourne=as.numeric(CouncilArea=="Melbourne"),
         CA_Monash=as.numeric(CouncilArea=="Monash"),
         CA_Moonee_Valley=as.numeric(CouncilArea=="Moonee Valley"),
         CA_Moreland=as.numeric(CouncilArea=="Moreland"),
         CA_Port_Phillip=as.numeric(CouncilArea=="Port Phillip"),
         CA_Stonnington=as.numeric(CouncilArea=="Stonnington"),
         CA_Whitehorse=as.numeric(CouncilArea=="Whitehorse"),
         CA_Yarra=as.numeric(CouncilArea=="Yarra")) %>% 
  select(-CouncilArea)
glimpse(all_data)

# Separating Train and Test Data
Final_Train=all_data[!is.na(all_data$Price),]

# 7536 observations and 85 variables
glimpse(Final_Train) 
Final_Test=all_data[is.na(all_data$Price),]
glimpse(Final_Test)

# Remove Price from Test Data
Final_Test=Final_Test %>% 
  select(-Price)

# 1885 observations and 84 variables
glimpse(Final_Test)

# Divide the Final_Train dataset in the ratio 75:25
s=sample(1:nrow(Final_Train),0.75*nrow(Final_Train))
s
Final_Train_75=Final_Train[s,]
Final_Test_25=Final_Train[-s,]

# Model Building
# We will use Final_Train_75 for linear regression model building 
# and use Final_Train_25 to test the performance of the model thus built.


library(car)
LinReg_Final_Train_75=lm(Price~.,data = Final_Train_75)
summary(LinReg_Final_Train_75)

library(Hmisc)
describe(Final_Train_75$Price)

# Removing outliers
Final_Train_75 = Final_Train_75[!(Final_Train_75$Price>4000000),]

# Applying Linear Regression Model
LinReg_Final_Train_75 = lm(Price~., data = Final_Train_75)
summary(LinReg_Final_Train_75)

# Validating on 25% data
predict(LinReg_Final_Train_75,Final_Test_25,se.fit = T)
predicted=predict(LinReg_Final_Train_75, newdata = Final_Test_25)
predicted=round(predicted,1)

# Actual Value-Predicted Value
Result=Final_Test_25$Price-predicted
RMSE_Test=sqrt(mean(Result^2,na.rm = T))
RMSE_Test

# Applying the model built on Test Data
predicted=predict(LinReg_Final_Train_75,newdata = Final_Test)
predicted=round(predicted,1)
class(predicted)
Final_Test$Predicted_Price=predicted
Final_Test

# Minimum train price is considered for noted negative price in the predicted final prices
Final_Test$Predicted_Price[Final_Test$Predicted_Price<0]=85000
ggplot(data = Final_Test)+
   geom_histogram(mapping = aes(x=Predicted_Price))

describe(Final_Test$Predicted_Price)
class(Final_Test)

212467/RMSE_Test

write.csv(predicted, "Arjita_Srivastava_P1_part2.csv")

summary(LinReg_Final_Train_75)

# The passing criteria mentioned,was to have value >0.5 which we have successfuly crossed.Hence our model is good.
# 0.5905041

# Conclusion:

# *Real estate price prediction was done successfully using linear regression model 
# having Adjusted R-square:0.7071.*

var(df$Price)
sum(is.na(df$YearBuilt))


