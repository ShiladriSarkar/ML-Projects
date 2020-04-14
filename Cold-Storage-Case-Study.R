##############################Cold Storage Case Study#########################################
##Problem 1

### Setting Working Directory.
setwd("E:/BABI Study Materials/Project1-Cold-Storage-CaseStudy")


### Readding/importing Cold_Storage_Temp_Data.csv file in R.
colddata = read.csv("Cold_Storage_Temp_Data.csv")

############# Descriptive Statistics#############################################################
### Verifying Columns names of colddata dataset
names(colddata) ## There are four Columns

###Verifying Structure of data
str(colddata)
## Colddat is a data frame with 365 Obesrvvation and 4 Variables
## Varaible Season and MOnth are factors(Catagorical Variable)
## Vraibles Date is integer and Temperature is numeric

### Verify Data summary
summary(colddata)
## There are three Season Rainy, Summer, Winter with 122,120 and 123 days respectively.
summary(colddata$Month) ## There are twelve months with February is 28 (Not Leap Year).
## Date ranges between 1 to 31 as usual.(NOrmally Distributed)
## Temperature varies between 1.700 to 5.000 with Mean 2.963 and Median 2.900.(Normally Distributed)

### Dimention of dataset
dim(colddata) ## 365 by 4

### Checkig for null values 
anyNA(colddata) ## There are no null values in the dataset
sum(is.na(colddata))


### Box Plot of Temperature To check for outliers
library(ggplot2)  ##liading ggplot2 package to R session

ggplot(colddata, aes(x = 1, y = colddata$Temperature)) + geom_boxplot(fill = "orange",col = "Blue") +
  ylab("Temperature") + ggtitle("BoxPLot of Temperatue with outlier value") +
  stat_summary(aes(label = round(stat(y),1)),color="red",geom = "text", fun.y = function(y){
    o <- boxplot.stats(y)$out; if (length(o) == 0) NA else o
  },hjust = -1) + coord_flip()

boxplot(colddata$Temperature,plot = F)$out

## we have two outliers in Temparature column (values 5 and 5)

### BoxPLot of Temperature at Season Level

ggplot(colddata, aes(y = colddata$Temperature, x = colddata$Season)) + geom_boxplot(fill = c("blue","brown","grey")) +
  xlab("Season") + ylab("Temperature") + ggtitle("Comperative BoxPlot SeasonVsTemperature") +
  stat_summary(aes(label = round(stat(y),1)),color="red",geom = "text", fun.y = function(y){
    o <- boxplot.stats(y)$out; if (length(o) == 0) NA else o
  },hjust = -1)

## This shows that we have outliers at Season level as well
boxplot(colddata$Temperature~colddata$Season)$out
## This shows if we brak the Temperatre there are total 5 outliers

###Barplot of Season with number of days.

ggplot(colddata, aes(x = colddata$Season,fill = colddata$Season)) + geom_bar(stat="count") +
  xlab("Season") + ylab("Number of Days") + ggtitle("Different season with number of days") + 
  geom_text(aes(label =..count..),stat = "count",vjust=1.6, color="white", size=5.5)
## All three season Almost equally distributed throughout the year.

### Density plot of Temperature and spread.
ggplot(colddata, aes(x = colddata$Temperature)) + geom_density(fill = "lightblue") + 
  ggtitle("Density plot of Temperature") + xlab("Temperature")
## We can see from this diagram that temperature is slightly right skued distributed.

### Question 1.Find mean cold storage temperature for Summer, Winter and Rainy Season#####
### We can Use grouup by using  dplyr also to generate Mean For all three season.
library(dplyr) ## Loading dplyr package to r session
colddata %>% group_by(Season = Season) %>% summarise(Mean = format(mean(Temperature),digits = 4))
## Mean of Rainy = 3.039, Summer = 3.153, Winter = 2.701 (round upto 3 decimal point)


###Question 2.Find overall mean for the full year################################
MeanTemp = mean(colddata$Temperature)
MeanTemp
## Overall mean for the full year is 2.962

###Question 3. Find Standard Deviation for the full year ################################
SDTemp = sd(colddata$Temperature)
SDTemp
## Standard Deviation for the full year is 0.509

### Question 4. Assume Normal distribution, 
###what is the probability of temperature having fallen below 2 deg C? #######################
poftemlt2c = pnorm(2,mean = MeanTemp, sd = SDTemp, lower.tail = T)
poftemlt2c # probability of temperature having fallen below 2 deg is 0.0291.

###Question 5. Assume Normal distribution, 
###what is the probability of temperature having gone above 4 deg C?##########################
poftemgt4c = pnorm(4,mean = MeanTemp, sd = SDTemp, lower.tail = F)
poftemgt4c
## probability of temperature having gone above 4 deg C is 0.0207.

### Question 6. What will be the penalty for the AMC Company?
## IN previous two example we saw that for a normal distribution probability of temperature
## going below 2 and above 4 degree C is the total probablity if Temperature < 2 + Temperature > 4

TotalPofPenalty = poftemlt2c + poftemgt4c
TotalPofPenalty

## Since the total probablility 0.0499 value is between 2.5% and 5% so the  
##  penalty for the AMC Company would be 10%.

###############################################################################################

### Problem 2 ################################################################################

### Reading the dataset Cold_Storage_Mar2018.csv

coldmarch = read.csv("Cold_Storage_Mar2018.csv")

### Basic Descrptive Analysis of Cold_Storage_Mar2018.csv dataset.
str(coldmarch)
summary(coldmarch)
anyNA(coldmarch)

### Question 1. State the Hypothesis, do the calculation using z tes.
### Hypothesis
## H0 Mu <= 3.9 -> No need for changes in Cold Storage Plan (Null Hypothesis)
## H1 Mu > 3.9 -> Problem in Cold Storage plant and need correction (Alternative Hypothesis)
### Calculation Using z test (Polulation Meean : SD(Standard Deviation) Known)
Smean = mean(coldmarch$Temperature) ## Sample Mean
Smean
## Here Mean Mu is Smean = 3.974286  and Standard Daviation calclate from prev SDTemp = 0.508589
## Population Mean given POPMean = 3.9 and Alpha = 0.1 and population size = 35
POPMean = 3.9 ## GIven in the Problem
Alpha = 0.1 ## Given in the problem
PopSize = 35 ## GIven in dataset Cold_Storage_Mar2018.csv
SDTemp  ## Standard deviation calculated from previous sample.

## Calculating z value
Zval = (Smean - POPMean) / (SDTemp/sqrt(35))
Zval
## Calculating z critical value as it is right of Mean we minus 1 from Alpha
Zcritical = qnorm(1-0.1)
Zcritical
## Zee value approach
## Zval < Zcritical
## Since Zval(z value is less than Z critical we can not reject the null hypothesis)

## Pvalue Approach, Calcluating pvalue using z value
Pval = 1- pnorm(Zval)
Pval
## Since Pval > Zval We can not reject the null hypothesis.(We failed to reject null hypothesis)
## So we can say that Temperature is maintained below permitable level 3.9 degree C

### Question 2. State the Hypothesis, do the calculation using t-test.

## H0 Mu <= 3.9 -> No need for changes in Cold Storage Plan (Null Hypothesis)
## H1 Mu > 3.9 -> Problem in Cold Storage plant and need correction (Alternative Hypothesis)
### Calculation Using z test (Polulation Meean : SD(Standard Deviation) Unknown)

POPMean ## GIven Upper limit (using as POP Mean)
Alpha = 0.1 ## Given in the problem
PopSize = 35 ## GIven in dataset Cold_Storage_Mar2018.csv
## Calculate Sample Standarad Deviation SamSD as known as sample error
SamSD = sd(coldmarch$Temperature) / sqrt(PopSize)
SamSD
## Usint t test
t.test(coldmarch$Temperature, mu = POPMean, alternative = "greater",conf.level = 0.90)
t.test(coldmarch$Temperature, mu = POPMean, alternative = "greater",conf.level = 0.99)
##  From Summary of test we can see that p value is very small 0.004711 which is less than 
## Alpha 0.1 so that we can ConClude that We reject NULL hypothesis.
## From this test we can say that roblem in Cold Storage plant and need corrective measure.


### Question 3. Give your inference after doing both the tests.

##	We received two completely different result using two test. 
##	Reason for this is we used two different Standard deviation, for Z we used SD from 2016 dataset and for T test we used 2018 sample dataset.
##	Since the issue reported from 2018, we can say that T test is more accurate because we used recent Sample Dataset.
##	The sample (35) which is given, we saw that all the days belongs to summer (In summary). that could be another reason that temperature going above 3.9.
###############################################################################################
