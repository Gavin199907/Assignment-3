######## Assignment 3

######## Q1. Read data ########
read.csv("/Users/apple/Desktop/R/price_index_Feb20201.csv")

###Q2. Use 4 methods to manipulate the dataset, write the code.#######

###### 1.Subesetting and sorting data #######
DF1<-read.csv("/Users/apple/Desktop/R/price_index_Feb20201.csv")

##### Clear blank lines
DF1 <- na.omit(DF1)

##### Quickly check some of the data
head(DF1,5) ####Select the first five rows of data
tail(DF1,5) ###Select data for the last five rows
DF1[1:10,"DATA_VAL"]
DF1[2,]

######### using ands / ors / which to find the target data

DF1[DF1$DATA_VAL>1050 & DF1$Series_title_1 =="Auckland",]   ####ands

DF1[DF1$DATA_VA>=1070 | DF1$Series_title_1 =="Wellington",]  #####ors

DF1[which(DF1$DATA_VA>1110),] #######which

#######sorting and ordering the data
sort(DF1$DATA_VAL) ######Sorted DATA_VAL from lowest to highest

sort(DF1$DATA_VAL,decreasing = T) #####Sorted DATA_VAL from highest to lowest

sort(DF1$DATA_VAL,na.last = T) ###Sort DATA_VAL from lowest to highest, and place NA at the end

DF1[order(DF1$DATA_VAL),] ######ordered DATA_VAL from lowest to highest

DF1[order(DF1$DATA_VAL,DF1$TIME_REF),]

###### ordering with plyr
library(plyr)
arrange(DF1,DATA_VAL) #####ordered DATA_VAL from lowest to highest

arrange(DF1,desc(DATA_VAL))  #####ordered DATA_VAL from highest to lowest

####### adding row and col 
set.seed(100)
DF1$number<-rnorm(1203) ######Add a column of 1206 random numbers
DF1

DF2<-rbind(DF1,c(1:11)) ######Add a row of numbers from 1 to 11
DF2



########### 2.Summarizing data ######
DF3<-read.csv("/Users/apple/Desktop/R/price_index_Feb20201.csv")

##### Clear blank lines
DF3<- na.omit(DF3)

####### Select data
head(DF3,n=3) #####Select the first 3 rows of data
tail(DF3)  #####Select the last 6 rows of data

###### summary
names(DF3) ######Determine the name of the data title

summary(DF3) ######Summary of the overall data

str(DF3) #####Returns data as a string of n characters

quantile((DF3$DATA_VAL)) ###Find the percentile ratio of DATA_VAL in the data

######## make table
table(DF3$DATA_VAL,useNA = "ifany")  ####Find the number of occurrences of DATA VAL in the data

table(DF3$DATA_VAL,DF3$Series_title_1) ####Find the number of occurrences of DATA VAL and serise 1 in the data

######check NA values
sum(is.na(DF3$DATA_VAL)) #####Check if data val has NA values

colSums((is.na(DF3))) #####Check for NA values in each column

all(colSums(is.na(DF3))==0) ####Determine if the entire data has NA values

######values with specific characteristics
table(DF3$DATA_VAL %in% c("1000")) ####Determine how many 1000s are in data val

DF3[DF3$DATA_VAL %in% c("1000"),] ####Find the column containing 1000 in data val

####### cross tabs
summary(DF3) ######Summary of the overall data

DE<-xtabs(~SER_REF+DATA_VAL,data=DF3) #####Forming a list of columns
DE

####### size of data set
object.size(DF3) ####View the amount of memory occupied by the data

print(object.size(DF3), units="Mb") #####View the size of memory occupied by a data object ( in MB unit )

######### 3.Creating new variables ######
install.packages("tidyr")
library(tidyr)
DF4<-read.csv("/Users/apple/Desktop/R/price_index_Feb20201.csv")

##### Clear blank lines
DF4<- na.omit(DF4)

####### creat data in a dataset
DF4<-data.frame(DF4,header=T) #####Indicates the name of the variable in the first row

head(DF4) ######Select the first five rows of data

dim(DF4) ###Retrieving or setting the size of data

#### subsetting variables
DF4$Lastone<-DF4$Series_title_1 %in% c("Auckland","Wellington") ###Determine if the elements of the previous
####### vector are in the latter vector and put the result in the last column

head(DF4) ####Select the first 6 rows of data

table(DF4$Lastone)  ###Find the number of occurrences of TURE in the last column of the data.

#######   Creating binary variables
DF4$DATAWrong=ifelse(DF4$DATA_VAL<1000,TRUE,FALSE) #####Determine if the number in data val is less than 1000  
######### and put the result in the last column


table(DF4$DATAWrong,DF4$DATA_VAL<1000) ####Display correct and incorrect in binary

######Creating categorical variables
DF4$values=cut(DF4$DATA_VAL,breaks=quantile(DF4$DATA_VAL)) ##Divide the number vectors by percentile ratio

table(DF4$values) ####Show results

######## Easy cutting
library(Hmisc)
DF4$values=cut2(DF4$DATA_VAL,g=4)  ####Dividing the data into 4 segments 

table(DF4$values) ####Show results

#######  Creating factor variables
DF4$fac<-factor(DF4$DATA_VAL) ####Grouping of data
DF4$fac[1:20] ### Show the first 20 groups and number of all levels

######## 4.Reshaping data #########
DF5<-read.csv("/Users/apple/Desktop/R/price_index_Feb20201.csv")

##### Clear blank lines
DF5<- na.omit(DF5)
########### reshaping
library(reshape2)
head(DF5)

DF5$newname<-rownames(DF5) ###Match the name of each row to the last column
head(DF5)    ####show the first 6 rows of data

###### Melting data frames
DF5Melt<-melt(DF5,id=c("newname","DATA_VAL","Subject"),measure.vars = c("TIME_REF","UNITS"),na.rm=TRUE) ###Extract the three columns of data and form a new column of 
##### variables with the selected headings
head(DF5Melt)  ####show the first 6 rows of data
tail(DF5Melt)   ####show the last 6 rows of data

###### Casting data frame
names(DF5Melt)<-tolower(names(DF5Melt)) #######Alphabetic characters converted to lower case

DATA_VALData<-dcast(DF5Melt,newname~variable) ###Converting long format data to wide format data

DATA_VALData ####show result

###### Q3. Use the factor function for column "Series_title_1" and get the average in column "Data_value" by sapply #########

######## read data 
DF6<-read.csv("/Users/apple/Desktop/R/price_index_Feb20201.csv")

##### Clear blank lines
DF6<- na.omit(DF6)

#####factor function
s<-split(DF6$DATA_VAL,DF6$Series_title_1) ##### Separate the two required columns
s ###### show the result

sapply(s, mean) ######Find the average value of the product
