# Ali Zahraei, 2013
#Text message mining.
#THIS IS ANOTHER VERSION OF THIS CODE, MIGHT BE SLIGHTLY DIFFERENT THAN 
# WHAT I DISCUSSED.IT DOES ANALYTICS, GRAPHICS WAS DONE BY MATLAB.

# Porbabilistic Model (Naive Bayes), and Deterministic Model (SVM- Linear & Radial Kernel] and Adaboost
# This code reads a text file containg SMS txt messages, and 
# classify SMS to spam and NON-spam.
# First column, data file is the classification ("Spams and Hams").
# Originally code was written for one of my project at UC-Irvine, 
# and it is majorly adapted recently for some new application.
# Code has several major sections.

#--------------------------------------------
cls <- function() cat(rep("\n",100))
cls()
# ----Section 1------------------------------
#--------------------------------------------
#----Install and call library packages
#--------------------------------------------
# Install required packages
install.packages('e1071',dependencies=TRUE)
library(e1071)
#--------------------
install.packages("ada")
library("ada")
#--------------------
install.packages("tm")
library("tm")

#---Section 2-----------------------------
#-----------------------------------------
#----Function Definition
#----------------------------------------
#--counter calculate frequency of each word within data set
#Function definition , comparing each string with repeated words
counter <- function(message,repeatedwords){
#convert to string
splittedSMS<-strsplit(toString(message), "\\s+")[[1]]
# vect is a seprate vector for each data entry, the length is equal to 
# number of repeated words

vect = NULL 
vect <- c(rep(0,length(repeatedwords)))
# counting frequency of the most repeated words 
# in the data poll for each SMS txt message.
# Function will repeat seprately for each SMS.
for (i in 1:length(repeatedwords)){
      for (j in 1:length(splittedSMS)){
            if (repeatedwords[i] == splittedSMS[j]){
              vect[i] <- 1 # vect[i] <- vect[i]+1
                                                   }
                                       }
                                  }
               return(vect)
                                          }
#--Define naive Bayes probability Function------------
prob <- function(x,stat) {
probability=NULL;
  if (x==0) {
    probability[1] <- stat[1,1] 
    probability[2] <- stat[1,2]
   }    else  {  
    probability[1] <- stat[2,1]
    probability[2] <- stat[2,2]}

return(probability)
}

#--Section 3----------------------------------
#---------------------------------------------
#---Loading data and few simple statistics
#---------------------------------------------

print("Loading the SMS data - Spams and Hams")
#Import the data from txt file / data downloaded from
#---Downloaded from http://archive.ics.uci.edu/ml/datasets/SMS+Spam+Collection
#SMSdata <-read.csv("C:/AliZ/Code/R/Sample_Project/data/SMSSpamCollection.txt", header = TRUE, sep = "\t") 
SMSdata <-read.csv("SMSSpamCollection.txt", header = TRUE, sep = "\t") 
colnames(SMSdata) <- c("Spam_OR_Ham","SMS_Content")
#tempdata<-SMSdata
# Replace ham and Spam with 0 and 1 (logical array)
#tempdata$Spam_OR_Ham[tempdata$Spam_OR_Ham=="spam"]<- 0
#tempdata$Spam_OR_Ham[tempdata$Spam_OR_Ham=="ham"]<- 1
#tempdata$Class=="ham"

#Simple Statistics Extracted from Original Data Set
count <- as.numeric(SMSdata$Spam_OR_Ham=="ham")
print ("The ratio of NON-SPAM SMS %"); 100*mean(count)
print ("The Varianc of data");  var(count)

# Section 4--------------------------------
#------------------------------------------
#---Sampling & Subsetting --> Training and Testing Data Set (two-fold cross validation)----
#------------------------------------------
print ("Sampling & Make two sets of data, Trainig 70%, and Testing 30%");
#
tempdata<-SMSdata
n<- nrow(tempdata)
#set.seed(100)
#
ind <- sample(1:n)
trainval <- ceiling(n* .7)
testval<- ceiling(n * .3)
#
trainData = NULL; trainData <- tempdata[ind[1:trainval],]
testData =NULL; testData <- tempdata[ind[(trainval+1):n],]
print("Training Data Set Size is"); dim(trainData);
print("Testing Data Set Size is"); dim(testData);

# Section 5---------------------------------
#-------------------------------------------
#--Feature extraction from text message (tm toolbox)----
#------------------------------------------

#(tm) Package
# Words with highest frequencies.
# We know highly used words, we use this frequency to 
# train vector space model
# corpus(x, reader), VectorSource: identify the character entity of data
trsmses<-Corpus(VectorSource(trainData[,2]))
#transformation :How to modify corpus
#Extra whitespace is eliminated by
trsmses<-tm_map(trsmses, stripWhitespace)
#Conversion to lower case by
trsmses<-tm_map(trsmses, tolower)
#Removal of stopwords by
trsmses<-tm_map(trsmses, removeWords, stopwords("english"))
dtm <- DocumentTermMatrix(trsmses) 
# find words with min frequency of (80)
repeatedwords<-findFreqTerms(dtm, 80)
repeatedwords <- repeatedwords[4:length(repeatedwords)]


#Section 6----------------------------------
#-------------------------------------------
#----How many of popular words are repeated whithin each text message?----
#------------------------------------------

#vectorized training data set
trdata=NULL
#vectorized test data set 
tedata=NULL

#Count the number of repeated words in any data instance
#train data
for (i in 1:length(trainData[,2])){
   if (trainData[i,1] == "ham") {    
   trdata = rbind(trdata,c(1,counter(trainData[i,2],repeatedwords)))
                                }
   else {
   trdata = rbind(trdata,c(0,counter(trainData[i,2],repeatedwords)))
        }

                                  }
#test data

for (i in 1:length(testData[,2])){
    if (testData[i,1] == "ham") {      
    tedata = rbind(tedata,c(1,counter(testData[i,2],repeatedwords)))
                                }
    else {
    tedata = rbind(tedata,c(0,counter(testData[i,2],repeatedwords)))
         }

                                 }
#-------------------------------------------
#-------------------------------------------
#---PREDICTIVE MODELS-----------------------
#_____-----_____----------____________------

#---Section 7-------------------------------
#------------Naive Bayes--------------------


#-------Lebel Identification -------------
lable.sum <- unique(sort(trdata[,1]))
#----------Training Model---------------
stat.data.class0=NULL;stat.data.class1=NULL
         # for (i in 1:length(lable.sum)){
         #Binary labels 1 is first label   such=successs for 1
         data.class0 <- trdata[trdata[,1]==lable.sum[1],]
         stat.data.class0 <- (colSums(data.class0[,2:(dim(data.class0)[2])]==lable.sum[1]))/nrow(data.class0) #nrow(trdata)
         
         stat.data.class0 <- rbind(stat.data.class0,(colSums(data.class0[,2:(dim(data.class0)[2])]==lable.sum[2]))/nrow(data.class0))

         data.class1 <- trdata[trdata[,1]==lable.sum[2],]
         stat.data.class1 <- (colSums(data.class1[,2:(dim(data.class1)[2])]==lable.sum[1]))/nrow(data.class1)
         stat.data.class1 <- rbind(stat.data.class1,(colSums(data.class1[,2:(dim(data.class1)[2])]==lable.sum[2]))/nrow(data.class1))


#--Testing--------------------
#--Prior Calculation----------
prior.p.set<- NULL
for (i in 1:length(lable.sum)){
prior.p.set[i] <- length(trdata[trdata[,1]==lable.sum[i],1])/dim(trdata)[1]
}

#-------Posterior Calculation-
likelihood.group0 = NULL;likelihood.group1= NULL;assigned.label=NULL;
 for (i in 1:nrow(tedata)){
     
    for (j in 2:(ncol(tedata)-1)) {

     likelihood.group0 [j-1] <- prob(tedata[i,j],t(rbind(stat.data.class0[,j-1],stat.data.class1[,j-1])))[1]
     likelihood.group1 [j-1] <- prob(tedata[i,j],t(rbind(stat.data.class0[,j-1],stat.data.class1[,j-1])))[2]       
                                   }

 evidence <- prior.p.set[1]*prod(likelihood.group0)+ prior.p.set[2]*prod(likelihood.group1)      
      
  posterior <- NULL
   posterior[1] <-  (prior.p.set[1]*prod(likelihood.group0))/evidence
     posterior[2] <-  (prior.p.set[2]*prod(likelihood.group1))/evidence
         assigned.label[i] <- which.max(posterior[])-1
                           }

#Contigency table
tab0 <- table(assigned.label, true=tedata[,1])
print("Naive Bayes Contigency Table")
classAgreement(tab0,match.names=FALSE)

#---Section 8-------------------------------
#----Adaboost-------------------------------

adaptiveboost<-ada(x=trdata[,-1],y=trdata[,1],test.x=tedata[,-1], test.y=tedata[,1], loss="logistic", type="gentle", iter=100)
summary(adaptiveboost)
varplot(adaptiveboost)
#-------------------------------------------
#---Section 9-------------------------------
#----SVM with Radial Kernel------------------
#Tunning SVM C & Gamma parameters-----------
#Two main parameters, 0.1 <c < 10; 10e-6 < gamma < 0.1

tuned <- tune.svm (x=trdata[,-1], y=trdata[,1],gamma=10^(-6:-1),cost=10^(-1:1))
summary(tuned)
#
model1 <- svm(x=trdata[,-1], y=trdata[,1], kernel="radial",gamma=0.01,cost=1, type= "C")
summary(model1)
prediction1 <- predict(model1,tedata[,-1])
tab1 <- table(pred1=prediction1, true=tedata[,1])
print(" Radial kernel, gamma=0.1,cost=10")
classAgreement(tab1)

#---Section 10-------------------------------
#----SVM with Linear Kernel------------------
#Tunning SVM C & Gamma parameters-----------
#-----------------------------------------------
model2 <- svm(x=trdata[,-1], y=trdata[,1], kernel="linear",gamma=0.01,cost=1, type= "C")
summary(model2)
prediction2 <- predict(model2,tedata[,-1])
tab2 <- table(pred2=prediction2, true=tedata[,1])
print(" Linear kernel, gamma=0.1,cost=10")
classAgreement(tab2,match.names=FALSE)


