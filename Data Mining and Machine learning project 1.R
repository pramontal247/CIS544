library(readr)
data <-  read_csv("C:/Users/manme/Desktop/EPL_DATA.csv")
data

# Description Of the Variables.
#HomeTeam = Home Team
#AwayTeam = Away Team
#FTHG = Full Time Home Team Goals
#FTAG = Full Time Away Team Goals
#FTR = Full Time Result (H=Home Win, D=Draw, A=Away Win)
#HTR = Half Time Result (H=Home Win, D=Draw, A=Away Win)	
#HS = Home Team Shots	
#AS = Away Team Shots
#HST = Home Team Shots on Target	
#AST = Away Team Shots on Target	
#HF = Home Team Fouls Committed	
#AF = Away Team Fouls Committed
#HC = Home Team Corners
#AC = Away Team Corners
#HTP = Home Team possession
#ATP = Away Team Possession

#Splitting the data set in Training set and Test set

#install.packages("caTools")
library(caTools)

# After downloading the caTools library I will now split my data into training set and Test set.

set.seed(100)
split <- sample.split(data$FTR,SplitRatio = 0.8)
training_set<- subset(data,split==TRUE)
test_set<- subset(data,split==FALSE)

#Feature Scaling

training_set[,5:18]<- scale(training_set[,5:18])
test_set[,5:18] <- scale(test_set[,5:18])

# Creating a new variable "Result" to convert FTR to a factor. 0 represents Away team, 0.5 represents Draw, and 1 represents Home team
training_set<- na.omit(training_set)
training_set$result <- ifelse(training_set$FTR=="A", 0 ,ifelse(training_set$FTR=="H", 1,0.5))
test_set$result<- ifelse(test_set$FTR=="A",0,ifelse(test_set$FTR=="H",1,0.5))


# Logistic regression to find out the winner.
classfier <- glm(formula =result~HST+AST+HC+AC,family = "binomial" ,data= training_set)
prob_pred <- predict(classfier,type = 'response',newdata = test_set[-19])
y_pred <- ifelse(prob_pred<0.50,"A",ifelse(prob_pred<0.55,"D","H"))

cm<- table(y_pred,test_set$result)
cm



# SVM for predicting the results.

# installing Kernlab for SVM
installed.packages("kernlab")
library(kernlab)

c <- svm(formula=FTR~HST+AST+HF+AF+HC+AC, data=training_set, type="C-classification",kernel= "radial")
s_pred<- predict(c,newdata = test_set[-4])
s_pred

cm<- table(s_pred,test_set$FTR)
cm


#Decision Tree Classification for predicting the result.

#install.packages("rpart")
library(rpart)

d <- rpart(formula=FTR~HST+AST+HF+AF, data=training_set)
d_pred<- predict(d,newdata = test_set[-4],type="class")

cm<- table(d_pred,test_set$FTR)
cm

# Artificial Neural Network to predict the result

#Installing the h2o package for ANN

install.packages("h2o")
library(h2o)

#Loading the data

data <-  read_csv("C:/Users/manme/Desktop/EPL_DATA.csv")
data <- data[4:18]
data

# Splitting the data set in to training set and test set

set.seed(100)
split <- sample.split(data$FTR,SplitRatio = 0.8)
training_set<- subset(data,split==TRUE)
test_set<- subset(data,split==FALSE)

#Creating a new variable "Result" to convert FTR to a factor. 0 represents Away team, 0.5 represents Draw, and 1 represents Home team

training_set$result <- ifelse(training_set$FTR=="A", 0 ,ifelse(training_set$FTR=="H", 1,0.5))
test_set$result<- ifelse(test_set$FTR=="A",0,ifelse(test_set$FTR=="H",1,0.5))

#removing the FTR Variable

training_set$FTR<- NULL
test_set$FTR<- NULL

#Feature scaling, because in this package feature scaling is must for optimal results.

training_set[-16]<- scale(training_set[-16])
#test_set [-16]<- scale(test_set[-16])

#Connecting to a h2o cluster

h2o.init(nthreads = -1)

ANN <- h2o.deeplearning(y = "result",
                        training_frame = as.h2o(training_set),
                        activation = "Rectifier",
                        hidden =c(8),
                        epochs = 100,
                        train_samples_per_iteration = -2)


neural.pred <- h2o.predict(ANN, newdata = as.h2o(test_set[-15]))
network.pred <- ifelse(neural.pred<0.33,"A",ifelse(neural.pred<0.66,"D","H"))
network.pred <- as.vector(network.pred)
network.pred

m <- table(network.pred,test_set$result)
m

h2o.shutdown()


# multiple linear regression to predict the goals.

library(readr)
data2 <-  read_csv("C:/Users/manme/Desktop/EPL_DATA.csv")
data2

library(caTools)

set.seed(100)
split <- sample.split(data2$FTHG,SplitRatio = 0.8)
training_set<- subset(data2,split==TRUE)
test_set<- subset(data2,split==FALSE)

training_set[,7:18]<- scale(training_set[,7:18])
test_set[,7:18] <- scale(test_set[,7:18])

# Pridicting the Home team goals.

hg<- lm(formula =FTHG~HST+HC+AST+HF+AC, data = training_set)
summary(hg)

hg_pred<- predict(hg,newdata = test_set[-5])
hg_pred

round(hg_pred)


# predicting the Away team goals.

ag<- lm(formula =FTAG~AST+AC, data = training_set)
summary(ag)

ag_pred<- predict(hg,newdata = test_set[-6])
ag_pred

round(ag_pred)


# Fitting Decision tree regression model to predict the scores

library(rpart)

#Predicting the home team goals.

hdt <- rpart(formula = FTHG~HST+HC+AST+HF+AC, data = training_set)
reg.pred<- predict(hdt,newdata = test_set[-5])
reg.pred
round(reg.pred)

#Predicting the away team goals.

adt <- rpart(formula = FTAG~AST+AC, data = training_set)
dt.pred<- predict(adt,newdata = test_set[-5])
dt.pred
round(dt.pred)

