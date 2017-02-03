setwd("E:/TDS 3301 - Data Mining/Assignment/Phase 3")
getwd()

library(Amelia)
library(editrules)
library(DT)
library(ggplot2)
library(ISLR)
library(tree)
library(party)
library(tidyr)
library(plyr)
library(dplyr)
library(ISLR)
library(caTools)
library(caret)
library(e1071)
library(neuralnet)

# saves workspace
save.image()

# saves history file
savehistory()

###------------------Pre-Processing & Data Exploration-----------------------###

# initialize dataset with proper columns and headers
stuMaths <- read.table(file = "student-mat.csv", header = TRUE, sep =";")

# initialize dataset without proper formatting
stuMaths<-read.csv("student-mat.csv")

# check dimension of dataset
dim(stuMaths)

# overview of dataset
summary(stuMaths)

# structure of dataset
str(stuMaths)

# check missing values
sum(is.na(stuMaths))

# check missing values again for dataset
sapply(stuMaths,function(x) sum(is.na(x)))

# Amelia package has a special plotting function missmap() that will plot dataset and highlight missing values 
missmap(stuMaths, main = "Missing values vs observed for studentMath")

# detects rows in a data.frame that do not contain any missing value 
complete.cases(stuMaths)

# calculate percentage of complete cases
complete_case_percent <- 100*length(which(complete.cases(stuMaths)))/nrow(stuMaths)

paste("Percentage of complete observations is ", complete_case_percent, "%", sep = "")

#Does the data contain other special values?
any(sapply(stuMaths, is.nan))

#check duplicate record
anyDuplicated(stuMaths)

#[1] 16 16 24 22 16 32 16 16 30 21 16 18 16 26 16 16 22 18 18 16 21
boxplot(stuMaths$absences, main = "Boxplot for stuMaths$absences")

#[1] 25 54 26 56 24 28 22 21 75 22 30 38 22 40 23
# Both outlier stuMat$absences and stuPor$absences should be acceptable since the range for number of school absences (numeric: from 0 to 93) 
# We need this data value to check any impact on the student's grade.
boxplot.stats(stuMaths$absences)$out

#check outlier for  first period grade G1 
boxplot(stuMaths$G1, main = "Boxplot for stuMaths$G1")

#check outlier for second period grade G2
boxplot(stuMaths$G2, main = "Boxplot for stuMaths$G2")
boxplot.stats(stuMaths$G2)$out

#check rules obey. the editrule file was created according to the dataset's attribute information
E <- editfile("edits.txt")
ve <- violatedEdits(E, stuMaths)
summary(ve)


###----------------------------Graph Plotting--------------------------------###

# two-way comparison on the number of males and females vs final grade rates in proportion using the train data frame stuMat
counts = prop.table(table(stuMaths$sex, stuMaths$G3))

barplot(counts, main="Sex VS Final Grade (G3) rates in proportion for StudentMaths", xlab="G3", col=c("darkblue","red"), legend = rownames(counts), beside=TRUE)

#visualize correlation G3 vs G1 and G2
pairs(~G3+G1+G2,data = stuMaths,col=c("red","blue","green"), main = "Correlationship for G3 with G1 and G2 - StudentMath")

#view original data cluster assignments are random - stuMath
ggplot(stuMaths, aes(G1, G2, color = higher)) + geom_point() + ggtitle("Random cluster for G1 vs G2 on higher variable")

# set the seed to ensure reproducibility
set.seed(20)

# R will try 20 different random starting assignments and then select the one with the lowest within cluster variation.
stuMathsCluster <- kmeans(stuMaths[, 31:32], 2, nstart = 20)
stuMathsCluster

# compare the clusters with the higher (yes or no)
table(stuMathsCluster$cluster, stuMaths$higher)
stuMathsCluster$cluster <- as.factor(stuMathsCluster$cluster)
ggplot(stuMaths, aes(G1, G2,  color = stuMathsCluster$cluster)) + geom_point() + ggtitle("2 cluster for  G1 vs G2 on higher variable")


###-------------------- Classification with Decision Tree -----------------###

# Only involves columns that are related to the problem
stuMaths <- stuMaths[,c(6,7,13,23,31,32,33)]

# Adding new column named Pass_Or_Fail and results determined by G3 data
Pass_Or_Fail <- ifelse(stuMaths$G3 >= 10, 'Pass', 'Fail')
stuMaths <- data.frame(stuMaths,Pass_Or_Fail)

# Predicts Pass or Fail with G1 + G2 using decision tree
formula <- Pass_Or_Fail ~ G1 + G2
tree <- ctree(formula, data=stuMaths)

# An overview of the tree
print(tree)

# plot tree with a simpler way with numbers
plot(tree, type = 'simple')

sprintf('Errors-on-predictions Matrix')

table(predict(tree, newdata=stuMaths), stuMaths$Pass_Or_Fail,dnn=c('Predicted','Actual'))

df.confmatrix <- data.frame(table(predict(tree, newdata=stuMaths), stuMaths$Pass_Or_Fail,dnn=c('Predicted','Actual')))

data_long <- gather(df.confmatrix, Type, Status, Predicted:Actual)

data_long <- data_long %>% group_by(Status,Type) %>% summarise(Frequency=sum(Freq))

ggplot(data_long, aes(x=Status,y=Frequency,fill=Type)) + geom_bar(stat='identity', position='dodge')

###------------------------Classification with Naive Bayes-------------------------###
stuMaths$Pass_Or_Fail <- as.factor(stuMaths$Pass_Or_Fail)

table(stuMaths$Pass_Or_Fail)

set.seed(450)

split_size <- 0.7
n <- nrow(stuMaths)
train1 <- sample(1:n, size = round(split_size*n), replace=FALSE)
training <- stuMaths[train1 ,]
testing <- stuMaths[-train1 ,]

#check the row and proportion for training and dataset
nrow(training)
nrow(testing)
prop.table(table(training$Pass_Or_Fail))
prop.table(table(testing$Pass_Or_Fail))


classify<-naiveBayes(stuMaths[,1:4], stuMaths[,8]) 

predictions <- predict(classify,testing) 

library(rminer)

head(predictions,n=10)

print(confusionMatrix(predictions,testing$Pass_Or_Fail,positive = "Yes", dnn=c("Prediction","Actual")))

mmetric(testing$Pass_Or_Fail,predictions,c("Acc","Precision","TPR","F1"))


table(predict(classify, stuMaths[,1:4]), stuMaths[,8], dnn = c('Prediction','Actual'))

df.confmatrix <- data.frame(table(predict(classify, stuMaths[,1:4]), stuMaths[,8], dnn = c('Predicted','Actual')))

data_long <- gather(df.confmatrix, Type, Status, Predicted:Actual)
data_long <- data_long %>% group_by(Status,Type) %>% summarise(Frequency=sum(Freq))
ggplot(data_long, aes(x=Status,y=Frequency,fill=Type)) + geom_bar(stat='identity', position='dodge') 


###------------------------Classification with ANN-------------------------###

# Converting non-numeric values to numeric
Pass_Or_Fail = as.numeric(stuMaths$Pass_Or_Fail)-1
stuMaths$Pass_Or_Fail <- Pass_Or_Fail

# Create Vector of Column Max and Min Values
maxs <- as.numeric(apply(stuMaths[,c(2,3,5,6,7,8)], 2, max))
mins <- as.numeric(apply(stuMaths[,c(2,3,5,6,7,8)], 2, min))
as.numeric()

# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(stuMaths[,c(2,3,5,6,7,8)],center = mins, scale = maxs - mins))

set.seed(101)

# Splits dataset into train and test set
split = sample.split(stuMaths$Pass_Or_Fail, SplitRatio = 0.70)

# Split based off of split Boolean Vector
train = subset(stuMaths, split == TRUE)
test = subset(stuMaths, split == FALSE)

feats <- names(scaled.data)

stuMaths$Pass_Or_Fail <- NULL
stuMathsScaled <- as.data.frame(stuMaths)

# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('Pass_Or_Fail ~',f)

# Convert to formula
f <- as.formula(f)
f

# Creating neural network
nn <- neuralnet(f,train,hidden=c(10,10,10),linear.output=FALSE)

n <- names(train)
f <- as.formula(paste("Fail_Or_Pass ~", paste(n[!n %in% "Fail_Or_Pass"], collapse = " + ")))
nn <- neuralnet(f,train,hidden=c(5,3),linear.output=T)

# Compute predictions off test set
predicted.nn.values <- compute(nn,test[1:5])

# Check out net.result
print(head(predicted.nn.values$net.result))

predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)

table(test$Private,predicted.nn.values$net.result)

plot(nn)


# Predicting medv using neural network
pr.nn <- compute(nn,test_[,1:6])

pr.nn_ <- pr.nn$net.result*(max(stuMaths$medv)-min(stuMaths$medv))+min(stuMaths$medv)
test.r <- (test_$medv)*(max(stuMaths$medv)-min(stuMaths$medv))+min(stuMaths$medv)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

# 
mmetric(pr.nn,predicted.nn.values,c("Acc","Precision","TPR","F1"))

par(mfrow=c(1,2))

plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)

legend('bottomright',legend='NN',pch=18,col='red', bty='n')
