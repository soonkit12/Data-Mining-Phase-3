library(Amelia)
library(editrules)
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

#reading file
stuMat <- read.table(file = "files/student-mat.csv", header = TRUE, sep =";")
stuPor <- read.table(file = "files/student-por.csv", header = TRUE, sep =";")

#check dimension 
dim(stuMat)
dim(stuPor)

#understand summary of the dataset
summary(stuMat)
summary(stuPor)

#check structure of dataset
str(stuMat)
str(stuPor)

#check missing value
sum(is.na(stuMat))
sum(is.na(stuPor))

#check missing value again for each variable
sapply(stuMat,function(x) sum(is.na(x)))
sapply(stuPor,function(x) sum(is.na(x)))

# the Amelia package has a special plotting function missmap() that will plot your dataset and highlight missing values 
missmap(stuMat, main = "Missing values vs observed for studentMath")
missmap(stuPor, main = "Missing values vs observed for studentPor")

# detects rows in a data.frame that do not contain any missing value 
complete.cases(stuMat)
complete.cases(stuPor)

# calculate percentage complete cases
complete_case_percent <- 100*length(which(complete.cases(stuMat)))/nrow(stuMat)
paste("Percentage of complete observations is ", complete_case_percent, "%", sep = "")
complete_case_percent <- 100*length(which(complete.cases(stuPor)))/nrow(stuPor)
paste("Percentage of complete observations is ", complete_case_percent, "%", sep = "")

#Does the data contain other special values?
any(sapply(stuMat, is.nan))
any(sapply(stuPor, is.nan))

#check duplicate record
anyDuplicated(stuMat)
anyDuplicated(stuPor)

#check outlier for absences
boxplot(stuPor$absences, main = "Boxplot for stuPor$absences")
boxplot.stats(stuPor$absences)$out
#[1] 16 16 24 22 16 32 16 16 30 21 16 18 16 26 16 16 22 18 18 16 21
boxplot(stuMat$absences, main = "Boxplot for stuMat$absences")
boxplot.stats(stuMat$absences)$out
#[1] 25 54 26 56 24 28 22 21 75 22 30 38 22 40 23
# Both outlier stuMat$absences and stuPor$absences should be acceptable since the range for number of school absences (numeric: from 0 to 93) 
# We need this data value to check any impact on the student's grade.

#check outlier for  first period grade G1 
boxplot(stuMat$G1, main = "Boxplot for stuMat$G1")
boxplot(stuPor$G1, main = "Boxplot for stuPor$G1")
boxplot.stats(stuPor$G1)$out
#[1]  0 18 18 18 18 18  5  4  4  5 18  5  5 18 19  5 
#The outlier stuPor$G1 is acceptable. Since, the target range for Grade is from 0 to 20, so we wont 
#correct it because we wanted to observe the relationship for grade G1, G2 and G3

#check outlier for second period grade G2
boxplot(stuMat$G2, main = "Boxplot for stuMat$G2")
boxplot.stats(stuMat$G2)$out
#[1] 0 0 0 0 0 0 0 0 0 0 0 0 0
boxplot(stuPor$G2, main = "Boxplot for stuPor$G2")
boxplot.stats(stuPor$G2)$out
#[1] 18 18 18 18 19 18 18 18 18 18  0  5 18  0  0  5 18 18  0  0  0 18  0  5 18
#The outlier stuMat$G2 and stuPor$G2  is acceptable. Since, the target range for Grade is from 0 to 20, so we wont 
#correct it because we wanted to observe the relationship for grade G1, G2 and G3


#check outlier for final grade (G3)
boxplot(stuMat$G3, main = "Boxplot for stuMat$G3")
boxplot(stuPor$G3, main = "Boxplot for stuMat$G3")
outliers_SL <- boxplot.stats(stuPor$G3)$out
outliers_SL
#[1] 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#The outlier stuPor$G3 is acceptable. Since, the target range for Grade is from 0 to 20, so we wont 
#correct it because we wanted to observe the relationship for grade G1, G2 and G3

#check rules obey. the editrule file was created according to the dataset's attribute information
E <- editfile("files/edits.txt")
ve <- violatedEdits(E, stuMat)
summary(ve)
veP <- violatedEdits(E, stuPor)
summary(veP)


#plot graph

# two-way comparison on the number of males and females vs final grade rates in proportion using the train data frame stuMat
counts = prop.table(table(stuMat$sex, stuMat$G3))
barplot(counts, main="Sex VS Final Grade (G3) rates in proportion for StudentMat", xlab="G3", col=c("darkblue","red"), legend = rownames(counts), beside=TRUE)
# two-way comparison on the number of males and females vs final grade rates in proportion using the train data frame stuPor
counts2 = prop.table(table(stuPor$sex, stuPor$G3))
barplot(counts2, main="Sex VS Final Grade (G3) rates in proportion for StudentPor", xlab="G3", col=c("darkblue","red"), legend = rownames(counts2), beside=TRUE)

#visualize correlation G3 vs G1 and G2
pairs(~G3+G1+G2,data = stuMat,col=c("red","blue","green"), main = "Correlationship for G3 with G1 and G2 - StudentMath")
pairs(~G3+G1+G2,data = stuPor,col=c("red","blue","green"), main = "Correlationship for G3 with G1 and G2 - StudentPor")

#k-means clustering - based on higher variable for student wants to take higher education (binary: yes or no) with 2 cluster (only yes or no)

#view original data cluster assignments are random - stuMath
ggplot(stuMat, aes(G1, G2, color = higher)) + geom_point() + ggtitle("Random cluster for G1 vs G2 on higher variable")
# set the seed to ensure reproducibility
set.seed(20)
# R will try 20 different random starting assignments and then select the one with the lowest within cluster variation.
stuMatCluster <- kmeans(stuMat[, 31:32], 2, nstart = 20)
stuMatCluster
#compare the clusters with the higher (yes or no)
table(stuMatCluster$cluster, stuMat$higher)
stuMatCluster$cluster <- as.factor(stuMatCluster$cluster)
ggplot(stuMat, aes(G1, G2,  color = stuMatCluster$cluster)) + geom_point() + ggtitle("2 cluster for  G1 vs G2 on higher variable")

#view original data cluster assignments are random - stuPor
ggplot(stuPor, aes(G1, G2, color = higher)) + geom_point() + ggtitle("Random cluster for G1 vs G2 on higher variable")
set.seed(20)
stuPorCluster <- kmeans(stuPor[, 31:32], 2, nstart = 20)
stuPorCluster
table(stuPorCluster$cluster, stuPor$higher)
stuPorCluster$cluster <- as.factor(stuPorCluster$cluster)
ggplot(stuPor, aes(G1, G2,  color = stuPorCluster$cluster)) + geom_point() + ggtitle("2 cluster for  G1 vs G2 on higher variable")


#Exploratory data Analysis
#The relationship between health and attendance in class and the gender of the student.
stuMat%>%group_by(sex)%>%ggplot(aes(x= health, y=absences, color=sex))+ geom_smooth(aes(group=sex), method="lm", se=FALSE)


#Relationship between access to internet and the performance of the students.
stuMat%>%group_by(internet)%>%ggplot(aes(x=G3, fill=internet))+geom_density( alpha=0.5)