setwd("C:/R files")
getwd()

employeeAttrition <- read.csv("Employee_Attrition.csv", header = TRUE, strip.white = TRUE)
head(employeeAttrition)
tail(employeeAttrition)
nrow(employeeAttrition)
ncol(employeeAttrition)
str(employeeAttrition)
names(employeeAttrition)
attach(employeeAttrition)

#------------------EDA-------------------------------------------------------------------

colSums(is.na(employeeAttrition))

meanMI <- mean(MonthlyIncome)
medianMI <- median(MonthlyIncome)
stdMI <- sd(MonthlyIncome)
ZscoreMI <- (MonthlyIncome - meanMI)/stdMI
employeeAttrition[ZscoreMI > 3 | ZscoreMI < -3,]
which(ZscoreMI > 3 | ZscoreMI < -3)
length(which(ZscoreMI > 3 | ZscoreMI < -3))

SkewnessMI <- 3*(meanMI - medianMI)/stdMI

qqnorm(MonthlyIncome, datax = TRUE, col = "Red")
qqline(MonthlyIncome, datax = TRUE, col = "Green")
range(ZscoreMI)

summary(MonthlyIncome)
length(which(MonthlyIncome == 0))
fquartile <- quantile(MonthlyIncome, 0.25)
tquartile <- quantile(MonthlyIncome, 0.75)
UpperQuartile <- tquartile + 1.5*(tquartile - fquartile)
LowerQuartile <- fquartile - 1.5*(tquartile - fquartile)
employeeAttrition[MonthlyIncome > UpperQuartile | MonthlyIncome < LowerQuartile,]
length(which(MonthlyIncome > UpperQuartile | MonthlyIncome < LowerQuartile))
# No outliers in Age

meanHourlyRate <- mean(HourlyRate)
medianHourlyRate <- median(HourlyRate)
stdHourlyRate <- sd(HourlyRate)
ZscoreHourlyRate <- (HourlyRate - meanHourlyRate)/stdHourlyRate
employeeAttrition[ZscoreHourlyRate > 3 | ZscoreHourlyRate < -3,]
which(ZscoreHourlyRate > 3 | ZscoreHourlyRate < -3)
length(which(ZscoreHourlyRate > 3 | ZscoreHourlyRate < -3))

SkewnessHourlyRate <- 3*(meanHourlyRate - medianHourlyRate)/stdHourlyRate

qqnorm(HourlyRate, datax = TRUE, col = "Red")
qqline(HourlyRate, datax = TRUE, col = "Green")
range(ZscoreAge)

summary(HourlyRate)
length(which(HourlyRate == 0))
fquartileHR <- quantile(HourlyRate, 0.25)
tquartileHR <- quantile(HourlyRate, 0.75)
UpperQuartileHourlyRate <- tquartileHR + 1.5*(tquartileHR - fquartileHR)
LowerQuartileHR <- fquartileHR - 1.5*(tquartileHR - fquartileHR)
employeeAttrition[HourlyRate > UpperQuartileHourlyRate | ï..Age < LowerQuartileHR,]
length(which(HourlyRate > UpperQuartileHourlyRate | HourlyRate < LowerQuartileHR))


library(ggplot2)


t2 <- table(Attrition)

paste(prop.table(t2)*100, "%", sep = "")
pie(t2, labels = paste(round(prop.table(t2)*100), "%", sep = ""),col = c("Red","Orange"), main = "Attrition of Employees")
legend("topright", c("No","Yes"), cex = 0.8,
       fill = c("Red","Orange"))
box(which = "plot",lty = "solid", col = "Black")



plot2 <- ggplot(data = employeeAttrition, aes(x = Attrition, y = MonthlyIncome, color = Attrition))
test1 <- plot2 + geom_boxplot(alpha = 0.5) + xlab("Attrition") + ylab("Monthly Income") + ggtitle("Attrition Vs Monthly Income") + 
  theme(axis.title.x = element_text(color = "Black", size = 10),
        axis.title.y = element_text(color = "Black", size = 10),
        axis.text.x = element_text(color = "Black", size = 10),
        axis.text.y = element_text(color = "Black", size = 10),
        plot.title = element_text(color = "Dark Green", size = 13))
test1


t3 <- table(Attrition, OverTime)
t4 <- round(prop.table(t3, margin = 2),4)*100


barplot(t4, legend = row.names(t4), xlab = "Over Time", ylab = "Percentage", main = "Attrition Vs Over Time", col = c("Blue","Yellow"))
box(which = "plot",lty = "solid", col = "Black")
# Attrition Rate among employees working overtime are higher when compared to employees not working overtime

plot3 <- ggplot(data = employeeAttrition, aes(x = Attrition, y = TotalWorkingYears, color = Attrition))
test2<-plot3 + geom_boxplot(alpha = 0.5) + xlab("Attrition") + ylab("Total Work Experience") + ggtitle("Attrition Vs Total Work Experience") + 
  theme(axis.title.x = element_text(color = "Black", size = 10),
        axis.title.y = element_text(color = "Black", size = 10),
        axis.text.x = element_text(color = "Black", size = 10),
        axis.text.y = element_text(color = "Black", size = 10),
        plot.title = element_text(color = "Dark Green", size = 13))
test2

#######
plot4 <- ggplot(data = employeeAttrition, aes(x = Attrition, y = YearsAtCompany, color = Attrition))
test3<-plot4 + geom_boxplot(alpha = 0.5) + xlab("Attrition") + ylab("Years at Company") + ggtitle("Attrition Vs Years at Company") + 
  theme(axis.title.x = element_text(color = "Black", size = 10),
        axis.title.y = element_text(color = "Black", size = 10),
        axis.text.x = element_text(color = "Black", size = 10),
        axis.text.y = element_text(color = "Black", size = 10),
        plot.title = element_text(color = "Dark Green", size = 13))
test3

plot5 <- ggplot(data = employeeAttrition, aes(x = MonthlyIncome, y = ï..Age, color = Attrition))
test4<-plot5 + geom_point(alpha = 0.4) + xlab("Monthly Income") + ylab("Age") + ggtitle("Monthly Income Vs Age") + 
  theme(axis.title.x = element_text(color = "Black", size = 10),
        axis.title.y = element_text(color = "Black", size = 10),
        axis.text.x = element_text(color = "Black", size = 10),
        axis.text.y = element_text(color = "Black", size = 10),
        plot.title = element_text(color = "Dark Green", size = 13))
test4

plot6 <- ggplot(data = employeeAttrition, aes(x = MonthlyIncome, fill = Attrition))
test5<-plot6 + geom_histogram(color = "Black", binwidth = 1000) + facet_grid(Attrition~., space = "free") + xlab("Monthly Income") + ggtitle("Attrition on Monthly Income") + 
  theme(axis.title.x = element_text(color = "Black", size = 10),
        axis.title.y = element_text(color = "Black", size = 10),
        axis.text.x = element_text(color = "Black", size = 10),
        axis.text.y = element_text(color = "Black", size = 10),
        plot.title = element_text(color = "Dark Green", size = 13))
test5


#install.packages("gridExtra")
require(gridExtra)
grid.arrange(test1,test2,test3)


# The Attrition of an employee is high with less work experience

t5 <- table(Attrition, Gender)
t6 <- round(prop.table(t5, margin = 2),4)*100

barplot(t6, legend = row.names(t5), xlab = "Gender", ylab = "Attrition Percentage", main = "Attrition Vs Gender", col = c("Blue","Yellow"))
box(which = "plot",lty = "solid", col = "Black")
#There is no such significance found from the plot

t7 <- table(Attrition, MaritalStatus)
t8 <- round(prop.table(t7, margin = 2),4)*100

barplot(t8, legend = row.names(t7), xlab = "Martial Status", ylab = "Attrition Percentage", main = "Attrition Vs Marital Status", col = c("Blue","Yellow"))
box(which = "plot",lty = "solid", col = "Black")
# The attition rate is high for singles when compared to Married and Divorced employees


t9 <- table(Attrition, PerformanceRating)
t10 <- round(prop.table(t9, margin = 2),4)*100

barplot(t10, legend = row.names(t9), xlab = "Martial Status", ylab = "Attrition Percentage", main = "Attrition Vs Marital Status", col = c("Blue","Yellow"))
box(which = "plot",lty = "solid", col = "Black")

unique(WorkLifeBalance)
# We found that perfomance rating variable is not a dependable predictor as rating 1 & 2 is not considered in the performance rating variable

t11 <- table(Attrition, WorkLifeBalance)
t12 <- round(prop.table(t11, margin = 2),4)*100

barplot(t12, legend = row.names(t11), xlab = "Work Life Balance", ylab = "Attrition Percentage", main = "Attrition Vs Work Life Balance", col = c("Blue","Yellow"))
box(which = "plot",lty = "solid", col = "Black")
#Attrition rate is high for employees who have rated their work life balance as 1.

t13 <- table(Attrition, YearsSinceLastPromotion)
t14 <- round(prop.table(t13, margin = 2),4)*100

barplot(t14, legend = row.names(t13), xlab = "Years Since Last Promotion", ylab = "Attrition Percentage", main = "Attrition Vs Last Promotion", col = c("Blue","Yellow"))
box(which = "plot",lty = "solid", col = "Black")

#We could find a cyclic pattern in attritions when compared with Employee's last promotion year by year
# The employee attrition becomes nill/almost nill in every 4th or 5th year.



par(mfrow = c(1,1))

#-------------------------------------------------------------------------------------------

#converted Attrition and Gender to binary variables
employeeAttrition$Attrition <- ifelse(employeeAttrition$Attrition == "Yes",1,0)
names(employeeAttrition[,c(7,9,10,22,27)])
employeeAttrition <- employeeAttrition[,-c(7,9,10,22,27)]


#creating dummies of categorical variables
library(fastDummies)
dummies <- dummy_cols(employeeAttrition, select_columns = c("BusinessTravel","Department","Gender","EducationField","EnvironmentSatisfaction","JobInvolvement", "JobLevel","JobRole","JobSatisfaction","MaritalStatus","OverTime","PerformanceRating","StockOptionLevel","RelationshipSatisfaction","WorkLifeBalance"))
str(dummies)
ncol(dummies)
#removing base categorical variables that have dummies
colnames(dummies)
dummies_R1 <- dummies[,-c(3,5,7,8,9,11,12,13,14,15,19,21,22,23,26)]
ncol(dummies_R1)
colnames(dummies_R1)
dummies_R2 <- dummies_R1[,-c(16,19,22,24,30,34,38,43,52,56,59,61,63,67,71)]
ncol(dummies_R2)
colnames(dummies_R2)

#partitioning the data for training and test dataset
set.seed((23))
library(caTools)
attach(dummies_R2)

split <- sample.split(dummies_R2[,2], SplitRatio = 0.8)
training <- subset(dummies_R2, split == "TRUE")
testing <- subset(dummies_R2, split == "FALSE")
View(dummies_R2)

#Fitting Logistic Regression
logisticRegModel <- glm(Attrition~., data = training, family = binomial)
summary(logisticRegModel)


library(car)
vif(logisticRegModel)

logisticRegModel1 <- glm(Attrition~1, data = training, binomial(link = logit))

finalModel <- step(logisticRegModel1,scope=list(lower=logisticRegModel1,upper=logisticRegModel),direction = "both",test="F",trace=TRUE)
summary(finalModel)


vif(finalModel)

#Checking linearity assumptions
resTrain <- predict(finalModel, data = training, type = "response")
length(resTrain)
logitTrain <- log(resTrain/(1-resTrain))
plot(logitTrain, training$MonthlyIncome)

#Check for influential points
plot(finalModel, 5)
# there are no influential points in the dataset of the finalmodel

#Modal Significance
with(finalModel,null.deviance-deviance)

#degree of freedom
with(finalModel,df.null-df.residual)

#p-value of the test
with(finalModel,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail=FALSE))

# the p-value of the test is 8.588861e-67 which is highly significant

summary(finalModel)

#Predicting probabilities on the test data from logistic model
resTest<- predict(finalModel, testing, type = "response")
length(resTest)

#Performance on test data
#ROC curve & AUC
#install.packages("InformationValue")
library(InformationValue)
plotROC(actuals = testing$Attrition, predictedScores = resTest)

#Binary Classification
## Cuttoff value while minimizing misclassification error
threshold <- optimalCutoff(actuals = testing$Attrition, predictedScores = resTest, optimiseFor = "misclasserror")
threshold
# the threshold value is 0.6265676

#Performance on test data
# confusion matrix with threshold 0.6265676
predictedType <- ifelse(resTest > threshold, "positive","negative")
p <- table(ActualValue = testing$Attrition, PredictedValue = predictedType)
accuracy <- (p[1,1]+p[2,2])/sum(p)
accuracy
#Accuracy of the model is 89.11
