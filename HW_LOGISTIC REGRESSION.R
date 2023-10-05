
# Load the Customer Churn Dataset
customer_churn_dataset <- read.csv("LAST SEMESTER/archive/WA_Fn-UseC_-Telco-Customer-Churn.csv")
View(customer_churn_dataset)

#Remove missing values
customer_churn_dataset <- na.omit(customer_churn_dataset)
View(customer_churn_dataset)

# Identify the binary variables
binary_variables <- c('Partner', 'Dependents', 'PhoneService', 'MultipleLines', 'OnlineSecurity', 'OnlineBackup', 'DeviceProtection', 'TechSupport', 'StreamingTV', 'StreamingMovies','PaperlessBilling', 'Churn')

# Convert the binary variables to numerical variables using the ifelse() function
for (binary_variable in binary_variables) {
  customer_churn_dataset[, binary_variable] <- ifelse(customer_churn_dataset[, binary_variable] == 'Yes', 1, 0)
}

# Create a dictionary to map the levels of the categorical variables to integer values
{gender_mapping <- list(
  "Female" = 0,
  "Male" = 1
)

internet_service_mapping <- list(
  No = 0,
  DSL = 1,
  "Fiber optic" = 2
)

contract_mapping <- list(
  "Month-to-month" = 1,
  "One year" = 2,
  "Two year" = 3
)

payment_method_mapping <- list(
  "Electronic check" = 1,
  "Mailed check" = 2,
  "Bank transfer (automatic)" = 3,
  "Credit card (automatic)" = 4
)
}
# Convert the categorical variables to numerical variables using label encoding
#customer_churn_dataset$gender <- gender_mapping[customer_churn_dataset$gender] #_Numerical
#customer_churn_dataset$InternetService <- internet_service_mapping[customer_churn_dataset$InternetService] #_Numerical
#customer_churn_dataset$Contract <- contract_mapping[customer_churn_dataset$Contract] #_Numerical
#customer_churn_dataset$PaymentMethod <- payment_method_mapping[customer_churn_dataset$PaymentMethod] #_Numerical


# Convert the variables to numerical using factors
{customer_churn_dataset$gender <- as.numeric(factor(customer_churn_dataset$gender, levels = names(gender_mapping)))
customer_churn_dataset$InternetService <- as.numeric(factor(customer_churn_dataset$InternetService, levels = names(internet_service_mapping)))
customer_churn_dataset$Contract <- as.numeric(factor(customer_churn_dataset$Contract, levels = names(contract_mapping)))
customer_churn_dataset$PaymentMethod <- as.numeric(factor(customer_churn_dataset$PaymentMethod, levels = names(payment_method_mapping)))
}
#MAKE ALL VARIABLES FACTORS
{customer_churn_dataset$gender <- as.factor(customer_churn_dataset$gender)
customer_churn_dataset$SeniorCitizen <- as.factor(customer_churn_dataset$SeniorCitizen)
customer_churn_dataset$Partner <- as.factor(customer_churn_dataset$Partner)
customer_churn_dataset$Dependents <- as.factor(customer_churn_dataset$Dependents)
customer_churn_dataset$PhoneService <- as.factor(customer_churn_dataset$PhoneService)
customer_churn_dataset$MultipleLines <- as.factor(customer_churn_dataset$MultipleLines)
customer_churn_dataset$InternetService <- as.factor(customer_churn_dataset$InternetService)
customer_churn_dataset$OnlineSecurity <- as.factor(customer_churn_dataset$OnlineSecurity)
customer_churn_dataset$OnlineBackup <- as.factor(customer_churn_dataset$OnlineBackup)
customer_churn_dataset$DeviceProtection <- as.factor(customer_churn_dataset$DeviceProtection)
customer_churn_dataset$TechSupport <- as.factor(customer_churn_dataset$TechSupport)
customer_churn_dataset$StreamingTV <- as.factor(customer_churn_dataset$StreamingTV)
customer_churn_dataset$StreamingMovies <- as.factor(customer_churn_dataset$StreamingMovies)
customer_churn_dataset$Contract <- as.factor(customer_churn_dataset$Contract)
customer_churn_dataset$PaperlessBilling <- as.factor(customer_churn_dataset$PaperlessBilling)
customer_churn_dataset$PaymentMethod <- as.factor(customer_churn_dataset$PaymentMethod)
customer_churn_dataset$Churn <- as.factor(customer_churn_dataset$Churn)
}
# Save the converted dataset
write.csv(customer_churn_dataset, 'Customer_Churn_Dataset_Numerical.csv', row.names = FALSE)
View(customer_churn_dataset)
str(customer_churn_dataset)

#DATASET IS NUMERICAL, READY TO USE
####SOME CHARTS####
ggplot(data=customer_churn_dataset,aes(x=gender,fill=Churn))+
  geom_bar(position="fill")
ggplot(data=customer_churn_dataset,aes(x=SeniorCitizen,fill=Churn))+
  geom_bar(position="fill")
ggplot(data=customer_churn_dataset,aes(x=Dependents,fill=Churn))+
  geom_bar(position="fill")
ggplot(data=customer_churn_dataset,aes(x=tenure,fill=Churn))+
  geom_bar(position="fill")
ggplot(data=customer_churn_dataset,aes(x=PhoneService,fill=Churn))+
  geom_bar(position="fill")
ggplot(data=customer_churn_dataset,aes(x=MultipleLines,fill=Churn))+
  geom_bar(position="fill")
ggplot(data=customer_churn_dataset,aes(x=InternetService,fill=Churn))+
  geom_bar(position="fill")
ggplot(data=customer_churn_dataset,aes(x=OnlineSecurity,fill=Churn))+
  geom_bar(position="fill")
ggplot(data=customer_churn_dataset,aes(x=OnlineBackup,fill=Churn))+
  geom_bar(position="fill")
ggplot(data=customer_churn_dataset,aes(x=DeviceProtection,fill=Churn))+
  geom_bar(position="fill")
ggplot(data=customer_churn_dataset,aes(x=TechSupport,fill=Churn))+
  geom_bar(position="fill")
ggplot(data=customer_churn_dataset,aes(x=StreamingTV,fill=Churn))+
  geom_bar(position="fill")
ggplot(data=customer_churn_dataset,aes(x=StreamingMovies,fill=Churn))+
  geom_bar(position="fill")
ggplot(data=customer_churn_dataset,aes(x=Contract,fill=Churn))+
  geom_bar(position="fill")
ggplot(data=customer_churn_dataset,aes(x=PaperlessBilling,fill=Churn))+
  geom_bar(position="fill")
ggplot(data=customer_churn_dataset,aes(x=PaymentMethod,fill=Churn))+
  geom_bar(position="fill")
ggplot(data=customer_churn_dataset,aes(x=MonthlyCharges,fill=Churn))+
  geom_boxplot()
ggplot(data=customer_churn_dataset,aes(x=TotalCharges,fill=Churn))+
  geom_boxplot()
ggplot(data=customer_churn_dataset,aes(x=Churn,fill=TotalCharges))+
  geom_bar()
plot(customer_churn_dataset$Churn,xlab="Churn client",ylab="Frequency")

###LETS CHECK THE CORRELATION__CORRELATION ANALYSIS##########
#For continuos data 

#For categorical data
###Create a contingency table.
library(stats)
{contingency_table2 <- table(customer_churn_dataset$Churn,customer_churn_dataset$gender)
contingency_table3 <- table(customer_churn_dataset$Churn,customer_churn_dataset$SeniorCitizen)
contingency_table4 <- table(customer_churn_dataset$Churn,customer_churn_dataset$Partner)
contingency_table5 <- table(customer_churn_dataset$Churn,customer_churn_dataset$Dependents)
contingency_table6 <- table(customer_churn_dataset$Churn,customer_churn_dataset$tenure)
contingency_table7 <- table(customer_churn_dataset$Churn,customer_churn_dataset$PhoneService)
contingency_table8 <- table(customer_churn_dataset$Churn,customer_churn_dataset$MultipleLines)
contingency_table9 <- table(customer_churn_dataset$Churn,customer_churn_dataset$InternetService)
contingency_table10 <- table(customer_churn_dataset$Churn,customer_churn_dataset$OnlineSecurity)
contingency_table11<- table(customer_churn_dataset$Churn,customer_churn_dataset$OnlineBackup)
contingency_table12<- table(customer_churn_dataset$Churn,customer_churn_dataset$DeviceProtection)
contingency_table13<- table(customer_churn_dataset$Churn,customer_churn_dataset$TechSupport)
contingency_table14<- table(customer_churn_dataset$Churn,customer_churn_dataset$StreamingTV)
contingency_table15<- table(customer_churn_dataset$Churn,customer_churn_dataset$StreamingMovies)
contingency_table16<- table(customer_churn_dataset$Churn,customer_churn_dataset$Contract)
contingency_table17<- table(customer_churn_dataset$Churn,customer_churn_dataset$PaperlessBilling)
contingency_table18<- table(customer_churn_dataset$Churn,customer_churn_dataset$PaymentMethod)
contingency_table19<- table(customer_churn_dataset$Churn,customer_churn_dataset$MonthlyCharges)
contingency_table20<- table(customer_churn_dataset$Churn,customer_churn_dataset$TotalCharges)


# Perform the chi-square test
CH2 <- chisq.test(contingency_table2)
CH3 <- chisq.test(contingency_table3)
CH4 <- chisq.test(contingency_table4)
CH5 <- chisq.test(contingency_table5)
CH6 <- chisq.test(contingency_table6)
CH7 <- chisq.test(contingency_table7)
CH8 <- chisq.test(contingency_table8)
CH9 <- chisq.test(contingency_table9)
CH10 <- chisq.test(contingency_table10)
CH11<- chisq.test(contingency_table11)
CH12 <- chisq.test(contingency_table12)
CH13<- chisq.test(contingency_table13)
CH14<- chisq.test(contingency_table14)
CH15<- chisq.test(contingency_table15)
CH16<- chisq.test(contingency_table16)
CH17<- chisq.test(contingency_table17)
CH18<- chisq.test(contingency_table18)
CH19<- chisq.test(contingency_table19)
CH20<- chisq.test(contingency_table20)}
PearsonChiSQ<-data.frame(Test_table=NA,P_value=NA)
for(i in 2:20){
  a<-paste("CH", i, sep="")
PearsonChiSQ[i-1,1]<-paste("CH", i, sep="")
}
PearsonChiSQ$P_value<-c(CH2$p.value,CH3$p.value,
CH4$p.value,CH5$p.value,CH6$p.value,CH7$p.value,CH8$p.value,
CH9$p.value,CH10$p.value, 
CH11$p.value,CH12$p.value,CH13$p.value,CH14$p.value,
CH15$p.value,CH16$p.value,CH17$p.value, CH18$p.value,CH19$p.value,CH20$p.value)
PearsonChiSQ

# Split the dataset into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(customer_churn_dataset), nrow(customer_churn_dataset) * 0.7)
test_index <- (1:nrow(customer_churn_dataset))[-train_index]

# Create the training and testing sets
train_set <- customer_churn_dataset[train_index, ]
test_set <- customer_churn_dataset[test_index, ]

#DATASET HAS BEEN SPLITTED

# Create a histogram of the monthly charges feature
hist(train_set$MonthlyCharges)

# Create a boxplot of the Churn by type of contract
boxplot(Churn ~ Contract, data = train_set)

# Ensure categorical values in the training set are numeric
train_set$gender <- as.factor(train_set$gender)
train_set$InternetService <- as.factor(train_set$InternetService)
train_set$Contract <- as.factor(train_set$Contract)
train_set$PaymentMethod <- as.factor(train_set$PaymentMethod)

# And in test set
test_set$gender <- as.factor(test_set$gender)
test_set$InternetService <- as.factor(test_set$InternetService)
test_set$Contract <- as.factor(test_set$Contract)
test_set$PaymentMethod <- as.factor(test_set$PaymentMethod)

#First a linear model
m1 <- lm(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges, data = train_set)
summary(m1)

# Build a logistic regression model to predict customer churn
m2 <- glm(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges, data = train_set, family=binomial)
summary(m2)

#THE MODEL IS ALREADY DONE (BOTH LM AND GLM)

#Dropping non relevant variables
m3 <- glm(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges, data = train_set, family=binomial)
summary(m3)

m4 <- glm(Churn ~ gender+SeniorCitizen+Partner+Dependents+tenure+MultipleLines+InternetService+OnlineSecurity+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges, data = train_set, family=binomial)
summary(m4)

m5 <- glm(Churn ~ SeniorCitizen+Partner+Dependents+tenure+MultipleLines+InternetService+OnlineSecurity+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges, data = train_set, family=binomial)
summary(m5)

m6 <- glm(Churn ~ SeniorCitizen+Partner+Dependents+tenure+MultipleLines+InternetService+OnlineSecurity+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges, data = train_set, family=binomial)
summary(m6)

m7 <- glm(Churn ~ SeniorCitizen+Dependents+tenure+MultipleLines+InternetService+OnlineSecurity+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges, data = train_set, family=binomial)
summary(m7)

#BEST MODEL
m8 <- glm(Churn ~ Dependents+tenure+MultipleLines+InternetService+OnlineSecurity+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges, data = train_set, family=binomial)
summary(m8)
#BEST MODEL

m9 <- glm(Churn ~ Dependents+tenure+MultipleLines+InternetService+OnlineSecurity+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges, data = train_set, family=binomial)
summary(m9)

library(caret)
library(pROC)

newmodel <- train(
  form = Churn ~ SeniorCitizen+Dependents+tenure+MultipleLines+InternetService+OnlineSecurity+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges ,
  data = train_set,
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial"
)

# Make predictions on the test set
predictions <- predict(newmodel, newdata = test_set, type = "raw")

#MODEL IS TESTED AND PREDICTIONS ARE FOUND

install.packages("MLmetrics")
library(MLmetrics)

# Calculate the accuracy, precision, recall, and F1-score
accuracy <- mean(predictions == test_set$Churn)
precision <- precision(predictions, test_set$Churn)
recall <- recall(predictions, test_set$Churn)
f1 <- F1_Score(predictions, test_set$Churn)

# Print the evaluation metrics
print(paste('Accuracy:', accuracy))
print(paste('Precision:', precision))
print(paste('Recall:', recall))
print(paste('F1-score:', f1))

# Calculate the ROC AUC
roc_curve <- roc(as.numeric(test_set$Churn), as.numeric(predictions))
roc_auc <- auc(roc_curve)
print(paste('ROC:',roc_auc))


###IMBALANCE DATA 
#Lets check the imbalance in a table 
table(train_set$Churn)
#downtrain creation
ddown_train <- downSample(x = train_set,
                          y = train_set$Churn)
table(ddown_train$Class)  
##TRAINING BALANCED DATA

newmodelbalanced <- train(
  form = Churn ~ SeniorCitizen+Dependents+tenure+MultipleLines+InternetService+OnlineSecurity+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges ,
  data = ddown_train,
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial"
)
newmodelbalanced
# Make predictions on the test set
predictionsbalanced <- predict(newmodelbalanced, newdata = test_set, type = "raw")

#MODEL IS TESTED AND PREDICTIONS ARE FOUND

install.packages("MLmetrics")
library(MLmetrics)

# Calculate the accuracy, precision, recall, and F1-score
accuracyb <- mean(predictionsbalanced  == test_set$Churn)
precisionb <- precision(predictionsbalanced , test_set$Churn)
recallb <- recall(predictionsbalanced , test_set$Churn)
f1b <- F1_Score(predictionsbalanced , test_set$Churn)

# Print the evaluation metrics
print(paste('Accuracy:', accuracyb))
print(paste('Precision:', precisionb))
print(paste('Recall:', recallb))
print(paste('F1-score:', f1b))

# Calculate the ROC AUC
library(pROC)
rocb <- roc_(as.numeric(test_set$Churn), as.numeric(predictionsbalanced))
roc_curveb <- roc(as.numeric(test_set$Churn), as.numeric(predictionsbalanced))
roc_aucb <- auc(roc_curve)
print(paste('ROC:',roc_aucb))

plot(ddtrainbalanced)
