install.packages("Metrics")
install.packages("car")

library("Metrics")
library("car")

rm(list=ls())

setwd("~/uconn/course\ content/fall\ 2021/econ2311/")
insurance_data = read.csv("insurance.csv", header = TRUE)

sum(is.na(insurance_data))

insurance_data$sex[insurance_data$sex == "female"] <- 0
insurance_data$sex[insurance_data$sex == "male"] <- 1

insurance_data$smoker[insurance_data$smoker == "no"] <- 0
insurance_data$smoker[insurance_data$smoker == "yes"] <- 1

insurance_data$region[insurance_data$region == "northeast"] <- 0
insurance_data$region[insurance_data$region == "northwest"] <- 1
insurance_data$region[insurance_data$region == "southeast"] <- 2
insurance_data$region[insurance_data$region == "southwest"] <- 3

plot(insurance_data$age, insurance_data$charges, main="Age vs. Charges", 
     xlab="Age ", ylab="Charges", pch=19)

plot(insurance_data$sex, insurance_data$charges, main="Sex vs. Charges", 
     xlab="Sex ", ylab="Charges", pch=19)

plot(insurance_data$bmi, insurance_data$charges, main="BMI vs. Charges", 
     xlab="BMI ", ylab="Charges", pch=19)

plot(insurance_data$children, insurance_data$charges, main="# of Children vs. Charges", 
     xlab="# of Children ", ylab="Charges", pch=19)

plot(insurance_data$smoker, insurance_data$charges, main="Smoker Status vs. Charges", 
     xlab="Smoker Status", ylab="Charges", pch=19)

plot(insurance_data$region, insurance_data$charges, main="Geographic Region vs. Charges", 
     xlab="Geographic Region ", ylab="Charges", pch=19)

insurance_num <- as.data.frame(apply(insurance_data, 2, as.numeric))  # Convert all variable types to numeric
sapply(insurance_num, class)

typeof(insurance_num$age)
typeof(insurance_num$sex)
typeof(insurance_num$smoker)
typeof(insurance_num$region)
typeof(insurance_num$bmi)
typeof(insurance_num$children)

cor(insurance_num, method = c("pearson"))

smp_size <- floor(0.75 * nrow(insurance_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(insurance_data)), size = smp_size)

train <- insurance_data[train_ind, ]
test <- insurance_data[-train_ind, ]

model_1 = lm(charges~age+sex+smoker+region+bmi+children, data=train)

summary(model_1)

pred_1 = predict.lm(model_1, test)

MAE_1 = mae(test$charges, pred_1)


model_2 = lm(charges~age+smoker+bmi+children, data=train)

summary(model_2)

pred_2 = predict.lm(model_2, test)

MAE_2 = mae(test$charges, pred_2)


model_3 = lm(charges~age+smoker+bmi+children
             +age*smoker +age*bmi +age*children
             +smoker*bmi +smoker*children
             +bmi*children
             +age*smoker*bmi +age*smoker*children
             +smoker*bmi*children +age*bmi*children
             +age*smoker*bmi*children
             , data=train)

summary(model_3)

pred_3 = predict.lm(model_3, test)

MAE_3 = mae(test$charges, pred_3)


model_4 = lm(charges~age+smoker+bmi+children
             +age*smoker
             +age*bmi
             +smoker*children
             +age*smoker*bmi
             , data=train)

summary(model_4)

pred_4 = predict.lm(model_4, test)

MAE_4 = mae(test$charges, pred_4)

