library(MASS)
setwd("C:/Users/robbe/selftaught/kaggle")

testing <- read.csv("test.csv")
training <- read.csv("train.csv")
attach(training)
#remove less importent info
training <- subset(training, select =- c(PassengerId, Name, Ticket, Embarked, Cabin))
summary(training)

#looking for good estimates to model Age
boxplot(Age ~ Pclass)
boxplot(Age ~ SibSp)
boxplot(Age ~ Sex)

mod <- lm(Age ~ Pclass + SibSp + Sex)

#best model based on stepwise AIC:
stepAIC(mod) # best is complete model

#add age estimates to dataset based on model
#first subset the rows with missing age values
naAge <- subset(training, is.na(Age))
training <- subset(training, !is.na(Age))

#predict age values
y <- predict(mod,naAge)
NaAgeFilled <- as.data.frame(y)

#change all valuesbelow 0 to 0
NaAgeFilled <- within(NaAgeFilled, y[y<=0] <- 0)
naAge$Age <- NaAgeFilled$y

#recreate complete dataframe
training <- rbind(naAge, training)

#build model
fit <- glm(Survived ~ ., family = binomial(link = logit), data = training)
summary(fit)

#best model based on stepwise AIC:
stepAIC(fit)

fit <- glm(Survived ~ Pclass + Sex + Age + SibSp, family = binomial(link = logit), data = training)

#accuracy sensitivity andspecificity
prediction <- predict(fit, type = "response")
table(training$Survived, prediction >= 0.5)

accuracy <- (244 + 481) / nrow(training)
accuracy # 0.8136925

#preparing test dataset
detach(training)
attach(testing)

#add age estimates to test dataset
#first subset the rows with missing age values
naAge <- subset(testing, is.na(Age))
testing <- subset(testing, !is.na(Age))

y <- predict(mod, naAge)
NaAgeFilled <- as.data.frame(y)

#change all valuesbelow 0 to 0
NaAgeFilled <- within(NaAgeFilled, y[y<=0] <- 0)
naAge$Age <- NaAgeFilled$y

testing <- rbind(naAge, testing)

# predictions for test data based on the model
prediction <- predict(fit, type = "response", newdata = testing)
testing$Survived <- as.numeric(prediction >= 0.5)
table(testing$Survived)

final <- subset(testing, select = c(PassengerId, Survived))
write.csv(file = "titanicRobbe.csv", x = final, row.names=FALSE, quote=FALSE)




