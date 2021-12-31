#Loading packages ('randomForest' is the machine learning model and 'ggplot2' is for charts) 
library(randomForest)
library(ggplot2)

#Importing the data
train_set <- read.csv("train.csv", stringsAsFactors = T)
test_set <-  read.csv("test.csv", stringsAsFactors = T)

#Check missing data
colSums(is.na(train_set))
colSums(is.na(test_set))
colSums(train_set == "")
colSums(test_set == "")

#Create missing column in test_set
test_set$Survived <- NA

#Create a column to identify whether the data is training or testing data (both data frames must have the same columns/variables/features)
train_set$IsTrainSet <- T
test_set$IsTrainSet <- F

#Group the data frames using rbind
titanic_set <- rbind(train_set, test_set)

#Summary of the new data frame
summary(titanic_set)

#Check missing data again (now on the new dataframe)
colSums(is.na(titanic_set))
colSums(titanic_set == "")

#Simple transformations ETL/4Cs (Cleaning, completing, correting, creating)
titanic_set$Survived <- as.factor(titanic_set$Survived)
titanic_set$Pclass <- as.factor(titanic_set$Pclass)
titanic_set$Age[is.na(titanic_set$Age)] <- median(titanic_set$Age, na.rm = T)
titanic_set$SibSp <- as.numeric(titanic_set$SibSp)
titanic_set$Parch <- as.numeric(titanic_set$Parch)
titanic_set$Fare[is.na(titanic_set$Fare)] <- median(titanic_set$Fare, na.rm = T)
titanic_set$Embarked[titanic_set$Embarked==""] <- "S"
titanic_set$Embarked <- as.factor(as.character(titanic_set$Embarked))
table(titanic_set$Embarked)

#Now we're going to build the model. Split intro train and test
titanic_train <- titanic_set[titanic_set$IsTrainSet==T,]
titanic_test  <- titanic_set[titanic_set$IsTrainSet==F,]

#Creating a formula
survived_formula <- as.formula("Survived ~ Sex + Pclass + Age + SibSp + Parch + Fare + Embarked")

#Building the model
titanic_model <- randomForest(formula = survived_formula,
                              data = titanic_train,
                              ntree = 65,
                              importance = T)

#Interpreting results
titanic_model

#Show the error function of the model
plot(titanic_model)

#Generating a variable importance matrix
importance_var <-  importance(titanic_model, type=1)

importance_var
  
#Formatting the table
importance_table <- data.frame(features=row.names(importance_var),
                                 importance=importance_var[,1]);importance_table

#Creating the graph
model_graph <- ggplot(importance_table,
                  aes(x=reorder(features, importance), y=importance_var)) +
                  geom_bar(stat = "identity", fill="#5cc9c1") +
                  coord_flip() +
                  theme_light(base_size = 20) +
                  xlab("") +
                  ylab("Importance") +
                  ggtitle("Features importance in Random Forest model\nAccuracy of 82.94%") +
                  theme(plot.title = element_text(size = 18))

model_graph

#Preparing material for submission

submission <- data.frame(PassengerId = test_set$PassengerId,
                         Survived = predict(titanic_model, newdata = titanic_test))

#View the csv files and write it to a file for kaggle submission.
View(submission)

write.csv(submission, file = "titanic_kaggle.csv", row.names = F)
