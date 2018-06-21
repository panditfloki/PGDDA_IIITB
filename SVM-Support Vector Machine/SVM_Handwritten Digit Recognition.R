#loading necessary libraries

library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(kernlab)
library(gridExtra)


# Loading the test and train data

mnist_train <- read_csv("mnist_train.csv",col_names = FALSE)
mnist_test <- read_csv("mnist_test.csv",col_names = FALSE)


# Reduce training size to cut down on time - subsample the training data
subsamplesize <- 2000
subsample <- sample(nrow(mnist_train), subsamplesize, replace=FALSE)
validationsize <- subsamplesize*0.3
validationsample <- subsample[1:validationsize]
trainsample <- subsample[(validationsize+1):subsamplesize]

mnistTrain <- mnist_train[trainsample,]
mnistTest <- mnist_test[sample(nrow(mnist_test), (subsamplesize*0.5)),]
write.csv(mnistTrain, file='mnist-train.csv', row.names=FALSE)
write.csv(mnistTest, file='mnist-test.csv', row.names=FALSE)


# Data Understanding and preparation


#Structure of the data
dplyr::glimpse(mnistTrain)


#peeking in to top few rows
head(mnistTrain[1:8])
head(mnistTest[1:8])


#names of all columns
names(mnistTrain)
names(mnistTest)

#we can understand that the first column X1 is our label 
#since it has only 0 to 10 values representing image labels


#Renaming the target column to label
colnames(mnistTrain)[colnames(mnistTrain)=="X1"] <- "label"
colnames(mnistTest)[colnames(mnistTest)=="X1"] <- "label"


#By looking at the summary statistics of the data we can understand that there is no negative pixels 
#and data founds to be consistent
summary(mnistTrain)


#Check for missing values
sapply(mnistTrain, function(x) sum(is.na(x)))
sapply(mnistTest, function(x) sum(is.na(x)))



# Plot the digits in a grid
previewNbyN <- 10

nsamples <- previewNbyN*previewNbyN

# Pick a sample from the training set
viewSample <- mnistTrain[sample(nrow(mnistTrain), nsamples),]

# Arrange the digit images in a grid
par( mfrow = c(previewNbyN, previewNbyN), mai = c(0,0,0,0))
imageRows <- 28
imageCols <- 28
for(i in 1:nsamples) {
  digit <- as.matrix(viewSample[i, 2:ncol(viewSample)], byrow=TRUE)
  dim(digit) <- c(imageRows, imageCols)
  image(digit[,ncol(digit):2], axes=FALSE, col=gray(255:0 / 255))
  text( 0.2, 0.8, viewSample[i,1], cex=2, col=3)
}



#Converting target column to factor
mnistTrain$label <- as.factor(mnistTrain$label)
mnistTest$label <- as.factor(mnistTest$label)



# let us create a new feature called intensity by taking mean of each row and
# Plot the intensity of each label
mnistTrain$intensity <- apply(mnistTrain[,-1], 1, mean) 

intbylabel <- aggregate (mnistTrain$intensity, by = list(mnistTrain$label), FUN = mean)

plot <- ggplot(data=intbylabel, aes(x=Group.1, y = x)) +
  geom_bar(stat="identity")
plot + scale_x_discrete(limits=0:9) + xlab("Label") + 
  ylab("Average Intensity")


#Let us plot the intensity distribution of all labels 
L0 <- qplot(subset(mnistTrain, label ==0)$intensity, binwidth = .75, 
            xlab = "Intensity Histogram for 0")

L1<- qplot(subset(mnistTrain, label ==1)$intensity, binwidth = .75,
           xlab = "Intensity Histogram for 1")

L2 <- qplot(subset(mnistTrain, label ==2)$intensity, binwidth = .75,
            xlab = "Intensity Histogram for 2")

L3 <- qplot(subset(mnistTrain, label ==3)$intensity, binwidth = .75,
            xlab = "Intensity Histogram for 3")


L4 <- qplot(subset(mnistTrain, label ==4)$intensity, binwidth = .75, 
            xlab = "Intensity Histogram for 4")

L5 <- qplot(subset(mnistTrain, label ==5)$intensity, binwidth = .75,
            xlab = "Intensity Histogram for 5")

L6 <- qplot(subset(mnistTrain, label ==6)$intensity, binwidth = .75,
            xlab = "Intensity Histogram for 6")

L7 <- qplot(subset(mnistTrain, label ==7)$intensity, binwidth = .75,
            xlab = "Intensity Histogram for 7")

L8 <- qplot(subset(mnistTrain, label ==8)$intensity, binwidth = .75,
            xlab = "Intensity Histogram for 8")

L9 <- qplot(subset(mnistTrain, label ==9)$intensity, binwidth = .75,
            xlab = "Intensity Histogram for 9")

grid.arrange(L0,L1 ,L2, L3,L4,L5,L6,L7,L8,L9)



###################################################################

#Constructing Model

mnistTrain$intensity<-NULL

#Using Linear Kernel
Model_linear <- ksvm(label~ ., data = mnistTrain, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, mnistTest)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,mnistTest$label)


#Using RBF Kernel
Model_RBF <- ksvm(label~ ., data = mnistTrain, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, mnistTest)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,mnistTest$label)


############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 5 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(7)
grid <- expand.grid(.sigma=c(0.015 ,0.025, 0.05), .C=c(0.1,0.5,1,2) )


#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm <- train(label~., data=mnistTrain, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)


# Constructing final model with sigma(gamma) = 0.05 and Cost(c) =0.1
Final_model <- ksvm(label~ ., data = mnistTrain, scale = FALSE, gamma=0.05,cost=0.1 ,kernel = "rbfdot")
Eval_Final_model<- predict(Final_model, mnistTest)

confusionMatrix(Eval_Final_model,mnistTest$label)
