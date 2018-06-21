# Simple Linear regression 

#Importing dataset in R 
advertising <- read.csv("tvmarketing.csv")

# Now let's check the structure of advertising dataset. 
# structure of dataset

str(advertising)



# Let's quickly look at the plot of sales and TV spend
plot(advertising$TV, advertising$Sales)




### So set the seed to 100, let's run it 
set.seed(100)

#### You can use sample function for getting the indices of your 70% of observations. 
#### Let's execute this commad. 

trainindices= sample(1:nrow(advertising), 0.7*nrow(advertising))

### Now create an object "train.advertising" and store the 70% of the data of housing dataset 
# by just passing the indices inside the housing dataset

train.advertising = advertising[trainindices,]


### Similarly store the rest of the observations into an object "test".
### Let's execute both train and test commands

test = advertising[-trainindices,]





model<-lm(Sales~TV,data = train.advertising)

# The model is stored as an object in the variable 'model' 
# Now, we want to check the summary to analyse the results of the model using summary (model).

summary(model)

# A lot of information is popped by just checking the summary, 




# Now, execute this command
Predict_1 <- predict(model, test[-2])

##Next, append the predicted results with the test data set 
# to compare the actual prices with the predicted ones

test$test_sales <- Predict_1


r <- cor(test$Sales,test$test_sales)
rsquared <- r^2
rsquared
