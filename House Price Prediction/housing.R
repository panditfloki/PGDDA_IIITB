#Linear Regression Model

housing <- read.csv("Housing.csv")

View(housing)

# datadictonary
# price represents the sale price of a house in Rs.
# area gives the total size of a property in square feet
# bedrooms represents the number of bedrooms
# bathrooms shows the number of bathrooms
# stories variable shows the number of stories excluding basement
# mainroad =1 if the house faces a main road
# livingroom   = 1 if the house has a separate living room or a drawing room for guests
# basement shows if the house has a basement
# hotwaterheating  = 1 if the house uses gas for hot water heating
# airconditioning    = 1 if there is central air conditioning
# parking shoes the number of cars that can be parked
# prefarea is 1 if the house is located in the preferred neighbourhood of the city


#Let us examine the structure of the dataset
str(housing)


#DUMMY VARIABLE CREATION. 
#Let us see the structure of variable "mainroad".
str(housing$mainroad)
summary(factor(housing$mainroad))

# One simple way to convert mainroad variable to numeric is to replace the levels- Yes's and Nos's with 1 and 0 is:
levels(housing$mainroad)<-c(1,0)

# Now store the numeric values in the same variable
housing$mainroad<- as.numeric(levels(housing$mainroad))[housing$mainroad]

# Check the summary of mainroad variable
summary(housing$mainroad)

# Do the same for other such categorical variables
levels(housing$guestroom)<-c(1,0)
housing$guestroom <- as.numeric(levels(housing$guestroom))[housing$guestroom]

levels(housing$basement)<-c(1,0)
housing$basement <- as.numeric(levels(housing$basement))[housing$basement]

levels(housing$hotwaterheating)<-c(1,0) 
housing$hotwaterheating <- as.numeric(levels(housing$hotwaterheating))[housing$hotwaterheating]

levels(housing$airconditioning)<-c(1,0)
housing$airconditioning <- as.numeric(levels(housing$airconditioning))[housing$airconditioning]

levels(housing$prefarea)<-c(1,0)
housing$prefarea <- as.numeric(levels(housing$prefarea))[housing$prefarea]


# Now we come across variables having more than 3 levels. 
summary(factor(housing$furnishingstatus))

#Converting "furnishingstatus" into dummies . 
dummy_1 <- data.frame(model.matrix( ~furnishingstatus, data = housing))

#check the dummy_1 data frame.
View(dummy_1)

#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "fusrnishingstatus". 
dummy_1 <- dummy_1[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "furnishingstatus" column
housing_1 <- cbind(housing[,-13], dummy_1)
View(housing_1)

##############

# Let us create the new metric and assign it to "areaperbedroom"
housing_1$areaperbedroom <- housing_1$area/housing_1$bedrooms
# metric - bathrooms per bedroom
housing_1$bbratio <- housing_1$bathrooms/housing_1$bedrooms

##############

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(housing_1), 0.7*nrow(housing_1))
# generate the train data set
train = housing_1[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = housing_1[-trainindices,]



##############

#Execute the first model_1 multilinear model in the training set. 
model_1 <-lm(price~.,data=train)

# Check the summary of model. 
summary(model_1)

# Check if the correlation matrix givessome insight.
corrs = cor(housing_1)
View(corrs)


##############
#For the calculation of VIF you need to install the package "car", 
# First let's install this package in your R system
install.packages("car")

# load the package
library("car")

# Pass the model_1 in the vif function
vif(model_1)

# Look at summary of the model again to see the P values
summary(model_1)
 
# remove bbratio variable based on High VIF and insignificance (p>0.05)
# Make a new model without bbratio variable

model_2 <- lm(formula = price ~ area + bedrooms + bathrooms + stories + mainroad + guestroom + 
                basement + hotwaterheating + airconditioning + parking + prefarea + 
                furnishingstatusunfurnished + furnishingstatussemi.furnished + areaperbedroom, data = train)


# check the accuracy of this model
summary(model_2)

# Repeat the process of vif calculation of model_2. 
vif(model_2)


# Remove the areaperbedroom variable based on high VIF and insignificance (p>0.05)
# Make a model without areaperbedroom variable
model_3 <- lm(formula = price ~ area + bedrooms + bathrooms + stories + mainroad + guestroom + 
                basement + hotwaterheating + airconditioning + parking + prefarea + 
                furnishingstatusunfurnished + furnishingstatussemi.furnished , data = train)

#Check the accuracy of this model
summary(model_3)

# Calculate the vif for model_3. 
vif(model_3)
# all VIFs are below 2, thus Multicollinearity is not a problem anymore


# Remove furnishingstatussemi.furnishingstatus based on insignificance. It has has the highest p value of 0.53 among the remaining variables
# Make a model without areaperbedroom variable

model_4 <- lm(formula = price ~ area + bedrooms + bathrooms + stories + mainroad + guestroom + 
                basement + hotwaterheating + airconditioning + parking + prefarea + 
                furnishingstatusunfurnished , data = train)

# Check the accuracy and p-values again
summary(model_4)


# bedrooms variable has a p value of 0.22, thus is insignificant
## Make a new model after removing bedrooms variable 

model_5 <- lm(formula = price ~ area + bathrooms + stories + mainroad + guestroom + 
                basement + hotwaterheating + airconditioning + parking + prefarea + 
                furnishingstatusunfurnished , data = train)

### Check the accuracy and p-values again
summary(model_5)


# basement variable is having the highest p-value (lowest significance) among the remaining variables.
# Make a new model after removing basement variable 

model_6 <- lm(formula = price ~ area + bathrooms + stories + mainroad + guestroom + 
                hotwaterheating + airconditioning + parking + prefarea + 
                furnishingstatusunfurnished , data = train)

### Check the accuracy and p-values again
summary(model_6)


# mainroad variable has the highest p-value among the other variables in the model
# Make a new model after removing mainroad variable 

model_7 <- lm(formula = price ~ area + bathrooms + stories + guestroom + 
     hotwaterheating + airconditioning + parking + prefarea + 
       furnishingstatusunfurnished , data = train)

# Check the adjusted R-squared and p-values again
summary(model_7)


# hotwater heating variable is having the highest p-value among the other variables in the model, 
# Make a new model after removing hotwaterheating variable 

model_8 <- lm(formula = price ~ area + bathrooms + stories + guestroom + 
                airconditioning + parking + prefarea + 
                furnishingstatusunfurnished , data = train)

#Check the accuracy
summary(model_8)


# Predict the house prices in the testing dataset
Predict_1 <- predict(model_8,test[,-1])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# check R-squared
rsquared