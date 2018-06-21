# loading the required libraries
install.packages ("MASS")
install.packages ("car")
library ("MASS")
library("car")

###################################################################################################
##### Data understanding 
#1. Loading "Carprice" data 
carprice <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE)

carprice <- na.omit(carprice)
###################################################################################################
#checking first few records
head(carprice)

# structure of data shows the data types
str(carprice)

View(carprice)

#considering only company name as the independent variable for the model building.
carprice$CarName <- as.character(carprice$CarName)
CarCompany= sapply(strsplit(carprice$CarName, ' '), function(x) x[1])
carprice$CarCompany <- CarCompany

str(carprice)

#DUMMY VARIABLE CREATION.

# convert factors with 2 levels to numerical variables
str(carprice$fueltype)
summary(carprice$fueltype)
levels(carprice$fueltype)<-c(1,0)
carprice$fueltype<- as.numeric(levels(carprice$fueltype))[carprice$fueltype]

str(carprice$aspiration)
levels(carprice$aspiration)<-c(1,0)
carprice$aspiration<- as.numeric(levels(carprice$aspiration))[carprice$aspiration]

levels(carprice$doornumber)<-c(1,0)
carprice$doornumber<- as.numeric(levels(carprice$doornumber))[carprice$doornumber]

levels(carprice$carbody)<-c(1,0)
carprice$carbody<- as.numeric(levels(carprice$carbody))[carprice$carbody]

levels(carprice$enginelocation)<-c(1,0)
carprice$enginelocation<- as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]

# convert factors with 3 or more levels to numerical variables
str(carprice$carbody)
summary(carprice$carbody)

dummy_1 <- data.frame(model.matrix( ~carbody, data = carprice))
dummy_1 <- dummy_1[,-1]
carprice_1 <- cbind(carprice[,-7], dummy_1)
str(carprice_1)

dummy_2 <- data.frame((model.matrix( ~drivewheel, data= carprice_1)))
dummy_2 <- dummy_2[,-1]
carprice_2 <- cbind(carprice_1[,-7], dummy_2)
str(carprice_2)

dummy_3 <- data.frame((model.matrix( ~enginetype, data= carprice_2)))
dummy_3 <- dummy_3[,-1]
carprice_3 <- cbind(carprice_2[,-13], dummy_3)
str(carprice_3)

dummy_4 <- data.frame((model.matrix( ~cylindernumber, data= carprice_3)))
dummy_4 <- dummy_4[,-1]
carprice_4 <- cbind(carprice_3[,-13], dummy_4)
str(carprice_4)

dummy_5 <- data.frame((model.matrix( ~fuelsystem, data= carprice_4)))
dummy_5 <- dummy_5[,-1]
carprice_5 <- cbind(carprice_4[,-14], dummy_5)
str(carprice_5)
View(carprice_5)

carprice_final <- carprice_5[,-c(1:3)]
carprice_final <- carprice_final[c(19, 1:18, 20:44)]
carprice_final$CarCompany <- as.factor(carprice_final$CarCompany)
View(carprice_final)
str(carprice_final)



# Divide into training and test data set
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(carprice_final), 0.7*nrow(carprice_final))
# generate the train data set
train = carprice_final[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = carprice_final[-trainindices,]

##############

#Execute the first model_1 multilinear model in the training set. 
model_1 <-lm(CarCompany~.,data=train)

# Check the summary of model. 
summary(carprice_final)

# Check if the correlation matrix givessome insight.
corrs = cor(carprice_final)
View(corrs)
