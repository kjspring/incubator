# Question 3

library(caret)
library(rattle)
library(car)
library(ggplot2)

# Data from https://archive.ics.uci.edu/ml/datasets/Contraceptive+Method+Choice

dat_raw <- read.csv("contraceptive.csv")
dat <- dat_raw

# Add variable names
colnames(dat) <- c("wife_age", "wife_education", "husband_education",
                   "children_born", "wife_religion", "wife_working", 
                   "husband_occupation", "standard_of_living", "media_exposure",
                   "contraceptive_method")

## Recode data with descriptive data descriptions
dat$wife_education[dat$wife_education == 1] <- "no education"
dat$wife_education[dat$wife_education == 2] <- "some education"
dat$wife_education[dat$wife_education == 3] <- "high school"
dat$wife_education[dat$wife_education == 4] <- "college"

dat$husband_education[dat$husband_education == 1] <- "no education"
dat$husband_education[dat$husband_education == 2] <- "some education"
dat$husband_education[dat$husband_education == 3] <- "high school"
dat$husband_education[dat$husband_education == 4] <- "college"

dat$wife_religion[dat$wife_religion == 0] <- "non Islam"
dat$wife_religion[dat$wife_religion == 1] <- "Islam"

dat$wife_working[dat$wife_working == 0] <- "working"
dat$wife_working[dat$wife_working == 1] <- "not working"

dat$media_exposure[dat$media_exposure == 0] <- "good"
dat$media_exposure[dat$media_exposure == 1] <- "not good"

dat$contraceptive_method[dat$contraceptive_method == 1] <- "no use"
dat$contraceptive_method[dat$contraceptive_method == 2] <- "long term"
dat$contraceptive_method[dat$contraceptive_method == 3] <- "short term"

dat$standard_of_living[dat$standard_of_living == 1] <- "below poverty line"
dat$standard_of_living[dat$standard_of_living == 1] <- "middle income"
dat$standard_of_living[dat$standard_of_living == 1] <- "upper income"
dat$standard_of_living[dat$standard_of_living == 1] <- "extremely wealth"


## set class types
dat <- data.frame(lapply(dat, as.factor)) # set catagorical data
dat$wife_age <- as.numeric(as.character(dat$wife_age)) # set 
dat$children_born <- as.numeric(as.character(dat$children_born))

# Exploratory Data analysis plots

#scatterplotMatrix(dat)
fig1 <- ggplot(dat, aes(contraceptive_method, fill=wife_education) ) + geom_bar(position="dodge")
fig2 <- ggplot(dat, aes(contraceptive_method, fill=media_exposure) ) + geom_bar(position="dodge")
fig3 <- ggplot(dat, aes(contraceptive_method, fill=standard_of_living) ) + geom_bar(position="dodge")

## Split the data into training and testing data sets
inTrain <- createDataPartition(y=dat$contraceptive_method, p=0.70, list=F)
training <- dat[inTrain, ]
testing <- dat[-inTrain, ]
ctrl <- trainControl(method="repeatedcv", number = 10, repeats=10)
modFit <- train(contraceptive_method ~ ., data=training, method="rf", metric="Kappa", trControl = ctrl)

# tree_plot <- fancyRpartPlot(modFit$finalModel)

pred <- predict(modFit, testing)
testing$predRight <- pred == testing$contraceptive_method

pred_table <- table(pred, testing$contraceptive_method)
