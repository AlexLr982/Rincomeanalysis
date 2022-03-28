set.seed(12345)

#Importing the data set to R studio
proj1 <- read.csv("C:/Users/alexa/Downloads/proj1")

#Loading all needed packages at the top
library(caret)
library(RANN)
library(rpart)
library(rpart.plot)
#Creating a histogram of capital gain to view the distribution
hist(proj1$capital.gain)

#There are anomalous values at 99999 representing missing values
#Something should be done about these values
#Setting capital gain values of 99999 to NA
proj1$capital.gain[proj1$capital.gain == 99999] <- NA

#Checking to see that the previous command worked as intended
summary(proj1$capital.gain)

#Calculating the mean and SD
cgm <- mean(proj1$capital.gain, na.rm = TRUE)
cgsd <- sd(proj1$capital.gain, na.rm = TRUE)

#Replacing NA data with knnimputation
imputation_model <- preProcess(proj1, method = c("knnImpute"))
proj1.imp <- predict(imputation_model, proj1)

#Replacing the values in the original set with imputed values, because the caret package standardized the data it should be de-standardized by inverting the Z score standardization formula
proj1$capital.gain <- 
  ifelse(
    test = is.na(proj1$capital.gain) == TRUE,
    yes = proj1.imp$capital.gain * cgsd + cgm,
    no = proj1$capital.gain
  )
#Recalculating the mean and SD after imputation
cgimm <- mean(proj1$capital.gain)
cgimsd <- sd(proj1$capital.gain)

#Recreating the original data set without imputation for further analysis
proj1_copy <- read.csv("C:/Users/alexa/Downloads/proj1")
proj1_copy$capital.gain[proj1_copy$capital.gain == 99999] <- NA

#Creating a flag variable for capital gain NA's
proj1_copy$cgmiss <- 
  ifelse(
    test = is.na(proj1_copy$capital.gain) == TRUE,
    yes = 1,
    no = 0
  )

#Creating a contingency table with income as rows and cgmiss as columns
t1 <- table(proj1_copy$income, proj1_copy$cgmiss)
t1


#Making an ID column
proj1_copy$ID <- seq.int(nrow(proj1_copy))

#Checking the ID column works by pulling out the data for the 2001st record
proj1_copy[proj1_copy$ID == 2001,]
proj1_copy$marital.status  <- as.factor(proj1_copy$marital.status)

#Changing the name of marital status before creating a new variable
(names(proj1)[names(proj1)=="marital.status"] <- "marital.status.old")

#Checking what values make up the marital status column
summary(proj1$marital.status.old)

#Creating a new marital status variable that combines all married values in one called married and the rest into other using an elseif statement
proj1$marital.status <- ifelse(
  test = proj1$marital.status.old == "Married-civ-spouse" | proj1$marital.status.old == "Married-AF-spouse" | proj1$marital.status.old == "Married-spouse-absent",
  yes = "Married",
  no = "Other"
)

#Making a table with income as the rows and the new marital status as the columns
t2 <- table(proj1$income, proj1$marital.status)
round(prop.table(t2, 2) * 100, 2)
93.65 - 56.20

#Adding the cgmiss flag variable to the imputed original proj1 dataframe as a new variable cgimp which will as a flag for imputed values
proj1$cgimp <- proj1_copy$cgmiss

#creating a new variable capgl which is 1 for any record with any capital gain value or if the record has a capital loss value other than 0
proj1$capgl <- ifelse(
  test = proj1$capital.gain != 0 | proj1$capital.loss != 0, 
  yes = 1,
  no = 0
)

#Creating a table with income as the rows and and the new capgl variable as the columns
t3 <- table(proj1$income, proj1$capgl)
#Getting column proportions of the new table t3
round(prop.table(t3, 2) * 100, 2)

#Turning Income into a factor to get counts when performing a summary
proj1$income_cat <- as.factor(proj1$income)
summary(proj1$income_cat)
#Calculating proportion of high income
3554/14797

#Calculating the mean and Standard deviation of capital loss
clm <- mean(proj1$capital.loss)
clsd <- sd(proj1$capital.loss)
#Calculating upper outlier cutoff 
cloc <- clm + 3 * clsd

#Selecting the records which have capital loss exceeding this value
loss <- proj1[proj1$capital.loss > cloc,]
#Calculating the proportion of high income records with oulier loss values
summary(loss$income_cat)
353/679

#Creating a decision tree which will predict income based on education
eb <- rpart(formula = income ~ education,
            data = proj1,
            control = rpart.control(
              minbucket = .01 * nrow(proj1),
            maxdepth = 2))
#Plotting the tree to view the splits
rpart.plot(eb)
#Binning based on the splits made by the tree
educ.bin <- cut(proj1$education, breaks = c(0,13,14,16))

#Making a table with income as the rows and educbin as the columns 
t4 <- table(proj1$income, educ.bin)
t4
round(prop.table(t4, 2) * 100, 2)

#Creating a stacked bar chart of educ.bin with an income overlay
ggplot(proj1, aes(educ.bin)) + geom_bar(aes(fill = income), position = "stack") +
  xlab("Education binned") + ylab("Frequncy") + ggtitle("Bar chart of Binned Education with Income overlay")
#Creating a normalized bar chart to better show proportions of overlay
ggplot(proj1, aes(educ.bin)) + geom_bar(aes(fill = income), position = "fill") +
  xlab("Education binned") + ylab("Frequncy") + ggtitle(" Normalized Bar chart of Binned Education with Income")

#Creating a table with income rows and sex as columns with sums 
t5 <- table(proj1$income, proj1$sex)
t5
#This adds the sums
addmargins(t5)
#The same table with proportions
propt5 <- round(prop.table(t5, 2) * 100, 2)
#Adding just column sums this time
addmargins(propt5, 1)
