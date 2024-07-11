#***********************************************************************************
## PART 1: Reading and setting up the data inputs
#***********************************************************************************

## Install and/or load all required R Packages
if(!require("data.table")) {install.packages("data.table"); library("data.table")}
if(!require("tidyverse")) {install.packages("tidyverse"); library("tidyverse")}
if(!require("haven")) {install.packages("haven"); library("haven")}
if(!require("Hmisc")) {install.packages("Hmisc"); library("Hmisc")}
if(!require("sjlabelled")) {install.packages("sjlabelled"); library("sjlabelled")}
if(!require("DescTools")) {install.packages("DescTools"); library("DescTools")}
if(!require("forcats")) {install.packages("forcats"); library("forcats")}
if(!require("conjoint")) {install.packages("conjoint"); library("conjoint")}
if(!require("cowplot")) {install.packages("cowplot"); library("cowplot")}

# Load the 'conjoint' library for conducting conjoint analysis
library(conjoint)

# Define attributes and levels for my specific case
attributes <- list(
  Availability = c("Supermarket", "Online"),
  Nutrition = c("10g protein per 100g", "20g protein per 100g", "30g protein per 100g"),
  Price = c("1.20€ per 100g", "2.40€ per 100g ", "3.60€ per 100g "),
  Taste = c("Meat-like taste", "Not meat-like taste"),
  Texture = c("Meat-like texture", "Not meat-like texture")
)

# Display all possible combinations
experiment <- expand.grid(attributes)

# Display the 'experiment' data frame to see the generated combinations
experiment 

# 1. Orthogonal Design
# Generate an orthogonal design using the 'caFactorialDesign' function
ortho.design <- caFactorialDesign(data = experiment, 
                                  type = "orthogonal", 
                                  seed = 123)

# Print the orthogonal design
ortho.design

# Check the quality of the orthogonal design using a correlation matrix.
cor(caEncodedDesign(ortho.design)) 

#***********************************************************************************
## PART 2: Reading and setting up the data inputs
#***********************************************************************************

library(tidyverse)
library(readxl)
library(GGally)

# 1. Conjoint dummies
# Import the conjoint orthogonal design output dummy file.
dummies <- read_excel("data/07072024_conjointdummies.xlsx")

# Check that the computer has read the file correctly, by taking a look at the first few lines of the "dummies" object.
head(dummies)

# Dummies is not in the right "format" needed for a regression, so the format needs to be converted.
dummies <- as.matrix(dummies)

# 2. Response (ratings) data
# Read the response (ratings) data. Call it "cdata"

cdata <- read_csv("data/11072024_qualtrics cleaned data.csv")

# Check that the computer has "read" the file correctly. Let's peak at the first few lines of the "cdata" object.
head(cdata)

# Converting the format.
cdata <- as.matrix(cdata)

# Separate columns 2-13 to have a matrix of only the conjoint rankings.
ratings =  as.matrix(cdata[,2:13]) # a matrix of the ratings data for each respondent

#***********************************************************************************
## PART 3: Run a regression for each respondent
#***********************************************************************************

# An empty matrix called "bhat" is created, where the regression coefficients (betas) from each regression are saved.
# It's dimensions are the following: one row for each respondent, one column for each beta coefficient in the regression.
# Then, a "for loop" is uses to run a linear regression for each respondent. 
# The coefficients for each respondent is stored in the corresponding row of the bhat matrix.

# The number of rows of bhat is equal to the number of respondents
NumRespondents = dim(cdata)[1] 

# The number of columns of bhat is the number of X's plus one for the intercept
NumBetas = dim(dummies)[2] +1 

#---------- #IS THERE A BUG HERE? ---------------------------------------------

# The line below creates an empty matrix, called bhat. 
# The dimensions of bhat are the following: the number of rows equal to the number of respondents, and the number of columns equal to the number of betas. 
bhat = matrix(NA,NumRespondents,NumBetas) 

# Do the regression for one respondent.
y.respondent.1 <- ratings[1,] 
linear.model.1 <- lm(y.respondent.1 ~ dummies) 
summary(linear.model.1)
linear.model.1$coef
linear.model.1$coefficients

# Now, run a regression for each respondent and save the betas in the matrix.
for(respondent in 1:NumRespondents){
    linear.model.respondent <- lm( ratings[respondent,] ~ dummies) #run the regression for one respondent
    bhat[respondent,] <- linear.model.respondent$coef #safe the coefficient in the row of bhat that corresponds to the respondent
}

# Let's take a peek at only the first few lines of bhat to see if the regression coefficients look OK.
head(bhat)

# Change the format of bhat into a "data frame" format and set names for each of its columns.
bhat = as.data.frame(bhat)
colnames(bhat) <- c("Intercept",colnames(dummies))

#----------------- # how do i get the one for all now?

#***********************************************************************************
## PART 4: Get attribute importances
#***********************************************************************************

getrange <- function(x) { max(x)-min(x) }

range.availability = apply(cbind(0,bhat[,1]),1,getrange)
range.nutrition = apply(cbind(0,bhat[,2:3]),1,getrange)
range.price = apply(cbind(0,bhat[,4:5]),1,getrange)
range.taste = apply(cbind(0,bhat[,6]),1,getrange)
range.texture = apply(cbind(0,bhat[,7]),1,getrange)

allranges = cbind(range.availability, range.nutrition, range.price, range.taste, range.texture)

AttrImportance = 100*allranges/rowSums(allranges) #attribute importance for each person.

#Let's take a peek at the first few lines.
head(AttrImportance) 

#Data visualization:
#Plot attribute importance for Price vs. Drink Vs. Food
plot(as.data.frame(AttrImportance)[,1:5],labels=c("Availability","Nutrition","Price","Taste","Texture"))


# do the same with GGally::ggpairs()
as.data.frame(AttrImportance) |> 
  ggpairs()

#***********************************************************************************
## PART 5: Testing hypotheses by segmenting
#***********************************************************************************




