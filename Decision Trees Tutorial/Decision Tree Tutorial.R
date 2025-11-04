# Load the necessary library for reading the data
library(readr)
# Load the caret package for stratified sampling
library(caret)
# Load the rpart package
library(rpart)


# Read the dataset
data <- read_csv("data/HSE_Procedure_Rates.csv")

# View the structure of the dataset
str(data)


# Convert categorical variables to factors
data$Year <- as.factor(data$Year)
data$Type_of_surgical_procedure <- as.factor(data$Type_of_surgical_procedure)
data$Case_type <- as.factor(data$Case_type)


# Split the data into non-break and break data
non_break_data <- subset(data, is.na(Flag))
break_data <- subset(data, Flag == "Break in series")
unique(data$Flag)





# Create a stratified split of the non-break data
set.seed(253) # for reproducibility
splitIndex <- createDataPartition(non_break_data$Type_of_surgical_procedure, p = 0.70, list = FALSE)
train_data <- non_break_data[splitIndex, ]
test_data <- non_break_data[-splitIndex, ]





# Build the decision tree model
tree_model <- rpart(VALUE ~ Year + Type_of_surgical_procedure + Case_type, data = train_data, method = "anova")




# Print the model summary
print(summary(tree_model))

# Plot the decision tree
plot(tree_model)
text(tree_model, use.n = TRUE)


# Predict values for the break in series data
break_data$predicted_VALUE <- predict(tree_model, newdata = break_data)

# View some of the predictions
head(break_data)


## Above performs poorly, so let's see why and improve. 



# Predict the test set
test_data$predicted_VALUE <- predict(tree_model, newdata = test_data)

# Calculate Mean Squared Error (MSE) and R-squared
mse <- mean((test_data$predicted_VALUE - test_data$VALUE)^2)
rsq <- 1 - sum((test_data$predicted_VALUE - test_data$VALUE)^2) / sum((mean(train_data$VALUE) - test_data$VALUE)^2)

# Print the MSE and R-squared values
print(paste("Mean Squared Error:", mse)) # Means 2697.367 differences can be expected
print(paste("R-squared:", rsq))

### MSE is very high, and as it stands our model can only ever explain a little over half the variance of the inputs. 
## Generally this can be seen as a bad model for the problem and is a good example of why you need to pick the right tool for any one particular job.
## That said, let's try to at least improve performance. 


# Perform 10-fold cross-validation
set.seed(123) # for reproducibility
cv_results <- train(VALUE ~ Year + Type_of_surgical_procedure + Case_type,
                    data = train_data,
                    method = "rpart",
                    trControl = trainControl("cv", number = 10))

# Print cross-validation results
print(cv_results) ## Best model has about a difference over under of 69, which is way better than the original.




second_model <- rpart(VALUE ~ Year + Type_of_surgical_procedure + Case_type,
                     data = non_break_data,  # Use the full non-break data
                     method = "anova",
                     control = rpart.control(cp = 0.05231421)) 

# Plot the decision tree
plot(second_model)
text(second_model, use.n = TRUE)


# Predict the VALUE for break in series data
break_data$predicted_VALUE <- predict(second_model, newdata = break_data)

# View some of the predictions
head(break_data)


## Our best performing result can only explain about 22% of the model, which indicates we may need to revisit how 
## This data is being used by the model, and how to better format it for understanding. 


##Given this is the case, we what would probably be some approaches we could do to reduce the issues with this model? 