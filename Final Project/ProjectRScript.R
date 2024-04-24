#######################################################
#####################################################
######
###### Data Analytics - Assignment 6
######          Nathan Crowell
######
#####################################################
#######################################################

#### Load in the data

hospital_inpatient_data = read.csv(file.choose(), header = T)
View(hospital_inpatient_data)

library(readxl)
other_financial_data = read_excel(file.choose())
View(other_financial_data)


#######################################################
#######################################################

#### EDA and Preprocessing on Hospital Inpatient Data:

library(ggplot2)

# create a temp variable to reload data easier
hospital_data = hospital_inpatient_data

# Summary statistics
summary(hospital_data)

# Check the structure
str(hospital_data)

# Missing values
colSums(is.na(hospital_data))

# Create the bar plot of SOI codes
ggplot(hospital_data, aes(x = as.factor(APR.Severity.of.Illness.Code))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of APR.Severity.of.Illness.Code", 
       x = "APR Severity of Illness Code", 
       y = "Frequency") +
  theme_minimal()

# Distribution of Discharges
ggplot(hospital_data, aes(x = Discharges)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Discharges",
       y = "Frequency")
# As we can tell from the graph, we need to remove the outliers of this column

# Distribution of Mean Charge
ggplot(hospital_data, aes(x = Mean.Charge)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Mean Charge",
       y = "Frequency")
# As we can tell from the graph, we need to remove the outliers of this column too

# Distribution of Mean Cost
ggplot(hospital_data, aes(x = Mean.Cost)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Mean Cost",
       y = "Frequency")
# As we can tell from the graph, we need to remove the outliers of this column too

# Average Mean.Charge by Year
yearly_mean_charge = aggregate(Mean.Charge ~ Year, data = hospital_data, FUN = mean)
ggplot(yearly_mean_charge, aes(x = Year, y = Mean.Charge)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Average Mean Charge per Year",
       x = "Year",
       y = "Average Mean Charge ($)") +
  theme_minimal()

# Average Mean.Cost by Year
yearly_mean_cost = aggregate(Mean.Cost ~ Year, data = hospital_data, FUN = mean)
ggplot(yearly_mean_cost, aes(x = Year, y = Mean.Cost)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Average Mean Cost per Year",
       x = "Year",
       y = "Average Mean Cost ($)") +
  theme_minimal()

#### Removing Outliers in the data

# Function to turn all outliers into NAs using IQR
remove_outliers <- function(x) {
  quartiles <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
  dist <- 1.5 * IQR(x, na.rm = TRUE)
  x[x < (quartiles[1] - dist)] <- NA
  x[x > (quartiles[2] + dist)] <- NA
  return(x)
}

# Removing the outliers from Discharges, Mean Charge, and Mean Cost
hospital_data$Discharges <- remove_outliers(hospital_data$Discharges)
hospital_data$Mean.Charge <- remove_outliers(hospital_data$Mean.Charge)
hospital_data$Mean.Cost <- remove_outliers(hospital_data$Mean.Cost)

# now remove the NAs after making all outliers NA
hospital_data = na.omit(hospital_data)

# Create the bar plot of SOI codes
ggplot(hospital_data, aes(x = as.factor(APR.Severity.of.Illness.Code))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of APR.Severity.of.Illness.Code", 
       x = "APR Severity of Illness Code", 
       y = "Frequency") +
  theme_minimal()

# Distribution of Discharges w/o outliers
ggplot(hospital_data, aes(x = Discharges)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Discharges",
       y = "Frequency")
# it seems that the distribution is close to an exponential distribution

# Distribution of Mean Charge w/o outliers
ggplot(hospital_data, aes(x = Mean.Charge)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Mean Charge",
       y = "Frequency")
# it seems that the distribution is close to a weibull distribution

# Distribution of Mean Cost w/o outliers
ggplot(hospital_data, aes(x = Mean.Cost)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of Mean Cost",
       y = "Frequency")
# it seems that the distribution is close to a weibull distribution

# Average Mean.Charge by Year
yearly_mean_charge = aggregate(Mean.Charge ~ Year, data = hospital_data, FUN = mean)
ggplot(yearly_mean_charge, aes(x = Year, y = Mean.Charge)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Average Mean Charge per Year",
       x = "Year",
       y = "Average Mean Charge ($)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(yearly_mean_charge$Year), max(yearly_mean_charge$Year), by = 1),
                     limits = c(min(yearly_mean_charge$Year), max(yearly_mean_charge$Year)))

# Average Mean.Cost by Year
yearly_mean_cost = aggregate(Mean.Cost ~ Year, data = hospital_data, FUN = mean)
ggplot(yearly_mean_cost, aes(x = Year, y = Mean.Cost)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Average Mean Cost per Year",
       x = "Year",
       y = "Average Mean Cost ($)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(yearly_mean_cost$Year), max(yearly_mean_cost$Year), by = 1),
                     limits = c(min(yearly_mean_cost$Year), max(yearly_mean_cost$Year)))

#######################################################

#### EDA on Financial Data:

# create temp variable to reload data easier
financial_data = other_financial_data

# Summary statistics
summary(financial_data)

# Check the structure
str(financial_data)

# Missing values
colSums(is.na(financial_data))

# Average CPI over years
ggplot(financial_data, aes(x = Year, y = `Average CPI`)) +
  geom_line(color = "blue") +
  labs(title = "Average CPI over Years") +
  scale_x_continuous(breaks = seq(min(financial_data$Year), max(financial_data$Year), by = 1),
                     limits = c(min(financial_data$Year), max(financial_data$Year)))

# GDP over years
ggplot(financial_data, aes(x = Year, y = `GDP (billions)`)) +
  geom_line(color = "black") +
  labs(title = "GDP over Years") +
  scale_x_continuous(breaks = seq(min(financial_data$Year), max(financial_data$Year), by = 1),
                     limits = c(min(financial_data$Year), max(financial_data$Year)))

# Housing Price Inflation over years
ggplot(financial_data, aes(x = Year, y = `Housing Price Inflation (%)`)) +
  geom_line(color = "red") +
  labs(title = "Housing Price Inflation over Years") +
  scale_x_continuous(breaks = seq(min(financial_data$Year), max(financial_data$Year), by = 1),
                     limits = c(min(financial_data$Year), max(financial_data$Year)))

# Inflation over years
ggplot(financial_data, aes(x = Year, y = Inflation)) +
  geom_line(color = "orange") +
  labs(title = "Inflation over Years") +
  scale_x_continuous(breaks = seq(min(financial_data$Year), max(financial_data$Year), by = 1),
                     limits = c(min(financial_data$Year), max(financial_data$Year)))

# Personal Income per capita over years
ggplot(financial_data, aes(x = Year, y = `Personal Income per capita`)) +
  geom_line(color = "purple") +
  labs(title = "Personal Income per capita over Years") +
  scale_x_continuous(breaks = seq(min(financial_data$Year), max(financial_data$Year), by = 1),
                     limits = c(min(financial_data$Year), max(financial_data$Year)))


#######################################################
#######################################################

#### Merge the 2 datasets into 1 for easier analysis
#### and create a customer mean up charge column (mean charge - mean cost)
#### then drop the charge and cost columns

# Merge the datasets based on the 'Year' column
merged_data = merge(hospital_data, financial_data, by = "Year")

# Create the Mean Up charge column
merged_data$Mean.Up.Charge = merged_data$Mean.Charge - merged_data$Mean.Cost

View(merged_data)

# Average Up Charge by Year
yearly_mean_up_charge = aggregate(Mean.Up.Charge ~ Year, data = merged_data, FUN = mean)
ggplot(yearly_mean_up_charge, aes(x = Year, y = Mean.Up.Charge)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Average Mean Up Charge per Year",
       x = "Year",
       y = "Average Mean Up Charge ($)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(yearly_mean_up_charge$Year), max(yearly_mean_up_charge$Year), by = 1),
                     limits = c(min(yearly_mean_up_charge$Year), max(yearly_mean_up_charge$Year)))

# Average Up Charge by SOI code
soi_mean_up_charge = aggregate(Mean.Up.Charge ~ APR.Severity.of.Illness.Code, data = merged_data, FUN = mean)
ggplot(soi_mean_up_charge, aes(x = APR.Severity.of.Illness.Code, y = Mean.Up.Charge)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Mean Up Charge per SOI",
       x = "Severity of Illness Code (SOI)",
       y = "Average Mean Up Charge ($)") +
  theme_minimal()

#######################################################
#######################################################

#### Model the data

# create a new dataset that has only the columns we want to use for modeling (for simplicity)
modeling_data = subset(merged_data, 
                        select = c(Year, APR.Severity.of.Illness.Code, APR.Medical.Surgical.Code, 
                                   Discharges, `Average CPI`, `GDP (billions)`, 
                                   `Housing Price Inflation (%)`, Inflation, 
                                   `Personal Income per capita`, Mean.Up.Charge))

# perform One-Hot Encoding on APR.Medical.Surgical.Code, and add new column to dataset
modeling_data = cbind(modeling_data, model.matrix(~ APR.Medical.Surgical.Code - 1, data = modeling_data))

# Remove the original APR.Medical.Surgical.Code and APR.Medical.Surgical.CodeP (redundancy) column
modeling_data = subset(modeling_data, select = -c(APR.Medical.Surgical.Code, APR.Medical.Surgical.CodeP))

# check correlation before scaling occurs
cor(modeling_data)

# standardize the dataset
modeling_data[,-9] = scale(modeling_data[,-9])

library(ISLR)

cor(modeling_data)

# perform a train/test split of 70/30
set.seed(123) # for re-producability
train_indices = sample(1:nrow(modeling_data), size = 0.7*nrow(modeling_data), replace = F)

# now create X and y splits with train/test splits
train_data = modeling_data[train_indices,]
test_data = modeling_data[-train_indices,]


#######################################################

## Linear Regression Model
set.seed(123)
LR_model = lm(Mean.Up.Charge ~ ., data = train_data)

summary(LR_model)
# from this, we notice that the only feature where we fail to reject the null hypothesis that the
# coefficient is equal to 0, is the inflation feature. So, we assume that every other feature
# has an effect on the Mean.Up.Charge

# plot(LR_model) # this takes a very long time, so it remains commented out until needed.

# predictions using test_data
LR_pred = predict(LR_model, newdata = test_data)

# Evaluate the Model
LR_RMSE = sqrt(mean((LR_pred - test_data$Mean.Up.Charge)^2))
print(paste("Root Mean Squared Error:", LR_RMSE))

#######################################################

## rpart Model

library(rpart)

set.seed(123)
rpart_model = rpart(Mean.Up.Charge ~ ., data = train_data, method = "anova")
plot(rpart_model)
text(rpart_model)
# here, we notice that the 'important' features are SOI code and MS code.

summary(rpart_model)

# Predictions
rpart_pred = predict(rpart_model, test_data)

# Evaluate the Model
rpart_RMSE = sqrt(mean((rpart_pred - test_data$Mean.Up.Charge)^2))
print(paste("Root Mean Squared Error:", rpart_RMSE))

#######################################################

## Random Forest Model

## THIS MODEL IS COMMENTED OUT BECAUSE MY COMPUTER DOES NOT HAVE THE PROCESSING
## POWER TO FIT THIS MODEL ON THE TRAIN_DATA

#library(randomForest)

#rf_model = randomForest(Mean.Up.Charge~., modeling_data, subset = train_indices)

#library(caret)

#rf_model = train(Mean.Up.Charge ~ ., data = train_data, method = "rf", metric = "RMSE", trControl = trainControl(method = "cv", number = 5))

#######################################################

## Gradient Boosting Model

library(gbm)

set.seed(123)
gbm_model = gbm(Mean.Up.Charge ~ ., data = train_data, distribution = "gaussian", n.trees = 100)

summary(gbm_model)

plot(gbm_model)

# predict and evaluate the model
gbm_pred = predict(gbm_model, test_data)
gbm_RMSE = sqrt(mean((gbm_pred - test_data$Mean.Up.Charge)^2))
print(paste("GBM Root Mean Squared Error:", gbm_RMSE))

#######################################################

## XGBoost Model

library(xgboost)

train_matrix = xgb.DMatrix(data = as.matrix(train_data[,-20]), label = train_data$Mean.Up.Charge)
test_matrix = xgb.DMatrix(data = as.matrix(test_data[,-20]), label = test_data$Mean.Up.Charge)

xgb_model = xgboost(data = train_matrix, nrounds = 25, objective = "reg:squarederror")
# here, the output shows the RMSE getting down to ~55, which is significantly better than the other models

summary(xgb_model)

# predict and evaluate the model
xgb_pred = predict(xgb_model, test_matrix)
xgb_RMSE = sqrt(mean((xgb_pred - test_data$Mean.Up.Charge)^2))
print(paste("XGBoost Root Mean Squared Error:", xgb_RMSE))

#######################################################

## SVR Model

## THIS MODEL IS COMMENTED OUT BECAUSE MY COMPUTER DOES NOT HAVE THE PROCESSING
## POWER TO FIT THIS MODEL ON THE TRAIN_DATA

#library(e1071)


#svr_model = svm(Mean.Up.Charge ~ ., data = train_data, kernel = "linear")

## predict and evaluate
#svr_pred = predict(svr_model, test_data)
#svr_RMSE = sqrt(mean((svr_pred - test_data$Mean.Up.Charge)^2))
#print(paste("XGBoost Root Mean Squared Error:", svr_RMSE))


#######################################################
#######################################################




