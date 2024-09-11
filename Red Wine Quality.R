# DATASET OVERVIEW

"Dataset: Red Wine Quality"

# This Wine Quality dataset comprises 1,599 instances of red wine samples from the 
# Vinho Verde region in Portugal, each characterized by 12 physicochemical attributes. 
# These attributes include fixed acidity, volatile acidity, citric acid, residual 
# sugar, chlorides, free and total sulfur dioxide, density, pH, sulphates, and 
# alcohol content, all of which contribute to determining the wine's quality.
# This dataset provides a comprehensive basis for exploring the relationships between 
# chemical properties and perceived wine quality, offering valuable insights for research 
# in fields like economics, where understanding such phenomena can inform studies 
# on market preferences and product valuation.

# Install necessary packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrplot")
install.packages("GGally")
install.packages("effects")
install.packages("margins")

# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(corrplot)
library(GGally)
library(effects)

# Load the dataset
wine_data <- read_excel(file.choose())
wine_data

# View the first few rows of the dataset
head(wine_data)

# Summary statistics and structure of the data
summary(wine_data)
str(wine_data)

# Check for missing values
sum(is.na(wine_data))

# Exploratory Data Analysis (EDA)
# Correlation matrix
cor_matrix <- cor(wine_data)
corrplot(cor_matrix, method = "color", type = "full", tl.cex = 0.8, tl.col = "black", number.cex = 0.7)

# Distribution of the quality variable
ggplot(wine_data, aes(x = quality)) +
  geom_bar(fill = "steelblue") +
  ggtitle("Distribution of Wine Quality") +
  theme_minimal()

# Pairplot (GGally package)
ggpairs(wine_data, aes(color = as.factor(quality), alpha = 0.5))

# Trend analysis (Scatter plots with trend lines)
ggplot(wine_data, aes(x = alcohol, y = quality)) +
  geom_point(aes(color = as.factor(quality))) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Trend of Alcohol vs Quality") +
  theme_minimal()

ggplot(wine_data, aes(x = pH, y = quality)) +
  geom_point(aes(color = as.factor(quality))) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Trend of pH vs Quality") +
  theme_minimal()

# Regression Analysis
# Understand the relationships between variables, assess model performance, and explore the underlying data structure.

# 1. Residual Plot with LOESS Smoothing
"This plot helps us to assess whether the residuals (errors) of our regression model are randomly distributed, 
which is a key assumption in linear regression. The addition of a LOESS (Locally Estimated Scatterplot Smoothing) curve allows you to detect any 
non-linear patterns in the residuals."

# Fit the linear regression model
# Replace `quality` with your response variable and other variables with your predictors
wine_model <- lm(quality ~ alcohol + sulphates + pH, data = wine_data)

# Generate the Residual Plot with LOESS Smoothing
ggplot(wine_model, aes(.fitted, .resid)) +
  geom_point() +
  stat_smooth(method = "loess", col = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residual Plot with LOESS Smoothing") +
  theme_minimal()

# 2. Interaction Plot
"an interaction plot helps visualize how the effect of one predictor variable on the response variable 
changes depending on the level of another predictor variable."

# Generate the Interaction Plot 
interaction.plot(x.factor = wine_data$alcohol, 
                 trace.factor = wine_data$sulphates, 
                 response = wine_data$quality, 
                 col = rainbow(10), 
                 xlab = "Alcohol", 
                 ylab = "Quality",
                 trace.label = "Sulphates",
                 main = "Interaction Plot of Alcohol and Sulphates on Quality")

# 3. Cook's Distance Plot
"Cook's Distance helps identify influential observations that have a disproportionate impact on the fitted model."

plot(model, which = 4, cook.levels = c(0.5, 1.0), main = "Cook's Distance")

# 4. Effect Plot
"For models with interaction terms or polynomial terms, effect plots help visualize the estimated effects of 
the predictors on the response variable."

library(effects)
plot(allEffects(model), main = "Effects Plot")

# 5. Prediction Interval Plot
"This plot visualizes the predicted values along with their prediction intervals, 
helping to understand the uncertainty associated with predictions."

ggplot(wine_data, aes(x = alcohol, y = quality)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  ggtitle("Prediction Interval Plot") +
  theme_minimal()
# 6. Diagnostic Plots Panel
"This is a combined plot that shows various diagnostic plots like Residuals vs Fitted, Normal Q-Q, 
Scale-Location, and Residuals vs Leverage in one panel to assess the goodness-of-fit of the model."

par(mfrow = c(2, 2))
plot(model)

# 7. Marginal Effects Plot:
"Marginal effects plots show the predicted effect of a change in one predictor variable on the 
response variable, holding other predictors constant."

library(margins)
marginal_effects <- margins(model)
plot(marginal_effects)

# 8. Decision Boundary Plot (for Logistic Regression)
"For logistic regression models, we can plot the decision boundary to visualize how different 
classes are separated by the model."

library(ggplot2)
ggplot(wine_data, aes(x = alcohol, y = pH, color = as.factor(quality))) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  ggtitle("Decision Boundary Plot") +
  theme_minimal()


# Identifying Outliers
# 1. Boxplots
"Boxplots are a standard method to detect outliers visually. Data points outside the whiskers 
are typically considered outliers."

# Boxplot for each numeric feature in the dataset
boxplot(wine_data$alcohol, main = "Boxplot for Alcohol", ylab = "Alcohol Content")
boxplot(wine_data$quality, main = "Boxplot for Quality", ylab = "Quality")

# 2. IQR Method
"Outliers can be detected based on the Interquartile Range (IQR). Typically, any point more than 1.5 times
the IQR above the third quartile or below the first quartile is considered an outlier."

# Calculate Q1, Q3, and IQR for a specific feature
Q1 <- quantile(wine_data$alcohol, 0.25)
Q3 <- quantile(wine_data$alcohol, 0.75)
IQR <- Q3 - Q1

# Define outlier boundaries
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- wine_data$alcohol[wine_data$alcohol < lower_bound | wine_data$alcohol > upper_bound]
outliers

# 3. Using Z-Scores:
"Z-scores measure how many standard deviations a data point is from the mean. A common threshold 
to identify outliers is a Z-score greater than 3 or less than -3."

# Calculate Z-scores
wine_data$z_scores <- scale(wine_data$alcohol)

# Identify outliers
outliers <- wine_data[wine_data$z_scores > 3 | wine_data$z_scores < -3, ]
outliers


# Ploting Outliers

# Identify outliers using the IQR method
Q1 <- quantile(wine_data$alcohol, 0.25)
Q3 <- quantile(wine_data$alcohol, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

wine_data$outlier <- ifelse(wine_data$alcohol < lower_bound | wine_data$alcohol > upper_bound, TRUE, FALSE)

# Scatter plot with outliers highlighted
ggplot(wine_data, aes(x = alcohol, y = quality)) +
  geom_point(aes(color = outlier), size = 2) +
  scale_color_manual(values = c("black", "red")) +
  ggtitle("Scatter Plot of Alcohol vs Quality with Outliers Highlighted") +
  theme_minimal()


# Advanced Outlier detection models
# 1. Cook's distance
"Cook’s Distance is used to detect influential outliers that affect the regression model significantly."

# Fit a linear model
model <- lm(quality ~ alcohol + sulphates + pH, data = wine_data)

# Calculate Cook's Distance
cooks_dist <- cooks.distance(model)

# Plot Cook's Distance
plot(cooks_dist, pch = 20, main = "Cook's Distance", ylab = "Cook's Distance")

# Add a horizontal line at a threshold to identify influential points
abline(h = 4/(nrow(wine_data)-length(model$coefficients)), col = "red")

# 2. Density Plot with Outliers
"This plot combines the density of the data with outliers highlighted."

ggplot(wine_data, aes(x = alcohol)) +
  geom_density(fill = "lightblue") +
  geom_rug(aes(color = outlier), sides = "b") +
  scale_color_manual(values = c("black", "red")) +
  ggtitle("Density Plot of Alcohol Content with Outliers Highlighted") +
  theme_minimal()

# Ridge Regression 
"Ridge regression is used to prevent multicollinearity by adding a penalty to the size of coefficients. 
It’s particularly useful when the dataset has highly correlated predictors."

install.packages("glmnet")
library(glmnet)

# Prepare the data
X <- model.matrix(quality ~ ., wine_data)[,-1]
y <- wine_data$quality

# Fit a Ridge Regression model
ridge_model <- glmnet(X, y, alpha = 0)
summary(ridge_model)

# Cross-validation for Ridge Regression
cv_ridge <- cv.glmnet(X, y, alpha = 0)
plot(cv_ridge)

# Logistic Regression:
"Logistic regression is used when the dependent variable is binary. It predicts the 
probability that an instance belongs to a particular class."

# Convert the quality variable into a binary factor
wine_data$quality_binary <- ifelse(wine_data$quality > 5, 1, 0)

# Fit a Logistic Regression model
logistic_model <- glm(quality_binary ~ alcohol + sulphates + pH, data = wine_data, family = binomial)
summary(logistic_model)
plot(logistic_model)
     
# Quantile Regression
"Quantile regression estimates the median or other quantiles of the dependent variable. It is useful for 
understanding the impact of predictors across the distribution of the response variable."
install.packages("quantreg")
library(quantreg)

# Fit a Quantile Regression model
quantile_model <- rq(quality ~ alcohol + sulphates + pH, tau = 0.5, data = wine_data)
summary(quantile_model)

#  Bayesian Regression:
"Bayesian regression incorporates prior distributions along with the likelihood of the data to perform regression analysis."
install.packages("rstanarm")
library(rstanarm)

# Fit a Bayesian Linear Regression model
bayesian_model <- stan_glm(quality ~ alcohol + sulphates + pH, data = wine_data)
summary(bayesian_model)


