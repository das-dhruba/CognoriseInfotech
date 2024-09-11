# The following report provides an analysis of unemployment rates across
# different states of India, with a focus on the impact of the COVID-19 
# pandemic. By exploring various dimensions of the dataset, including state-wise
# unemployment, urban vs. rural disparities, and labor participation rates, we
# aim to uncover patterns and insights that could inform policy decisions.


# The dataset contains monthly unemployment data from Kaggle spanning
# from 31st May, 2019 to 30th June 2020. 

# DATASET OVERVIEW

"Dataset Overview: Unemployment in India

The provided dataset delves into the unemployment landscape across diverse states in India:

States: Various states constituting the Indian subcontinent.
Date: The specific dates of unemployment rate recordings.
Measuring Frequency: The regularity of measurement collection (Monthly).
Estimated Unemployment Rate (%): The proportion of unemployed individuals in each Indian state.
Estimated Employed Individuals: The tally of presently engaged individuals.
Estimated Labour Participation Rate (%): The percentage of the working-age populace 
  (16-64 years) actively involved in the job market, including both employed individuals and 
  those actively seeking jobs."

"Step 1 : Loading the data"

# Install and load the readxl package
install.packages("readxl")
library(readxl)

# Load the data from the Excel file
data <- read_excel(file.choose())
data


"Step 2 : Cleaning and preprocessing the data "

"Duplicate entries were removed, and rows with missing values were omitted to
ensure the accuracy of the analysis. Date formats were standardized, and 
extraneous whitespace was trimmed from the 'Frequency' column."

installed.packages("dplyr")
library(dplyr)
# Remove duplicate rows
data <- distinct(data)

# Remove rows with all NAs
data <- na.omit(data)

# Convert the Date column to Date objects
data$Date <- as.Date(data$Date, format = "%d-%m-%Y")

# Strip leading/trailing whitespace from the Frequency column
data$Frequency <- trimws(data$Frequency)
data


"Step 3 : Exploratory data analysis"

# Summary statistics
summary(data$`Estimated Unemployment Rate (%)`)
sd(data$`Estimated Unemployment Rate (%)`)

"Unemployment Rate
The unemployment rate has a wide range, from 0% to 76.74%, with a mean around 11.79%.
The standard deviation is quite high at 10.72%, indicating significant variability.
50% of the data (the median) falls below 8.35%, which suggests that the distribution
   might be right-skewed with some very high unemployment rates pulling the mean upwards.
"

summary(data$`Estimated Employed`)
sd(data$`Estimated Employed`)

"Employed

The number of employed individuals also varies widely across the dataset.
The standard deviation is nearly as large as the mean, indicating a substantial 
    spread in the number of employed people across different regions or times.
The median is less than half of the maximum value, again suggesting a right-skewed 
   distribution."

summary(data$`Estimated Labour Participation Rate (%)`)
sd(data$`Estimated Labour Participation Rate (%)`)

"Labour Participation Rate

The labor participation rate has a mean of 42.63% with a standard deviation of 8.11%, 
    suggesting moderate variability.
The median is very close to the mean, which may indicate a more symmetric distribution
    of labor participation rates."


"Next, we should visualize these distributions and look at how the unemployment rate
changed over time, especially in light of the COVID-19 pandemic, as well as
differences between regions and urban vs. rural areas.

Let's start with visualizations for the distribution of the unemployment rate,
employed population, and labor participation rate. We'll use histograms
for this purpose."


# Install and load necessary packages
install.packages(c("ggplot2", "dplyr"))
library(ggplot2)
library(dplyr)

# Histogram of Estimated Unemployment Rate (%)
# Ensure the column is treated as numeric in R
data$`Estimated Unemployment Rate (%)` <- as.numeric(as.character(data$`Estimated Unemployment Rate (%)`))

# Create the histogram
ggplot(data, aes(x = `Estimated Unemployment Rate (%)`)) +
  geom_histogram(binwidth = 2.5, fill = "blue", colour = "black", alpha = 0.7) + 
  labs(title = "Distribution of Estimated Unemployment Rate (%)", x = "Unemployment Rate (%)", y = "Frequency")


"The distribution shows a peak at lower percentages, indicating that most data points 
    have a relatively low unemployment rate.
There is a long tail toward higher percentages, confirming the right-skewness
    suggested by the summary statistics.
The presence of very high unemployment rates may indicate extreme cases or 
    specific times of distress (such as the COVID-19 pandemic)."

# Convert the column to numeric, handling potential issues with factors or characters
data$`Estimated Labour Participation Rate (%)` <- as.numeric(as.character(data$`Estimated Labour Participation Rate (%)`))

# Create the histogram
ggplot(data, aes(x = `Estimated Labour Participation Rate (%)`)) +
  geom_histogram(binwidth = 3, fill = "blue", colour = "black", alpha = 0.7) + 
  labs(title = "Distribution of Estimated Labour Force Participation Rate (%)", 
       x = "Labour Participation Rate (%)", y = "Frequency")


"The distribution appears more symmetric than the other two, with a clear central
     peak and tails on both sides.
This suggests that the labor participation rate varies less dramatically across
    different regions or over time than the unemployment rate or the number 
    of employed individuals."

" Step 4"

"Next, let's analyze how the unemployment rate has changed over time. We'll 
    create a time series plot to visualize this trend. This will help us 
    understand if there were any significant changes that might be associated
    with the COVID-19 pandemic or other events. We'll also look at the 
    unemployment rate's trend in rural versus urban areas."

# Line plot for Unemployment Rate over time

ggplot(data, aes(x = Date, y = `Estimated Unemployment Rate (%)`, 
                 group = Area, color = Area)) +
  geom_line() 
labs(title = "Unemployment Rate Over Time by Area", x = "Date",
     y = "Unemployment Rate (%)")

"The time series plots provide the following insights:

Overall Trend:

The unemployment rate fluctuates over time, with some peaks and troughs indicating
  periods of higher and lower unemployment.
There is a noticeable spike around mid-2020, which aligns with the onset of the 
  COVID-19 pandemic and the subsequent lockdowns and economic disruptions.
  
By Area (Rural vs. Urban):

The plot shows the overall trend without specifically focusing on the 
COVID-19 period. The unemployment rate in urban areas seems to be generally higher
  than in rural areas.
The plot, highlights the impact of the COVID-19 pandemic more clearly. Both rural and urban areas 
experienced a significant increase in unemployment rates during the pandemic, 
with urban areas being more affected.
After the spike, there is a downward trend indicating a recovery phase,
but the rates have not returned to the pre-pandemic levels, especially in 
urban areas.
"
"step 5"

# State-wise analysis
state_unemployment <- data %>%
  group_by(Region) %>%
  summarize(Average_Unemployment = mean(`Estimated Unemployment Rate (%)`)) %>%
  arrange(Average_Unemployment)

ggplot(state_unemployment, aes(x = Average_Unemployment, y = reorder(Region, Average_Unemployment))) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(title = "Average Unemployment Rate by State", x = "Average Unemployment Rate (%)", y = "State")



# Load the required library
library(ggplot2)

# Create the line plot for Estimated Unemployed with separate lines for Rural and Urban Areas
ggplot(data, aes(x = as.Date(`Date`, format = "%d-%m-%Y"), y = `Estimated Employed`, color = Area)) +
  geom_line() +
  facet_wrap(~ Region) +
  labs(title = "Estimated Employed in Rural and Urban Areas Across States", 
       x = "Date", y = "Estimated Employed", color = "Area") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter the data for Delhi
delhi_data <- subset(data, Region == "Delhi")

# Create the line plot for Delhi showing trends in rural and urban areas
ggplot(delhi_data, aes(x = as.Date(`Date`, format = "%d-%m-%Y"), y = `Estimated Employed`, color = Area)) +
  geom_line(size = 1) +
  labs(title = "Trend of Estimated Employed in Delhi (Rural vs Urban)", 
       x = "Date", y = "Estimated Employed", color = "Area") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check for missing values in the Delhi data
sum(is.na(delhi_data$`Estimated Employed`))

# Create the line plot for Delhi showing trends in rural and urban areas, using LOESS for smoother trend lines
ggplot(delhi_data, aes(x = Date, y = `Estimated Employed`, color = Area)) +
  geom_line(size = 1) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", size = 0.8) +
  labs(title = "Trend of Estimated Employed in Delhi (Rural vs Urban)", 
       x = "Date", y = "Estimated Employed", color = "Area") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



"The bar chart shows the average unemployment rate for each state over the time
period covered by the dataset. Here are some observations:

There is a wide range in the average unemployment rates across different states,
indicating regional disparities in employment conditions.
Some states have notably higher average unemployment rates, while others have
managed to maintain lower averages.
This state-wise analysis is valuable for policymakers to identify which regions
may need more attention and resources to combat unemployment."

# Urban vs. Rural analysis
urban_rural_unemployment <- data %>%
  group_by(Area, Date) %>%
  summarize(Average_Unemployment = mean(`Estimated Unemployment Rate (%)`))

ggplot(urban_rural_unemployment, aes(x = Date, y = Average_Unemployment, group = Area, color = Area)) +
  geom_line() +
  labs(title = "Urban vs. Rural Unemployment Rate Over Time", x = "Date", y = "Average Unemployment Rate (%)")

"The line plot illustrates the urban versus rural unemployment rate trends over time:

Both urban and rural areas show fluctuations in unemployment rates, with urban 
areas generally experiencing higher rates than rural areas throughout the dataset's timeframe.
The spike around mid-2020, likely corresponding to the COVID-19 pandemic's impact,
is clearly visible in both urban and rural areas, with urban areas showing a sharper increase.
Post the spike, there appears to be a gradual decline, suggesting a recovery from
the peak unemployment rates experienced during the pandemic."


"Step 6"

# Pre and Post COVID-19 analysis
pre_pandemic <- filter(data, Date < as.Date("2020-03-01"))
post_pandemic <- filter(data, Date >= as.Date("2020-03-01"))

mean(pre_pandemic$`Estimated Unemployment Rate (%)`)
mean(post_pandemic$`Estimated Unemployment Rate (%)`)

"The comparison of average unemployment rates before and after the onset 
of the COVID-19 pandemic reveals:

Pre-pandemic average unemployment rate: approximately 9.51%
Post-pandemic average unemployment rate: approximately 17.77%
This indicates a significant increase in the unemployment rate following the onset of the
pandemic. The data suggests that the economic impact of COVID-19 was considerable,
nearly doubling the unemployment rate on average across the regions in the dataset."

# Simple Linear Regression
# Determine the relationship between two variables.
lm_model <- lm(`Estimated Unemployment Rate (%)` ~ Date, data = data)
summary(lm_model)

# Multiple Linear Regression and Logistic Regression
# Explore the relationship between Estimated Unemployment Rate (%) and multiple independent variables.
lm_model <- lm(`Estimated Unemployment Rate (%)` ~ `Estimated Labour Participation Rate (%)` + Area + Region, data = data)
summary(lm_model)

# Predict whether the unemployment rate is high (e.g., >7%) or low (<=7%) based on variables like Area and Region.
# Create a binary variable for High/Low unemployment
data$High_Unemployment <- ifelse(data$`Estimated Unemployment Rate (%)` > 7, 1, 0)

logit_model <- glm(High_Unemployment ~ `Estimated Labour Participation Rate (%)` + Area + Region, 
                   data = data, family = "binomial")
summary(logit_model)

# Time Series Analysis
# Analyze trends over time and forecast future unemployment rates using time series data.
# Model and forecast the Estimated Unemployment Rate (%) using time series models like ARIMA 
# or linear regression with time as a predictor.
install.packages("forecast")
library(forecast)

# Convert data to time series object
ts_data <- ts(data$`Estimated Unemployment Rate (%)`, start = c(2019, 5), frequency = 12)

# Fit an ARIMA model
arima_model <- auto.arima(ts_data)
forecast(arima_model)

# Difference in impact of Labor Participation Rate (%) on unemployment between rural and urban areas
interaction_model <- lm(`Estimated Unemployment Rate (%)` ~ `Estimated Labour Participation Rate (%)` * Area, data = data)
summary(interaction_model)

# Heteroscedasticity
# Test whether the variance of the residuals is constant (homoscedasticity) or not (heteroscedasticity)
# Load necessary libraries
install.packages("lmtest")
install.packages("sandwich")
library(lmtest)
library(sandwich)

# Fit a linear model
lm_model <- lm(`Estimated Unemployment Rate (%)` ~ `Estimated Labour Participation Rate (%)` + Area + Region, data = data)

# Breusch-Pagan Test for heteroscedasticity
bptest(lm_model)

# White's Test for heteroscedasticity
white_test <- bptest(lm_model, varformula = ~ fitted(lm_model) + I(fitted(lm_model)^2))
white_test

# Residuals vs Fitted Values plot is a common way to visually check for homoscedasticity
# Plot residuals vs. fitted values
plot(lm_model$fitted.values, lm_model$residuals,
     xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Autocorrelation
# Durbin-Watson Test: A commonly used to detect autocorrelation in the residuals of regression models.
# Durbin-Watson Test for autocorrelation
dw_test <- dwtest(lm_model)
dw_test

# Shapiro-Wilk Test for normality and Q-Q plots 
# Shapiro-Wilk Test for normality of residuals
shapiro_test <- shapiro.test(lm_model$residuals)
shapiro_test

# Q-Q Plot for visual inspection of normality
qqnorm(lm_model$residuals)
qqline(lm_model$residuals, col = "red")

# Multicollinearity
# Check for multicollinearity (i.e., high correlation between independent variables), 
# which can destabilize regression coefficients.
# variance Inflation Factor
install.packages("car")
library(car)

# Calculate VIF
vif(lm_model)
