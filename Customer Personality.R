# DATASET OVERVIEW

"Dataset: Customer Performance Anlysis"
# Customer Personality Analysis is a detailed analysis of a company’s ideal customers. 
# It helps a business to better understand its customers and makes it easier for them 
# to modify products according to the specific needs, behaviors and concerns of different types of customers.

# Install necessary packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("car")
install.packages("lmtest")
install.packages("psych")
install.packages("reshape2")
install.packages("cluster")
install.packages("corrplot")

# Load necessary libraries
library(readxl)
library(ggplot2)
library(corrplot)
library(dplyr)
library(tidyr)
library(lubridate)
library(car)
library(lmtest)
library(psych)
library(cluster)

# Load the dataset
cust_data <- read_excel(file.choose())
cust_data
colnames(cust_data)
summary(cust_data)

# Check the structure of the data
str(data)

# Check the first few values in the Year_Birth column
head(cust_data$Year_Birth)
head(cust_data$Income)

# Check for missing values
sum(is.na(cust_data$Year_Birth))

cust_data$Year_Birth <- as.numeric(cust_data$Year_Birth)
cust_data$Income <- as.numeric(cust_data$Income)
cust_data$Dt_Customer <- as.Date(cust_data$Dt_Customer, format="%d-%m-%Y")

# Handle missing values if any
cust_data <- na.omit(data)

# Summary statistics
summary(cust_data)

# Visualize age distribution
ggplot(cust_data, aes(x = Year_Birth)) + geom_histogram(binwidth = 5, fill = "blue", color = "black")

# Visualize income distribution
ggplot(cust_data, aes(x = Income)) + geom_histogram(binwidth = 10000, fill = "green", color = "black")

# Correlation matrix
cor_matrix <- cor(data[,sapply(data, is.numeric)])
print(cor_matrix)

# Regression model predicting total amount spent based on income, age, and number of children
model <- lm(MntWines + MntFruits + MntMeatProducts + MntFishProducts + MntSweetProducts + MntGoldProds ~ 
              Income + Year_Birth + Kidhome + Teenhome, data = data)

summary(model)
plot(model)

# Checking for multicollinearity using VIF
vif_values <- vif(model)
print(vif_values)

# Select only numeric columns
numeric_columns <- data[, sapply(data, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_columns, use = "complete.obs")

# Plot the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         col = colorRampPalette(c("blue", "white", "red"))(200))

# Convert the correlation matrix to a long format
library(reshape2)
cor_data <- melt(cor_matrix)

# Plot the heatmap using ggplot2
ggplot(cor_data, aes(Var1, Var2, fill = value)) + 
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) + 
  coord_fixed()

# Inference: Most customers are well-educated, with 'Graduation' being the most common.

# Marital Status of Customers
ggplot(cust_data) +
  geom_bar(aes(x = reorder(Marital_Status, Marital_Status, function(x)-length(x))),
           fill = "purple") +
  theme_minimal() +
  ggtitle("Marital Status of Customers") +
  xlab("Marital Status") +
  ylab("Count") +
  coord_flip()

# Inference: A significant number of customers are married or living together.

# Total Spending in Different Categories
cust_data$Total_Spending <- rowSums(cust_data[,c("MntWines", "MntFruits", "MntMeatProducts", 
                      "MntFishProducts", "MntSweetProducts", "MntGoldProds")])

# Relationships Between Demographics and Spending Habits

# Total Spending vs Age
ggplot(data, aes(x = Age, y = Total_Spending)) +
  geom_point(color = "red") +
  theme_minimal() +
  ggtitle("Total Spending vs Age") +
  xlab("Age") +
  ylab("Total Spending")

# Inference: Middle-aged customers tend to spend more, with a decline in spending
# for older age groups.

# Total Spending vs Income
ggplot(data, aes(x = Income, y = Total_Spending)) +
  geom_point(color = "red") +
  theme_minimal() +
  ggtitle("Total Spending vs Income") +
  xlab("Income") +
  ylab("Total Spending")

# Inference: Positive correlation between income and total spending.

# Total Spending by Education Level
ggplot(data, aes(x = Total_Spending, y = Education)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  ggtitle("Total Spending by Education Level") +
  xlab("Total Spending") +
  ylab("Education")

# Inference: Higher education levels, particularly PhD, correspond to higher spending.

# Total Spending by Marital Status
ggplot(data, aes(x = Total_Spending, y = Marital_Status)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  ggtitle("Total Spending by Marital Status") +
  xlab("Total Spending") +
  ylab("Marital Status")

# Inference: Marital status does not significantly affect spending habits.

# Correlation Analysis
numeric_columns <- sapply(data, is.numeric)
correlation_matrix <- cor(data[, numeric_columns], use = "complete.obs")
corrplot(correlation_matrix, method = "color")

#Inference 
"Income and Total Spending: 
There is a strong positive correlation between 'Income' and 'Total Spending'.
This indicates that as income increases, total spending on products also tends
to increase.

Age and Spending:
Age shows moderate correlations with different spending categories. For example,
there might be a noticeable relationship between age and spending on wines.

Recency and Spending :
The 'Recency' variable, indicating the number of days since the last purchase,
shows low to moderate negative correlations with spending categories. 
This might suggest that recent customers tend to spend more."

# Display column names to check for issues
colnames(cust_data)

# Perform ANOVA
anova_result <- aov(Income ~ Marital_Status, data = cust_data)

# Summary of the ANOVA test
summary(anova_result)

"Interpretation:
If the p-value is less than 0.05, you reject the null hypothesis and conclude that 
there is a significant difference in average income across at least one pair of marital statuses."

# One-Sample Proportion z-Test

# Assuming 'Response' indicates whether the customer accepted the last campaign (1 = yes, 0 = no)
# Test if the proportion of responses is 50%

prop_test_result <- prop.test(sum(cust_data$Response), n = nrow(data), p = 0.5)

# Summary of the proportion test
prop_test_result

# Perform the one-sample t-test
t_test_result <- t.test(cust_data$MntWines, mu = 300, alternative = "greater")

# Print the result
t_test_result

# Extract the t-statistic and degrees of freedom
t_stat <- t_test_result$statistic
df <- t_test_result$parameter


# Parameters
alpha <- 0.05
t_stat <- t_test_result$statistic
df <- t_test_result$parameter
t_critical <- qt(1 - alpha, df)

# Sequence for t-distribution
x <- seq(-4, 4, length = 1000)
y <- dt(x, df)

# Plot the t-distribution
plot(x, y, type = "l", lwd = 2, col = "blue",
     main = "T-Distribution with Acceptance and Rejection Regions",
     xlab = "t value", ylab = "Density")

# Shade the rejection region (right-tail)
polygon(c(t_critical, seq(t_critical, 4, length = 1000), 4),
        c(0, dt(seq(t_critical, 4, length = 1000), df), 0),
        col = rgb(1, 0, 0, 0.5), border = NA)

# Shade the acceptance region (left of the critical value)
polygon(c(-4, seq(-4, t_critical, length = 1000), t_critical),
        c(0, dt(seq(-4, t_critical, length = 1000), df), 0),
        col = rgb(0, 1, 0, 0.5), border = NA)

# Add vertical line for the critical value
abline(v = t_critical, col = "red", lty = 2)

# Add vertical line for the t-statistic
abline(v = t_stat, col = "black", lty = 2, lwd = 2)

# Add text annotations
text(t_critical + 0.4, max(y)/2, "Rejection Region", col = "red", cex = 0.8)
text(-3.5, max(y)/2, "Acceptance Region", col = "green", cex = 0.8)
text(t_stat + 0.4, max(y)/3, paste("t = ", round(t_stat, 2)), col = "black", cex = 0.8)

# Filter the dataset for "Married" and "Single" groups
subset_data <- cust_data[cust_data$Marital_Status %in% c("Married", "Single"), ]

# Check the filtered data
table(subset_data$Marital_Status)

# Perform the F-test for variance
f_test_result <- var.test(MntWines ~ Marital_Status, data = subset_data)

# Print the result
f_test_result

# Install necessary packages
install.packages("plotly")
install.packages("rgl")


# Load the necessary libraries
library(plotly)
library(rgl)

# Calculate variance by marital status
variance_data <- aggregate(MntWines ~ Marital_Status, data = subset_data, FUN = var)

# Merge variance with original data for plotting
merged_data <- merge(subset_data, variance_data, by = "Marital_Status")
colnames(merged_data) <- c("Marital_Status", "MntWines", "MntWines_Variance")

# 3D Scatter plot
plot_ly(merged_data, 
        x = ~MntWines, 
        y = ~Marital_Status, 
        z = ~MntWines_Variance,
        type = "scatter3d", 
        mode = "markers",
        marker = list(size = 5, color = ~MntWines_Variance, colorscale = 'Viridis')) %>%
  layout(scene = list(
    xaxis = list(title = "Spending on Wine"),
    yaxis = list(title = "Marital Status"),
    zaxis = list(title = "Variance in Wine Spending"),
    title = "3D Plot of Wine Spending Variance by Marital Status"))












