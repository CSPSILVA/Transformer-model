# Replace 'path_to_your_file.csv' with the actual path to your CSV file
solar_data <- read.csv('D:/IIT/Y5/Final Year Project/FYP/Datasets/archive4_1/unisolar/cleaned_new2.csv')

# View the first few rows of the data
head(solar_data)

# Assuming your data frame is named solar_data
correlation_matrix <- cor(solar_data[, c("SolarGeneration", "AirTemperature", "RelativeHumidity", "WindSpeed", "WindDirection")], 
                          use = "complete.obs", method = "pearson")
print(correlation_matrix)


####################################################################################################


# Scatter plot of SolarGeneration vs. AirTemperature
plot(solar_data$AirTemperature, solar_data$SolarGeneration,
     main = "Solar Generation vs. Air Temperature",
     xlab = "Air Temperature", ylab = "Solar Generation",
     pch = 19, col = "blue")

# Scatter plot of SolarGeneration vs. RelativeHumidity
plot(solar_data$RelativeHumidity, solar_data$SolarGeneration,
     main = "Solar Generation vs. Relative Humidity",
     xlab = "Relative Humidity", ylab = "Solar Generation",
     pch = 19, col = "yellow")

# Scatter plot of SolarGeneration vs. WindSpeed
plot(solar_data$WindSpeed, solar_data$SolarGeneration,
     main = "Solar Generation vs. Wind Speed",
     xlab = "Wind Speed", ylab = "Solar Generation",
     pch = 19, col = "green")

# Scatter plot of SolarGeneration vs. WindDirection
plot(solar_data$WindDirection, solar_data$SolarGeneration,
     main = "Solar Generation vs. Wind Direction",
     xlab = "Wind Direction", ylab = "Solar Generation",
     pch = 19, col = "orange")


######################################################################################


library(randomForest)

# Prepare the data (assuming solar_data is your dataset and SolarGeneration is the target variable)
set.seed(42) # For reproducibility
fit <- randomForest(SolarGeneration ~ ., data = data, importance = TRUE, na.action = na.omit)

# Print the importance of each variable
importance(fit)
varImpPlot(fit)


#######################################################################################


# Linear model
lm_model <- lm(SolarGeneration ~ AirTemperature + RelativeHumidity + WindSpeed + WindDirection, data = solar_data)
summary(lm_model)

# For non-linear relationships or interactions, consider adding interaction terms or using polynomial terms

########################################################################################


# Plotting histogram
hist(solar_data, main="Histogram of Dataset", xlab="Data", breaks=20, col="blue")

# Q-Q plot
qqnorm(solar_data)
qqline(solar_data, col="red")

######################################################################################

# Replace 'path_to_your_file.csv' with the actual path to your CSV file
df<- read.csv('D:/IIT/Y5/Final Year Project/FYP/Datasets/archive4_1/unisolar/Lag_roll_data.csv')

# View the first few rows of the data
head(df)

# Load necessary library for Q-Q plot
library(ggplot2)

# Histogram
hist(df$SolarGeneration_rolling_mean_1h, main = "Histogram of SolarGeneration Rolling Mean", xlab = "SolarGeneration Rolling Mean", breaks = 30)

# Q-Q Plot
qqnorm(df$SolarGeneration_rolling_mean_1h)
qqline(df$SolarGeneration_rolling_mean_1h)

# Alternatively, using ggplot2 for a Q-Q plot
ggplot(df, aes(sample = SolarGeneration_rolling_mean_1h)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Q-Q Plot of SolarGeneration Rolling Mean")

# Shapiro-Wilk Test
shapiro.test(df$SolarGeneration_rolling_mean_1h)

# Kolmogorov-Smirnov Test - comparing with a normal distribution
ks.test(df$SolarGeneration_rolling_mean_1h, "pnorm", mean(df$SolarGeneration_rolling_mean_1h, na.rm = TRUE), sd(df$SolarGeneration_rolling_mean_1h, na.rm = TRUE))

boxplot(df$SolarGeneration_rolling_mean_1h, main = "Boxplot", xlab = "Data", ylab = "Value")

