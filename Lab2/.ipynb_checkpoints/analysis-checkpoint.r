# Load necessary libraries
library(tidyverse)
library(lubridate)
library(moments)
library(scales)

# Load the dataset
data <- read_csv("yellow_tripdata_sample.csv")

# Preview data
head(data)
glimpse(data)

# Convert datetime columns
data <- data %>%
  mutate(
    pickup_datetime = ymd_hms(tpep_pickup_datetime),
    dropoff_datetime = ymd_hms(tpep_dropoff_datetime)
  )

# Check for missing values
sapply(data, function(x) sum(is.na(x)))

# Descriptive statistics function
describe_stats <- function(x) {
  x_no_na <- na.omit(x)
  mode_val <- as.numeric(names(sort(table(x_no_na), decreasing = TRUE)[1]))  # mode as most frequent number
  cat("Count (non-missing):", length(x_no_na), "\n")
  cat("Missing values:", sum(is.na(x)), "\n")
  cat("Mean:", mean(x_no_na), "\n")
  cat("Median:", median(x_no_na), "\n")
  cat("Mode:", mode_val, "\n")
  cat("Min:", min(x_no_na), "\n")
  cat("Max:", max(x_no_na), "\n")
  cat("Standard Deviation:", sd(x_no_na), "\n")
  cat("Variance:", var(x_no_na), "\n")
  cat("Skewness:", skewness(x_no_na), "\n")
  cat("Kurtosis:", kurtosis(x_no_na), "\n")
  cat("----------\n")
}

# Apply descriptive stats to selected numeric columns
numeric_cols <- c("passenger_count", "trip_distance", "fare_amount", "total_amount", "tip_amount", "extra")
for (col in numeric_cols) {
  cat("Statistics for", col, ":\n")
  describe_stats(data[[col]])
}

# Create time features for temporal analysis
data <- data %>%
  mutate(
    hour = hour(pickup_datetime),
    day_of_week = wday(pickup_datetime, label = TRUE, abbr = FALSE),
    month = month(pickup_datetime, label = TRUE, abbr = FALSE)
  )

# Visualization Setup
theme_set(theme_minimal())

# 1. Temporal Analysis
# Trips by hour
ggplot(data, aes(x = hour)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Trip Counts by Hour", x = "Hour of Day", y = "Number of Trips")

# Trips by day of week
ggplot(data, aes(x = day_of_week)) +
  geom_bar(fill = "coral") +
  labs(title = "Trip Counts by Day of Week", x = "Day of Week", y = "Number of Trips")

# Average trip distance by hour
data %>%
  group_by(hour) %>%
  summarize(avg_trip_distance = mean(trip_distance, na.rm = TRUE)) %>%
  ggplot(aes(x = hour, y = avg_trip_distance)) +
  geom_line(color = "darkgreen") +
  geom_point() +
  labs(title = "Average Trip Distance by Hour", x = "Hour", y = "Average Trip Distance (miles)")

# Average tip amount by day of week
data %>%
  group_by(day_of_week) %>%
  summarize(avg_tip = mean(tip_amount, na.rm = TRUE)) %>%
  ggplot(aes(x = day_of_week, y = avg_tip)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Average Tip Amount by Day of Week", x = "Day of Week", y = "Average Tip")

# 2. Distribution Visualizations
# Histograms for trip_distance, fare_amount, tip_amount
for (col in c("trip_distance", "fare_amount", "tip_amount")) {
  print(
    ggplot(data, aes_string(x = col)) +
      geom_histogram(bins = 50, fill = "skyblue", color = "black") +
      labs(title = paste("Histogram of", col), x = col, y = "Count")
  )
  
  print(
    ggplot(data, aes_string(x = col)) +
      geom_density(fill = "lightgreen", alpha = 0.6) +
      labs(title = paste("Density Plot of", col), x = col, y = "Density")
  )
}

# Box and Violin plots for passenger_count, trip_distance, fare_amount
for (col in c("passenger_count", "trip_distance", "fare_amount")) {
  print(
    ggplot(data, aes_string(x = "1", y = col)) +
      geom_boxplot(fill = "lightblue") +
      labs(title = paste("Boxplot of", col), x = "", y = col) +
      theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())
  )
  print(
    ggplot(data, aes_string(x = "1", y = col)) +
      geom_violin(fill = "orange") +
      labs(title = paste("Violin plot of", col), x = "", y = col) +
      theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())
  )
}

# 3. Bar charts of categorical variables: payment_type, RatecodeID, store_and_fwd_flag
for (cat_col in c("payment_type", "RatecodeID", "store_and_fwd_flag")) {
  print(
    ggplot(data, aes_string(x = cat_col)) +
      geom_bar(fill = "steelblue") +
      labs(title = paste("Bar Chart of", cat_col), x = cat_col, y = "Count")
  )
}

# 4. Pie chart for VendorID distribution
vendor_counts <- data %>%
  count(VendorID) %>%
  mutate(perc = n / sum(n) * 100,
         label = paste0(VendorID, " (", round(perc, 1), "%)"))

ggplot(vendor_counts, aes(x = "", y = perc, fill = factor(VendorID))) +
  geom_col(width = 1, color = "white") +
  coord_polar('y', start = 0) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "VendorID Proportions") +
  theme_void() +
  scale_fill_brewer(palette = "Set3")

# 5. Spatial Insights: Top 10 Pickup locations by count and average fare
top_pickups <- data %>%
  count(PULocationID, sort = TRUE) %>%
  top_n(10, n)

ggplot(top_pickups, aes(x = reorder(as.factor(PULocationID), n), y = n)) +
  geom_col(fill = "darkcyan") +
  coord_flip() +
  labs(title = "Top 10 Pickup Locations by Number of Trips", x = "PULocationID", y = "Number of Trips")

avg_fare_top_pu <- data %>%
  filter(PULocationID %in% top_pickups$PULocationID) %>%
  group_by(PULocationID) %>%
  summarize(avg_fare = mean(fare_amount, na.rm = TRUE))

ggplot(avg_fare_top_pu, aes(x = reorder(as.factor(PULocationID), avg_fare), y = avg_fare)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Average Fare for Top 10 Pickup Locations", x = "PULocationID", y = "Average Fare")

# 6. Fare and Tip Analysis
# Scatter plot fare_amount vs trip_distance
ggplot(data, aes(x = trip_distance, y = fare_amount)) +
  geom_point(alpha = 0.3, color = "blue") +
  labs(title = "Fare Amount vs Trip Distance", x = "Trip Distance (miles)", y = "Fare Amount ($)")

# Correlation matrix for fare_amount, tip_amount, trip_distance
cor_df <- data %>% select(fare_amount, tip_amount, trip_distance) %>% drop_na()
cor_matrix <- cor(cor_df)
print(cor_matrix)

# Tip amount by passenger count boxplot
ggplot(data, aes(x = as.factor(passenger_count), y = tip_amount)) +
  geom_boxplot(fill = "lightpink") +
  labs(title = "Tip Amount by Passenger Count", x = "Passenger Count", y = "Tip Amount")

# 7. Passenger count distribution and averages
ggplot(data, aes(x = as.factor(passenger_count))) +
  geom_bar(fill = "purple") +
  labs(title = "Passenger Count Distribution", x = "Passenger Count", y = "Number of Trips")

passenger_summary <- data %>%
  group_by(passenger_count) %>%
  summarize(
    avg_trip_distance = mean(trip_distance, na.rm = TRUE),
    avg_fare = mean(fare_amount, na.rm = TRUE),
    avg_tip = mean(tip_amount, na.rm = TRUE)
  )
print(passenger_summary)

ggplot(passenger_summary, aes(x = as.factor(passenger_count), y = avg_fare)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Average Fare by Passenger Count", x = "Passenger Count", y = "Average Fare")

# 8. Outlier Detection using boxplots
ggplot(data, aes(y = fare_amount)) + 
  geom_boxplot(fill = "lightblue") +
  coord_cartesian(ylim = quantile(data$fare_amount, c(0.01, 0.99), na.rm = TRUE)) +
  labs(title = "Boxplot of Fare Amount (1st-99th percentile)")

ggplot(data, aes(y = trip_distance)) + 
  geom_boxplot(fill = "lightgreen") +
  coord_cartesian(ylim = quantile(data$trip_distance, c(0.01, 0.99), na.rm = TRUE)) +
  labs(title = "Boxplot of Trip Distance (1st-99th percentile)")

# Trips with fare > 99th percentile
fare_99th <- quantile(data$fare_amount, 0.99, na.rm = TRUE)
long_trips <- filter(data, fare_amount > fare_99th)
cat("Number of trips with fare above 99th percentile:", nrow(long_trips), "\n")

# Trips flagged with store_and_fwd_flag == 'Y'
store_flagged <- filter(data, store_and_fwd_flag == 'Y')
cat("Trips with store_and_fwd_flag == 'Y':", nrow(store_flagged), "\n")
summary(store_flagged)

# 9. Payment and Vendor Insights
ggplot(data, aes(x = factor(payment_type))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Payment Type Distribution", x = "Payment Type", y = "Count")

ggplot(data, aes(x = factor(payment_type), y = tip_amount)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Tip Amount Distribution by Payment Type", x = "Payment Type", y = "Tip Amount")

vendor_summary <- data %>%
  group_by(VendorID) %>%
  summarize(
    avg_fare = mean(fare_amount, na.rm = TRUE),
    avg_tip = mean(tip_amount, na.rm = TRUE),
    avg_total = mean(total_amount, na.rm = TRUE)
  )
print(vendor_summary)

ggplot(vendor_summary, aes(x = as.factor(VendorID), y = avg_fare)) +
  geom_col(fill = "darkred") +
  labs(title = "Average Fare by VendorID", x = "VendorID", y = "Average Fare")

# 10. Correlations and Regression
numeric_vars <- data %>% select(passenger_count, trip_distance, fare_amount, total_amount, tip_amount, extra) %>% drop_na()
correlation_matrix <- cor(numeric_vars)
print(correlation_matrix)

# Simple linear regression: fare_amount ~ trip_distance
lm1 <- lm(fare_amount ~ trip_distance, data = data)
summary(lm1)

# Multiple linear regression: tip_amount ~ fare_amount + passenger_count
lm2 <- lm(tip_amount ~ fare_amount + passenger_count, data = data)
summary(lm2)

# 11. Trip Duration and Speed
data <- data %>%
  mutate(trip_duration = as.numeric(difftime(dropoff_datetime, pickup_datetime, units = "mins")),
         trip_speed_mph = trip_distance / (trip_duration / 60))

summary(data$trip_speed_mph)

ggplot(data, aes(x = hour, y = trip_speed_mph)) +
  stat_summary(fun = mean, geom = "line", color = "blue") +
  labs(title = "Average Trip Speed (mph) by Hour of Day", x = "Hour", y = "Speed (mph)")

# Impact of congestion surcharge and airport fee on total_amount
ggplot(data, aes(x = congestion_surcharge > 0, y = total_amount)) +
  geom_boxplot(fill = "orange") +
  labs(x = "Congestion Surcharge Applied", y = "Total Amount", title = "Total Amount with/without Congestion Surcharge")

ggplot(data, aes(x = Airport_fee > 0, y = total_amount)) +
  geom_boxplot(fill = "purple") +
  labs(x = "Airport Fee Applied", y = "Total Amount", title = "Total Amount with/without Airport Fee")
