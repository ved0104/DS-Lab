# ---------------- Libraries ----------------
library(dplyr)      # Data manipulation
library(ggplot2)    # Data visualization
library(moments)    # Skewness & kurtosis
library(readr)      # Reading CSV files
library(psych)      # Descriptive statistics (describe)
library(reshape2)   # Reshaping data, crosstabs
library(lubridate)  # Date-time handling
library(GGally)     # Pair plots & correlation plots
library(stats)      # Built-in statistical functions

# ---------------- Load data ----------------
# Load yellow taxi dataset (sample) from CSV
df <- read_csv("yellow_tripdata_sample.csv")

# Display first few rows
head(df)

# ---------------- Data info ----------------
# Show structure of dataset: variables, data types, etc.
str(df)

# ---------------- Descriptive stats --------
# Select numeric fields of interest
fields <- c(
  "passenger_count", "trip_distance", "fare_amount", "tip_amount",
  "extra", "total_amount"
)

# Compute summary statistics for each field
stats_list <- lapply(fields, function(col) {
  series <- df[[col]]
  list(
    mean = mean(series, na.rm = TRUE),                     # Mean
    median = median(series, na.rm = TRUE),                 # Median
    mode = names(sort(table(series), decreasing = TRUE))[1], # Mode
    min = min(series, na.rm = TRUE),                       # Minimum
    max = max(series, na.rm = TRUE),                       # Maximum
    std = sd(series, na.rm = TRUE),                        # Standard deviation
    variance = var(series, na.rm = TRUE),                  # Variance measures how far values are spread out from the mean.
    skewness = skewness(series, na.rm = TRUE),             # Skewness measures the asymmetry of the distribution.
    kurtosis = kurtosis(series, na.rm = TRUE),             # Kurtosis   measures the “tailedness” (heaviness of tails) of the distribution.
    count = sum(!is.na(series)),                           # Non-missing values
    missing_values = sum(is.na(series))                    # Missing values
  )
})

# Convert results to a dataframe
stats_df <- as.data.frame(do.call(rbind, stats_list))
rownames(stats_df) <- fields

# Print descriptive statistics
print(stats_df)

# Display first 10 rows of selected fields
print(head(df[fields], 10))

# ---------------- Histogram + freq polygon --
# Plot histogram with frequency polygon for each field
for (col in fields) {
  data <- na.omit(df[[col]])
  hist_data <- hist(data, breaks = 30, plot = FALSE)
  
  # Midpoints of histogram bins
  bin_mids <- 0.5 * (head(hist_data$breaks, -1) +
                       tail(hist_data$breaks, -1))
  
  # Create dataframe for frequency polygon
  df_hist <- data.frame(
    mids = bin_mids,
    counts = hist_data$counts
  )
  
  # Plot histogram + frequency polygon
  p <- ggplot() +
    geom_histogram(
      aes(x = data),
      bins = 30, fill = "skyblue",
      color = "black", alpha = 0.7
    ) +
    geom_line(
      data = df_hist,
      aes(x = mids, y = counts),
      color = "red"
    ) +
    geom_point(
      data = df_hist,
      aes(x = mids, y = counts),
      color = "red"
    ) +
    labs(
      title = paste("Histogram & Freq Polygon of", col),
      x = col, y = "Frequency"
    )
  print(p)
}

# ---------------- Boxplot + Violin ----------
# Create boxplot & violin plot for each field
for (col in fields) {
  # Boxplot
  p1 <- ggplot(df, aes(x = "", y = .data[[col]])) +
    geom_boxplot(fill = "lightgreen") +
    labs(title = paste("Box Plot of", col), y = col, x = "")
  
  # Violin plot
  p2 <- ggplot(df, aes(x = "", y = .data[[col]])) +
    geom_violin(fill = "lightblue") +
    labs(title = paste("Violin Plot of", col), y = col, x = "")
  
  print(p1)
  print(p2)
}

# ---------------- Density plots -------------
# Plot density distribution for each field
for (col in fields) {
  p <- ggplot(df, aes_string(x = col)) +
    geom_density(fill = "purple", alpha = 0.6) +
    labs(
      title = paste("Density Plot of", col),
      x = col, y = "Density"
    )
  print(p)
}

# ---------------- Confidence intervals -----
# Function to compute 95% confidence interval
confidence_interval <- function(data, conf = 0.95) {
  data <- na.omit(data)
  mean_val <- mean(data)
  se <- sd(data) / sqrt(length(data)) # standard error
  margin <- qt((1 + conf) / 2, df = length(data) - 1) * se
  c(mean_val, mean_val - margin, mean_val + margin)
}

# Apply CI to selected numeric columns
for (col in c("trip_distance", "fare_amount", "tip_amount")) {
  ci <- confidence_interval(df[[col]])
  cat(
    col, "-> Mean:", round(ci[1], 2),
    ", 95% CI: (", round(ci[2], 2),
    ",", round(ci[3], 2), ")\n"
  )
}

# ---------------- Hypothesis tests ----------
# (a) One-sample t-test: Tip vs expected $2
t1 <- t.test(df$tip_amount, mu = 2)
cat("\nOne-sample t-test (Tip vs $2):\n")
print(t1)

# (b) Two-sample t-test: Fare by payment type (credit vs cash)
fare_credit <- df %>%
  filter(payment_type == 1) %>%
  pull(fare_amount)

fare_cash <- df %>%
  filter(payment_type == 2) %>%
  pull(fare_amount)

t2 <- t.test(fare_credit, fare_cash)
cat("\nTwo-sample t-test (Fare Credit vs Cash):\n")
print(t2)

# (c) Chi-square test: Payment type vs RateCodeID
contingency <- table(df$payment_type, df$RatecodeID)
chi <- chisq.test(contingency)
cat("\nChi-square Test (Payment vs RateCodeID):\n")
print(chi)

# ---------------- Correlations -------------
# Pearson correlation tests
pearson_td_fa <- cor.test(
  df$trip_distance, df$fare_amount,
  use = "complete.obs"
)
pearson_fa_ta <- cor.test(
  df$fare_amount, df$tip_amount,
  use = "complete.obs"
)

cat("\nPearson Correlations:\n")
print(pearson_td_fa)
print(pearson_fa_ta)

# Correlation matrix heatmap
corr_cols <- c("trip_distance", "fare_amount", "tip_amount")
corr_matrix <- cor(
  df[, corr_cols],
  use = "complete.obs"
)
melted_corr <- melt(corr_matrix)

p_corr <- ggplot(
  melted_corr,
  aes(Var1, Var2, fill = value)
) +
  geom_tile() +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white"
  ) +
  labs(title = "Correlation Matrix Heatmap") +
  theme_minimal()
print(p_corr)

# ---------------- Time series ---------------
# Convert pickup/dropoff to datetime
df$tpep_pickup_datetime <- ymd_hms(
  df$tpep_pickup_datetime, tz = "UTC"
)
df$tpep_dropoff_datetime <- ymd_hms(
  df$tpep_dropoff_datetime, tz = "UTC"
)

# Aggregate trips by pickup date
df$pickup_date <- as.Date(df$tpep_pickup_datetime)
trip_counts <- df %>%
  group_by(pickup_date) %>%
  summarise(count = n())

# Daily trip count time series plot
p_ts <- ggplot(trip_counts, aes(x = pickup_date, y = count)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(
    title = "Daily Trip Count",
    x = "Date", y = "Number of Trips"
  ) +
  theme_minimal()
print(p_ts)

# ---------------- Hourly variation ----------
# Extract pickup hour from datetime
df$pickup_hour <- hour(df$tpep_pickup_datetime)

# Plot fare distribution by hour of day
p_hour <- ggplot(df, aes(x = factor(pickup_hour),
                         y = fare_amount)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Fare Amount by Hour of Day",
    x = "Hour of Day", y = "Fare Amount ($)"
  ) +
  theme_minimal()
print(p_hour)
