# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)
library(plotly)
library(corrplot)

cleaned_data <- "data/cleaned_power_data.csv"
df <- read.csv(cleaned_data,header=TRUE)

# ===================
# 📊 Univariate Plots
# ===================

# Histogram of Temperature
p1 <- ggplot(df, aes(x = Temperature)) +
  geom_histogram(fill = "skyblue", bins = 30) +
  labs(title = "Temperature Distribution", x = "Temperature (°C)", y = "Count")

# Histogram of Humidity
p2 <- ggplot(df, aes(x = Humidity)) +
  geom_histogram(fill = "orange", bins = 30) +
  labs(title = "Humidity Distribution", x = "Humidity (%)", y = "Count")

# Save combined plots
ggsave("plots/histograms.png", grid.arrange(p1, p2, ncol = 2), width = 12, height = 6)

# ========================
# 📈 Time Series Overview
# ========================

# Daily average power consumption
daily_avg <- df %>%
  group_by(Date) %>%
  summarise(across(starts_with("Zone"), mean, .names = "avg_{.col}"))

# Plot daily trends
line_plot<-ggplot(daily_avg, aes(x = Date)) +
  geom_line(aes(y = avg_Zone.1.Power.Consumption, color = "Zone 1")) +
  geom_line(aes(y = avg_Zone.2..Power.Consumption, color = "Zone 2")) +
  geom_line(aes(y = avg_Zone.3..Power.Consumption, color = "Zone 3")) +
  labs(title = "Daily Avg Power Consumption by Zone", y = "Power (kW)", x = "Date") +
  scale_color_manual(values = c("blue", "green", "red")) +
  theme_minimal() 

ggsave("plots/daily_avg_power.png", width = 10, height = 6)
ggplotly(line_plot)

# ========================
# 📈 Monthly average
# ========================

monthly_avg_long <- monthly_avg %>%
  pivot_longer(cols = starts_with("avg_Zone"), names_to = "Zone", values_to = "Power") %>%
  mutate(
    Zone = case_when(
      Zone == "avg_Zone.1.Power.Consumption" ~ "Zone 1",
      Zone == "avg_Zone.2..Power.Consumption" ~ "Zone 2",
      Zone == "avg_Zone.3..Power.Consumption" ~ "Zone 3",
      TRUE ~ Zone
    ),
    Month = factor(Month, levels = month.abb)  # ensures months ordered Jan, Feb, ...
  )

ggplot(monthly_avg_long, aes(x = Month, y = Power, color = Zone, group = Zone)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Monthly Power Consumption by Zone", y = "Power (kW)", x = "Month") +
  scale_color_manual(values = c("blue", "green", "red")) +
  theme_minimal()



ggsave("plots/monthly_avg_power.png", width = 10, height = 6)


# ===========================
# 🔁 Correlation Heatmap
# ===========================

# Remove Hour column if it exists
numeric_df <- df %>%
  select(where(is.numeric)) %>%
  select(-Hour)  # exclude Hour column if present

# Compute correlation matrix using complete cases only
cor_matrix <- cor(numeric_df, use = "complete.obs")

# Load ggcorrplot if not loaded
library(ggcorrplot)

# Plot correlation matrix with labels
corr_plot <- ggcorrplot::ggcorrplot(cor_matrix, 
                                    lab = TRUE, 
                                    lab_size = 3,
                                    title = "Correlation Matrix",
                                    ggtheme = theme_minimal())

print(corr_plot)

# Save the plot
ggsave("plots/correlation_matrix.png", plot = corr_plot, width = 8, height = 6)

# ===========================
# 📊 Day of the Week
# ===========================
df$DayOfWeek <- wday(df$DateTime, label = TRUE)

dow_plot<-df %>%
  group_by(DayOfWeek) %>%
  summarise(across(contains("Zone"), mean)) %>%
  pivot_longer(cols = contains("Zone"), names_to = "Zone", values_to = "Avg_Power") %>%
  ggplot(aes(x = DayOfWeek, y = Avg_Power, fill = Zone)) +
  geom_col(position = "dodge") +
  labs(title = "Average Power by Day of the Week") +
  theme_minimal()

ggsave("plots/day_of_the_week.png", plot = dow_plot, width = 8, height = 6)

df <- df %>%
  mutate(Weekend = if_else(DayOfWeek %in% c("Sat", "Sun"), "Weekend", "Weekday"))
daily_avg_weekend <- df %>%
  group_by(Date, Weekend) %>%
  summarise(across(contains("Zone"), mean, .names = "avg_{.col}"), .groups = "drop")

long_avg <- daily_avg_weekend %>%
  pivot_longer(cols = starts_with("avg_"), names_to = "Zone", values_to = "Avg_Power") %>%
  mutate(Zone = str_remove(Zone, "avg_"))

weekend <-ggplot(long_avg, aes(x = Date, y = Avg_Power, color = Weekend)) +
  geom_smooth() +
  facet_wrap(~ Zone, scales = "free_y") +
  labs(title = "Weekday vs Weekend Power Consumption (Daily Averages)",
       x = "Date", y = "Average Power (kW)") +
  theme_minimal() +
  scale_color_manual(values = c("Weekday" = "blue", "Weekend" = "red"))

ggsave("plots/weekday_vs_weekend.png", plot = weekend, width = 15, height = 6)

# ===========================
# 📊 Boxplots for Outliers
# ===========================

boxplot <- df %>%
  pivot_longer(cols = contains("Zone"), names_to = "Zone", values_to = "Power") %>%
  ggplot(aes(x = Zone, y = Power, fill = Zone)) +
  geom_boxplot() +
  labs(title = "Power Consumption Distribution by Zone") +
  theme_minimal()


ggplotly(boxplot)

# ===========================
# Outliers Analysis
# ===========================

df %>%
  pivot_longer(cols = contains("Zone"), names_to = "Zone", values_to = "Power") %>%
  group_by(Zone) %>%
  summarise(
    mean_power = mean(Power, na.rm = TRUE),
    median_power = median(Power, na.rm = TRUE),
    sd_power = sd(Power, na.rm = TRUE),
    min_power = min(Power, na.rm = TRUE),
    max_power = max(Power, na.rm = TRUE),
    n = n()
  )

library(dplyr)

# Reshape data to long format for analysis
power_long <- df %>%
  pivot_longer(cols = contains("Zone"), names_to = "Zone", values_to = "Power")

# Function to detect outliers using IQR method for one zone
detect_outliers <- function(data) {
  Q1 <- quantile(data$Power, 0.25, na.rm = TRUE)
  Q3 <- quantile(data$Power, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  data %>%
    mutate(
      Outlier = (Power < lower_bound) | (Power > upper_bound)
    )
}

# Apply outlier detection per zone
outliers_detected <- power_long %>%
  group_by(Zone) %>%
  group_modify(~ detect_outliers(.x)) %>%
  ungroup()

# Summary: number and percentage of outliers per zone
outlier_summary <- outliers_detected %>%
  group_by(Zone) %>%
  summarise(
    total_points = n(),
    outlier_count = sum(Outlier, na.rm = TRUE),
    outlier_percent = (outlier_count / total_points) * 100
  )

print(outlier_summary)

# Optional: view outlier values and details
outlier_values <- outliers_detected %>%
  filter(Outlier == TRUE) %>%
  arrange(Zone, Power)

print(outlier_values)

write.csv(outlier_values, "Zone3_outliers.csv", row.names = FALSE)

# Identify outliers in Zone 3
zone3_outliers <- df %>%
  mutate(is_outlier = Zone.3..Power.Consumption < quantile(Zone.3..Power.Consumption, 0.25, na.rm = TRUE) - 1.5 * IQR(Zone.3..Power.Consumption, na.rm = TRUE) |
           Zone.3..Power.Consumption > quantile(Zone.3..Power.Consumption, 0.75, na.rm = TRUE) + 1.5 * IQR(Zone.3..Power.Consumption, na.rm = TRUE))

# Plot time series with outliers highlighted
zone3_plot <- ggplot(zone3_outliers, aes(x = DateTime, y = Zone.3..Power.Consumption)) +
  geom_line(color = "lightblue") +  # Base line
  geom_point(data = subset(zone3_outliers, is_outlier), aes(x = DateTime, y = Zone.3..Power.Consumption), 
             color = "red", size = 1, alpha = 0.6) +
  labs(title = "Zone 3 Power Consumption Over Time",
       subtitle = "Red dots indicate outliers based on IQR",
       x = "DateTime", y = "Power (kW)") +
  theme_minimal()

# Show plot
print(zone3_plot)

# Save the plot
ggsave("plots/zone3_outliers_timeseries.png", plot = zone3_plot, width = 10, height = 6)

# Select numeric predictors and target
num_vars <- df_long %>%
  select(Power, Temperature_scaled, Wind.Speed_scaled, Humidity_scaled)

cor_matrix <- cor(num_vars, use = "complete.obs")

png("plots/corr_num.png", width = 6, height = 8, units = "in", res = 300)
corrplot(cor_matrix, method = "circle", type = "upper")
dev.off()
