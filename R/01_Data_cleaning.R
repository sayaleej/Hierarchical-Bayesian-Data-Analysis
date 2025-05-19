# Exploratory Data Loading and Cleaning

#install if necessary
#install.packages("tidyverse")
#install.packages("lubridate")

# Load libraries
library(tidyverse)
library(lubridate)

# Load the dataset

file_path <- "Data/power+consumption+of+tetouan+city.zip"
unzip(file_path, exdir = "data")
csv_file <- "data/Tetuan City power consumption.csv"
df <- read.csv(csv_file)

# Preview structure
glimpse(df)

# Convert DateTime
df$DateTime <- mdy_hms(df$DateTime)

# Summary stats
summary(df)

# Check missing values
cat("Missing values per column:\n")
colSums(is.na(df))

df <- df %>%
  mutate(
    Date = date(DateTime),
    Month = month(DateTime, label = TRUE),
    Weekday = wday(DateTime, label = TRUE),
    Hour = hour(DateTime)
  )

# Save cleaned data
write.csv(df, "data/cleaned_power_data.csv", row.names = FALSE)


# There are no missing values in the dataset. 
# We have datapoints for the year 2017 from 1 January to 30 December for every 10 minutes
