# ECF 5421, sample code to help with assignment 1 | Alexander Haas, 4 August 2026
# Update Y (real GDP series) in the MARTIN model & plot

# The first time you use a package in R, you need to install it with install.packages()

# Load necessary libraries
library(readrba)   # For reading RBA data
library(readabs)   # For reading ABS data
library(lubridate) # For date manipulation
library(readr)     # For reading data in various formats
library(zoo)       # For working with time series data
library(dplyr)     # For data manipulation
library(ggplot2)   # For plotting data

# Clear the workspace
rm(list = ls())  # Remove all objects from the workspace

# Set the working directory
# setwd("") or create a project




# (1) Download and clean data (Example: real gdp, levels from the ABS)
raw_data <- read_abs(series_id = c("A2304402X"))  # Read data from ABS using the specified series ID
# raw_data <- read_rba(series_id = "FCMYGBAG10") # Read data from the RBA using the specified series ID (ex: bond yield data)


cleaned_data <- raw_data %>%
  filter(series_id == "A2304402X") %>%
  mutate(date = as.Date(zoo::as.yearqtr(date))) %>%
  distinct(date, .keep_all = TRUE) %>%
  select(date, xx = value)

cleaned_data$date <- as.Date(cleaned_data$date)  # Convert 'date' to Date format



# (2) Load the MARTIN.RData file
load("MARTINDATA.RData")  # Load existing RData file containing MARTINDATA (equivalent to excel file)

# Inspect data (acronym Y for real gdp)
tail(MARTINDATA$Y, 20)        # Last 20 observations of the original series

# Create a copy of the dataset to which we'll append new data
MARTINDATA_UPDATED <- MARTINDATA

# Update MARTINDATA and save
MARTINDATA_UPDATED$Y <- ts(cleaned_data$xx, start = c(1959, 3), frequency = 4)  # Update MARTINDATA with cleaned data, convert to time series object

# Inspect data (acronym Y for real gdp)
tail(MARTINDATA$Y, 30)          # Last 20 observations of the original series
tail(MARTINDATA_UPDATED$Y, 30)  # Last 20 observations of the updated series

# save(MARTINDATA_UPDATED, MARTINDATA, file = "MARTINDATA.RData")  # Save the updated data back to RData file

rm(raw_data,cleaned_data)



# (3) Prepare and plot GDP data
zoo_obj <- zoo(MARTINDATA$Y, order.by = time(MARTINDATA$Y))  # Convert MARTINDATA to a zoo object (z' ordered observations)
zoo_obj_upd <- zoo(MARTINDATA_UPDATED$Y, order.by = time(MARTINDATA_UPDATED$Y))  # Convert MARTINDATA_UPDATED to a zoo object


MARTINDATA_df <- data.frame(
  date = as.yearqtr(index(zoo_obj)),  # Convert index of zoo object to year-quarter format
  Value = coredata(zoo_obj)           # Extract core data (values) from zoo object
)

MARTINDATA_df$date <- as.Date(MARTINDATA_df$date)  # Convert 'date' to Date format


MARTINDATA_UPDATED_df <- data.frame(
  date = as.yearqtr(index(zoo_obj_upd)),  # Convert index of zoo object to year-quarter format
  Value = coredata(zoo_obj_upd)           # Extract core data (values) from zoo object
)

MARTINDATA_UPDATED_df$date <- as.Date(MARTINDATA_UPDATED_df$date)  # Convert 'date' to Date format


# Plot the data using ggplot2
ggplot() +
  geom_line(data = MARTINDATA_UPDATED_df, aes(x = date, y = Value), color = "blue") +  # Plot updated data as a blue line
  geom_line(data = MARTINDATA_df, aes(x = date, y = Value), color = "red", linetype = "dashed") +  # Plot original data as a red dashed line
  labs(title = "Real GDP (Y)", x = "Date", y = "GDP", color = "Series") +  # Add labels and title
  theme_minimal()  # Use minimal theme for the plot


# Calculate percentage changes
MARTINDATA_UPDATED_pct <- MARTINDATA_UPDATED_df %>%
  mutate(Value_pct_change = (Value / dplyr::lag(Value) - 1) * 100) %>%  # Calculate percentage change of 'Value'
  filter(!is.na(Value_pct_change))  # Remove rows with NA values

MARTINDATA_pct <- MARTINDATA_df %>%
  mutate(Value_pct_change = (Value / dplyr::lag(Value) - 1) * 100) %>%  # Calculate percentage change of 'Value'
  filter(!is.na(Value_pct_change))  # Remove rows with NA values

# Plot percentage change data
ggplot() +
  geom_line(data = MARTINDATA_UPDATED_pct, aes(x = date, y = Value_pct_change), color = "blue") +  # Plot percentage change of updated data as a blue line
  geom_line(data = MARTINDATA_pct, aes(x = date, y = Value_pct_change), color = "red", linetype = "dashed") +  # Plot percentage change of original data as a red dashed line
  labs(title = "Percentage Change of Y Over Time", x = "Date", y = "Percentage Change (%)", color = "Series") +  # Add labels and title
  theme_minimal()  # Use minimal theme for the plot