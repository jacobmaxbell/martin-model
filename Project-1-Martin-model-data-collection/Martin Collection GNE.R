# ECF 5421, sample code to help with assignment 1 | Alexander Haas, 7 August 2026
# Update one MARTIN data series & plot. The example below is Y (real GDP);
# set `var` and `sid` at the top to your own allocated series.

# The first time you use a package in R, you need to install it with install.packages()

# Load necessary libraries
library(readrba) # For reading RBA data
library(readabs) # For reading ABS data
library(lubridate) # For date manipulation
library(readr) # For reading data in various formats
library(zoo) # For working with time series data
library(dplyr) # For data manipulation
library(ggplot2) # For plotting data

# Clear the workspace
rm(list = ls()) # Remove all objects from the workspace

# Set the working directory
# setwd("") or create a project

# (1) Download and clean data (Example: real gdp, levels from the ABS)

var <- "GNE" # MARTIN acronym
sid <- "A2304113C" # ABS series ID
CUTOFF <- as.yearqtr("2026 Q1") # assignment cut-off: ignore anything after 2026Q1

raw_data <- read_abs(series_id = sid) # Read data from ABS using the specified series ID
# raw_data <- read_rba(series_id = sid) # ...or from the RBA instead, if your series is an
#                                       # RBA one (e.g. sid <- "FCMYGBAG10", a bond yield)

# Confirm you actually got the series you were allocated
meta <- raw_data %>% filter(series_id == sid) %>% slice(1)
cat("Downloaded:", trimws(meta$series), "\n")
cat(
  "  table:",
  meta$table_no,
  "|",
  meta$series_type,
  "|",
  meta$unit,
  "|",
  meta$frequency,
  "\n"
)
stopifnot(meta$frequency == "Quarter") # MARTIN is quarterly

cleaned_data <- raw_data %>%
  filter(series_id == sid) %>%
  mutate(date = as.Date(zoo::as.yearqtr(date))) %>%
  arrange(date) %>% # sort BEFORE de-duplicating
  distinct(date, .keep_all = TRUE) %>%
  filter(as.yearqtr(date) <= CUTOFF) %>%
  select(date, xx = value)

cleaned_data$date <- as.Date(cleaned_data$date) # Convert 'date' to Date format

# Fail loudly rather than producing an empty or short series
stopifnot(nrow(cleaned_data) > 0)
stopifnot(max(as.yearqtr(cleaned_data$date)) == CUTOFF)


# (2) Load the MARTIN.RData file
load("MARTINDATA.RData") # Load existing RData file containing MARTINDATA (equivalent to excel file)

# Fail loudly if there is a mistyped variable name
stopifnot(var %in% names(MARTINDATA))

# Inspect data (var is your MARTIN acronym; Y = real gdp in this example)
tail(MARTINDATA[[var]], 20) # Last 20 observations of the original series

# Create a copy of the dataset to which we'll append new data
MARTINDATA_UPDATED <- MARTINDATA

# Update MARTINDATA and save
# Derive the start quarter FROM THE DATA. Do not hard-code start as different series have different dates.
first_date <- min(cleaned_data$date)
MARTINDATA_UPDATED[[var]] <- ts(
  cleaned_data$xx,
  start = c(year(first_date), quarter(first_date)),
  frequency = 4
) # Update MARTINDATA with cleaned data, convert to time series object

# Sanity check: the ts must start where the data starts
stopifnot(
  as.yearqtr(time(MARTINDATA_UPDATED[[var]])[1]) == as.yearqtr(first_date)
)

# Inspect data (var is your MARTIN acronym; Y = real gdp in this example)
tail(MARTINDATA[[var]], 30) # Last 20 observations of the original series
tail(MARTINDATA_UPDATED[[var]], 30) # Last 20 observations of the updated series

# save(MARTINDATA_UPDATED, MARTINDATA, file = "MARTINDATA.RData")  # Save the updated data back to RData file

rm(raw_data, cleaned_data) # keep `meta`: the plot labels below use it


# (3) Prepare and plot GDP data
zoo_obj <- zoo(MARTINDATA[[var]], order.by = time(MARTINDATA[[var]])) # Convert MARTINDATA to a zoo object (z' ordered observations)
zoo_obj_upd <- zoo(
  MARTINDATA_UPDATED[[var]],
  order.by = time(MARTINDATA_UPDATED[[var]])
) # Convert MARTINDATA_UPDATED to a zoo object


MARTINDATA_df <- data.frame(
  date = as.yearqtr(index(zoo_obj)), # Convert index of zoo object to year-quarter format
  Value = coredata(zoo_obj) # Extract core data (values) from zoo object
)

MARTINDATA_df$date <- as.Date(MARTINDATA_df$date) # Convert 'date' to Date format


MARTINDATA_UPDATED_df <- data.frame(
  date = as.yearqtr(index(zoo_obj_upd)), # Convert index of zoo object to year-quarter format
  Value = coredata(zoo_obj_upd) # Extract core data (values) from zoo object
)

MARTINDATA_UPDATED_df$date <- as.Date(MARTINDATA_UPDATED_df$date) # Convert 'date' to Date format


# Plot the data using ggplot2
ggplot() +
  geom_line(
    data = MARTINDATA_UPDATED_df,
    aes(x = date, y = Value),
    color = "blue"
  ) + # Plot updated data as a blue line
  geom_line(
    data = MARTINDATA_df,
    aes(x = date, y = Value),
    color = "red",
    linetype = "dashed"
  ) + # Plot original data as a red dashed line
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = paste0(var, " -- levels (", sid, ")"),
    subtitle = trimws(meta$series),
    x = "Date",
    y = meta$unit,
    color = "Series"
  ) + # Add labels and title
  theme_minimal() # Use minimal theme for the plot


# Calculate percentage changes
MARTINDATA_UPDATED_pct <- MARTINDATA_UPDATED_df %>%
  mutate(Value_pct_change = (Value / dplyr::lag(Value) - 1) * 100) %>% # Calculate percentage change of 'Value'
  filter(!is.na(Value_pct_change)) # Remove rows with NA values

MARTINDATA_pct <- MARTINDATA_df %>%
  mutate(Value_pct_change = (Value / dplyr::lag(Value) - 1) * 100) %>% # Calculate percentage change of 'Value'
  filter(!is.na(Value_pct_change)) # Remove rows with NA values

# Plot percentage change data
ggplot() +
  geom_line(
    data = MARTINDATA_UPDATED_pct,
    aes(x = date, y = Value_pct_change),
    color = "blue"
  ) + # Plot percentage change of updated data as a blue line
  geom_line(
    data = MARTINDATA_pct,
    aes(x = date, y = Value_pct_change),
    color = "red",
    linetype = "dashed"
  ) + # Plot percentage change of original data as a red dashed line
  labs(
    title = paste0(var, " -- quarterly % change"),
    subtitle = trimws(meta$series),
    x = "Date",
    y = "Percentage Change (%)",
    color = "Series"
  ) + # Add labels and title
  theme_minimal() # Use minimal theme for the plot
