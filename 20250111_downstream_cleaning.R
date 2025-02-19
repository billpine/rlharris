# Load necessary libraries
library(readxl)
library(dplyr)

# File path
d1 <- "Corrected Tallapoosa River Temp Data 2000-2018.xlsx"

# "Tailrace" sheet with proper splitting of Date and Time
tailrace_data <- read_excel(d1, sheet = "Tailrace") %>%
  mutate(
    Datetime = as.POSIXct(Date, format = "%m/%d/%y %H:%M"), # Convert to POSIXct datetime
    Date = as.Date(Datetime),                               # Extract Date
    Time = format(Datetime, "%H:%M:%S"),                    # Extract Time
    Site = "tailrace"
  ) %>%
  rename(Temperature = 2) %>% # Ensure correct temperature column is selected
  select(Date, Time, Temperature, Site)

# Repeat the same for "Malone" sheet
malone_data <- read_excel(d1, sheet = "Malone") %>%
  mutate(
    Datetime = as.POSIXct(Date, format = "%m/%d/%y %H:%M"), 
    Date = as.Date(Datetime),                               
    Time = format(Datetime, "%H:%M:%S"),                    
    Site = "Malone"
  ) %>%
  rename(Temperature = 2) %>% 
  select(Date, Time, Temperature, Site)

# Repeat the same for "Wadley" sheet
wadley_data <- read_excel(d1, sheet = "Wadley") %>%
  mutate(
    Datetime = as.POSIXct(Date, format = "%m/%d/%y %H:%M"), 
    Date = as.Date(Datetime),                               
    Time = format(Datetime, "%H:%M:%S"),                    
    Site = "Wadley"
  ) %>%
  rename(Temperature = 2) %>% 
  select(Date, Time, Temperature, Site)

# Combine all sheets into one data frame
combined_data <- bind_rows(tailrace_data, malone_data, wadley_data)

# Combine all sheets
combined_data <- bind_rows(tailrace_data, malone_data, wadley_data)

# Summarize the data for each site
summary_data <- combined_data %>%
  #group_by(Site) %>%
  summarize(
    Observations = sum(!is.na(Temperature)), # Count non-NA Temperature observations
    Min_Temperature = min(Temperature, na.rm = TRUE), # Minimum Temperature
    Max_Temperature = max(Temperature, na.rm = TRUE)  # Maximum Temperature
  )

# View the summary
print(summary_data)

# Count the number of Temperature observations >= 35 for each site
temperature_over_35 <- combined_data %>%
  filter(Temperature >= 35) %>%
  group_by(Site) %>%
  summarize(Count = n())

# View the result
print(temperature_over_35)


# Filter and display rows where Temperature >= 35
temperature_over_35_rows <- combined_data %>%
  filter(Temperature >= 35)

# View the rows that meet the condition
print(temperature_over_35_rows)

#16 observations at Wadley over 35 ()
#6 at tailrace

#write.csv(combined_data, "combined_data2.csv", row.names = FALSE)

# Calculate mean daily temperature for each site
mean_daily_temperature <- combined_data %>%
  group_by(Site, Date) %>%
  summarize(Mean_Temperature = mean(Temperature, na.rm = TRUE)) %>%
  ungroup()

# View the result
print(mean_daily_temperature)

# Write the mean_daily_temperature data frame to a CSV file
write.csv(mean_daily_temperature, "downstream_mean_daily_temperature.csv", row.names = FALSE)

