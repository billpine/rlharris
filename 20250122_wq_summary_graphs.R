#R Code to produce graphs 20250122

# Load necessary libraries
library(dplyr)
library(plotly)
library(tidyr)

# Read the data files
heflin_data <- read.csv("heflin.csv")
downstream_data <- read.csv("downstream_mean_daily_temperature.csv")

# Select and rename columns from the Heflin dataset
heflin_data <- heflin_data %>%
  select(Date, temperature, site) %>%
  rename(Temperature = temperature, Site = site)

# Select and rename columns from the downstream dataset
downstream_data <- downstream_data %>%
  select(Date, Mean_Temperature, Site) %>%
  rename(Temperature = Mean_Temperature)  # Removed redundant renaming of Site

# Combine the two datasets
combined_data <- bind_rows(
  heflin_data,
  downstream_data
)

# Ensure the Date column is in proper Date format
combined_data <- combined_data %>%
  mutate(Date = as.Date(Date))

# View the combined dataset
head(combined_data)

# Summarize available months and years by site
available_data <- combined_data %>%
  mutate(
    Year = format(Date, "%Y"),  # Consistent capitalization
    Month = format(Date, "%b")  # Consistent capitalization
  ) %>%
  group_by(Site, Year, Month) %>%
  summarise(Available = n(), .groups = "drop") %>%
  arrange(Site, Year, Month)

# Display the available data
available_data


##useful table


# Generate a complete grid of Site and Month combinations
all_sites <- unique(combined_data$Site)
all_months <- factor(month.abb, levels = month.abb)  # Ensure months are ordered correctly
site_month_grid <- expand.grid(Site = all_sites, Month = all_months)

# Summarize available data by site, year, and month
monthly_summary <- combined_data %>%
  mutate(
    Year = format(as.Date(Date), "%Y"),
    Month = factor(format(as.Date(Date), "%b"), levels = month.abb)  # Order months properly
  ) %>%
  group_by(Site, Year, Month) %>%
  summarise(Observations = n(), .groups = "drop") %>%
  right_join(site_month_grid, by = c("Site", "Month")) %>%  # Ensure all Site-Month combinations are included
  replace_na(list(Observations = 0))  # Fill missing observations with 0

# Reshape the data into a wide format with Year columns in sequence
summary_table <- monthly_summary %>%
  filter(!is.na(Year)) %>%  # Remove rows where Year is NA
  mutate(Year = as.numeric(Year)) %>%  # Convert Year to numeric for proper ordering
  arrange(Site, Month, Year) %>%  # Ensure Years are ordered numerically within Site and Month
  pivot_wider(
    names_from = Year,
    values_from = Observations,
    values_fill = 0
  ) %>%
  select(Site, Month, sort(tidyselect::peek_vars()))  # Ensure Year columns are sorted numerically


# View the resulting table
summary_table

write.csv(summary_table, "summary_table.csv", row.names = FALSE)



#timeline
# Simplify the data to show start and end dates for each site's data
gantt_data <- summary_table %>%
  pivot_longer(
    cols = -c(Site, Month),
    names_to = "Year",
    values_to = "Observations"
  ) %>%
  mutate(
    Year = as.numeric(Year),  # Convert Year to numeric
    Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d")  # Create date column
  ) %>%
  group_by(Site) %>%
  summarize(
    StartDate = min(Date[Observations > 0]),  # Earliest date with data
    EndDate = max(Date[Observations > 0])    # Latest date with data
  ) %>%
  filter(!is.na(StartDate) & !is.na(EndDate))  # Keep only sites with valid data

# Reverse the order of the Site column for ggplot
gantt_data$Site <- factor(gantt_data$Site, levels = rev(c("Heflin", "tailrace", "Malone", "Wadley")))

# Create the Gantt-like chart
ggplot(gantt_data, aes(y = Site)) +
  geom_segment(aes(x = StartDate, xend = EndDate, yend = Site, color = Site), size = 3) +
  scale_color_viridis_d(name = "Site") +  # Color-blind-friendly palette
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Data Availability by Site",
    x = "Year",
    y = "Site"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",  # Remove legend as sites are labeled on y-axis
    plot.title = element_text(hjust = 0.5, size = 14)
  )

ggsave("gantt_chart_years.jpeg", width = 8, height = 6, dpi = 300)


#
# Filter the data for 2018 only
data_2018 <- combined_data %>%
  filter(format(Date, "%Y") == "2018") %>%
  mutate(
    Month = factor(format(Date, "%b"), levels = month.abb),  # Convert to ordered month names
    Site = factor(Site, levels = rev(c("Heflin", "tailrace", "Malone", "Wadley")))  # Reverse and order sites
  )

# Create the plot
ggplot(data_2018, aes(x = Month, y = Site, fill = !is.na(Temperature))) +
  geom_tile(color = "black", size = 0.3) +
  scale_fill_viridis_d(
    name = "Data Availability",
    labels = c("No Data", "Data"),
    option = "D"  # Ensures consistency with previous viridis color palette
  ) +
  labs(
    title = "Daily Data Availability by Site (2018)",
    x = "Month",
    y = "Site"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 14)
  )

ggsave("gantt_chart_2018.jpeg", width = 8, height = 6, dpi = 300)


##specific site month year

# Filter the summary_table for the specific site, month, and year

# Filter the original data for example for Heflin, September 2018
site_month_year <- combined_data %>%
  mutate(
    Year = format(as.Date(Date), "%Y"),
    Month = format(as.Date(Date), "%b")
  ) %>%
  filter(Site == "Heflin", Year == "2018", Month == "Sep")

# Display the resulting data
site_month_year

#which months and years overlap

# Ensure the Date column is in proper Date format
combined_data <- combined_data %>%
  mutate(Date = as.Date(Date))

# Extract the Year and Month from the Date column
combined_data <- combined_data %>%
  mutate(
    Year = format(Date, "%Y"),
    Month = format(Date, "%b")
  )

# Group by Year, Month, and Site and count observations
monthly_site_summary <- combined_data %>%
  group_by(Year, Month, Site) %>%
  summarise(Observations = n(), .groups = "drop")

# Count the number of sites with data for each Year-Month combination
overlapping_data <- monthly_site_summary %>%
  group_by(Year, Month) %>%
  summarise(SitesWithData = n_distinct(Site), .groups = "drop") %>%
  filter(SitesWithData == 4)  # Keep only rows where all 4 sites have data

# Display the overlapping months and years
print(overlapping_data)

#

#double check calculating unique

# Group data by Site, Year, and Month
common_periods <- combined_data %>%
  mutate(
    Year = format(Date, "%Y"),
    Month = format(Date, "%m")
  ) %>%
  group_by(Year, Month) %>%
  summarise(Sites = n_distinct(Site), .groups = "drop") %>%
  filter(Sites == n_distinct(combined_data$Site)) %>%
  arrange(Year, Month)

# Display common months and years
common_years <- unique(common_periods$Year)
common_months <- unique(common_periods$Month)

cat("Common years:", paste(common_years, collapse = ", "), "\n")
cat("Common months:", paste(common_months, collapse = ", "), "\n")


# Filter data for the year 2018
data_2018 <- combined_data %>%
  filter(format(Date, "%Y") == "2018") %>%
  mutate(
    Month = factor(format(Date, "%b"), levels = month.abb),  # Convert to ordered month names
    DayOfYear = as.numeric(format(Date, "%j"))              # Day of the year (1-365)
  )

# Reorder the Site levels
data_2018 <- data_2018 %>%
  mutate(Site = factor(Site, levels = c("Heflin", "tailrace", "Malone", "Wadley")))

# Calculate mean daily temperature for each site and day
#the temps are already mean by day
heatmap_data <- data_2018 # %>%
  # group_by(Site, DayOfYear) %>%
  # summarise(MeanTemperature = mean(Temperature, na.rm = TRUE), .groups = "drop")


# Determine the global color scale range
global_min <- min(heatmap_data$Temperature, na.rm = TRUE)
global_max <- max(heatmap_data$Temperature, na.rm = TRUE)

# Define breaks and labels for each month
month_breaks <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)  # Approx. start of each month
month_labels <- month.abb

# Reorder the Site factor to specify the desired order
heatmap_data <- heatmap_data %>%
  mutate(Site = factor(Site, levels = c("Heflin", "tailrace", "Malone", "Wadley")))

# Create the heatmap with additional legend breaks
ggplot(heatmap_data, aes(x = DayOfYear, y = Site, fill = Temperature)) +
  geom_tile(color = "black") +
  scale_fill_gradientn(
    colors = c("#352A86", "#00A1D5", "#E2E2E2", "#F28E2C"),  # Color-blind-friendly palette
    limits = c(global_min, global_max),
    breaks = seq(floor(global_min), ceiling(global_max), by = 5),  # Add breaks every 5 degrees
    na.value = "black",  # Set missing values to black
    name = "Mean Temp (°C)"
  ) +
  scale_x_continuous(
    breaks = month_breaks,
    labels = month_labels
  ) +
  scale_y_discrete(limits = rev(levels(factor(heatmap_data$Site)))) +  # Reverse the y-axis
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.text = element_text(size = 10),  # Adjust legend text size
    legend.title = element_text(size = 12, face = "bold")  # Bold legend title
  ) +
  labs(
    title = "Mean Daily Temperature (2018)",
    x = "Month",
    y = "Site"
  )


ggsave("heat_map.jpeg", width = 8, height = 6, dpi = 300)

##simple line plot
ggplot(heatmap_data, aes(x = DayOfYear, y = Temperature, color = Site, group = Site, linetype = Site)) +
  geom_line(size = 1) +
  scale_color_viridis_d(name = "Site") +  # Use a color-blind-friendly palette
  scale_linetype_manual(
    values = c("Heflin" = "dashed", "tailrace" = "solid", "Malone" = "solid", "Wadley" = "solid"),
    name = "Site"
  ) +
  scale_x_continuous(
    breaks = month_breaks,
    labels = month_labels
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.grid = element_line(color = "grey80"),
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold")
  ) +
  labs(
    title = "Mean Daily Temperature (2018)",
    x = "Month",
    y = "Mean Temperature (°C)"
  )


ggsave("2018_temp_line_chart.jpeg", width = 8, height = 6, dpi = 300)



##difference
# Filter data for the year 2018
data_2018 <- combined_data %>%
  filter(format(Date, "%Y") == "2018") %>%
  mutate(
    DayOfYear = as.numeric(format(Date, "%j")),
    Month = factor(format(Date, "%b"), levels = month.abb)
  )

# # Calculate mean daily temperature for each site and day
# mean_temp <- data_2018 %>%
#   group_by(Site, Date) %>%
#   summarise(MeanTemperature = mean(Temperature, na.rm = TRUE), .groups = "drop")

# Pivot the data to calculate differences
temp_diff <- data_2018 %>%
  pivot_wider(names_from = Site, values_from = Temperature) %>%
  filter(!is.na(Heflin)) %>%
  mutate(
    tailrace_diff = tailrace - Heflin,
    malone_diff = Malone - Heflin,
    wadley_diff = Wadley - Heflin
  ) %>%
  pivot_longer(cols = c(tailrace_diff, malone_diff, wadley_diff), 
               names_to = "Site", 
               values_to = "TempDifference") %>%
  mutate(
    DayOfYear = as.numeric(format(Date, "%j")),
    Site = gsub("_diff", "", Site)  # Remove "_diff" suffix
  )

# Filter valid months where all sites have data
valid_months <- temp_diff %>%
  group_by(Month = format(Date, "%b")) %>%
  summarise(ValidSites = n_distinct(Site[!is.na(TempDifference)])) %>%
  filter(ValidSites == 3, Month != "Nov") %>%  # Exclude November
  pull(Month)

# Ensure sites are ordered correctly in the legend
temp_diff_filtered <- temp_diff %>%
  mutate(
    Month = format(Date, "%b"),
    #Site = factor(Site, levels = c("tailrace", "Malone", "Wadley"))  # Explicitly set the order
  ) %>%
  filter(Month %in% valid_months)

write.csv(temp_diff_filtered, "temp_diff_filtered.csv", row.names = FALSE)


# Define month breaks and labels to include all months from January to December
month_breaks <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
month_labels <- month.abb

# Create temperature difference plot



library(lubridate)

temp_diff_filtered$Date <- ymd("2018-01-01") + days(temp_diff_filtered$DayOfYear - 1)

p <- ggplot(temp_diff_filtered, aes(x = Date, y = TempDifference, group = Site, color = Site)) +
  geom_line(size = 1.5) +
  scale_color_manual(
    name = "Site",
    values = c(
      "tailrace" = "#440154FF",  # Purple (Viridis)
      "malone" = "#31688EFF",    # Blue (Viridis)
      "wadley" = "#35B779FF"     # Green (Viridis)
    ),
    labels = c("Tailrace", "Malone", "Wadley"),
    breaks = c("tailrace", "malone", "wadley")  # Ensuring the breaks match the labels
  ) +
  geom_hline(yintercept = c(2.7, -2.7), linetype = "dashed", color = "darkred", size = 1) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels = "%B", # Format labels as full month names
    limits = as.Date(c("2018-01-01", "2018-12-31")) # Limit to the year 2018
  ) +
  scale_y_continuous(
    breaks = seq(floor(min(temp_diff_filtered$TempDifference, na.rm = TRUE)),
                 ceiling(max(temp_diff_filtered$TempDifference, na.rm = TRUE)),
                 by = 1) # Add tick marks every 1 degree
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  labs(
    title = "Daily Temperature Difference from Heflin (1 January to 31 December 2018)",
    x = "Month",
    y = "Temperature Difference (°C)"
  )

# Display the plot
print(p)


ggsave("2018_difference_line_chart.jpeg", plot=p, width = 8, height = 6, dpi = 300)



# Convert to interactive plot
interactive_plot <- ggplotly(p)
interactive_plot



# Create a summary of the number of days with temperature differences exceeding the thresholds
temp_diff_summary <- temp_diff_filtered %>%
  mutate(
    Threshold2.7 = abs(TempDifference) >= 2.7, # Temperature difference > 2.7°C
    #Threshold3 = abs(TempDifference) > 3      # Temperature difference > 3°C
  ) %>%
  group_by(Site, Month) %>%
  summarise(
    DaysAbove2.7 = sum(Threshold2.7, na.rm = TRUE),
    #DaysAbove3 = sum(Threshold3, na.rm = TRUE),
    .groups = "drop"
  )

# Display the summary
print(temp_diff_summary)

write.csv(temp_diff_summary, "temp_diff_summary.csv", row.names = FALSE)

# Ensure all months from April to October are represented for each site
all_sites <- unique(temp_diff_filtered$Site)
all_months <- c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")
site_month_grid <- expand.grid(Site = all_sites, Month = all_months)

# Join the summary data with the full grid of sites and months
temp_diff_summary_full <- site_month_grid %>%
  left_join(temp_diff_summary, by = c("Site", "Month")) %>%
  mutate(DaysAbove2.7 = replace_na(DaysAbove2.7, 0))  # Fill missing values with 0

# Reorder the Site factor to specify the desired order
temp_diff_summary_full <- temp_diff_summary_full %>%
  mutate(Site = factor(Site, levels = c("tailrace", "malone", "wadley")))

write.csv(temp_diff_summary_full, "temp_diff_summary.csv", row.names = FALSE)


# Create the 3-panel bar plot
ggplot(temp_diff_summary_full, aes(x = Month, y = DaysAbove2.7, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  facet_wrap(~Site, ncol = 1) +  # Create a separate panel for each site
  scale_fill_viridis_d(name = "Site", guide = "none") +  # Use a color-blind friendly palette
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.grid.major.x = element_blank(),  # Remove vertical gridlines
    panel.grid.major.y = element_line(color = "grey80")  # Keep horizontal gridlines
  ) +
  labs(
    title = "Number of Days with Temperature Differences > 2.7°C",
    x = "Month",
    y = "Number of Days"
  )

