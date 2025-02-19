# Load necessary libraries

library(lubridate)
library(waterData)

# Import mean daily turbidity for tallapoosa at heflin

data <- importDVs("02412000", code=c("00010"),stat="00003", sdate="2017-12-05", edate="2020-11-24")
data <- data %>%
  rename(temperature = val) %>%
  mutate(qualcode = ifelse(qualcode != "A", NA, qualcode))

#stat = mean daily
#code = water temperature
#replace quality code other than A with NA
#hist(data$temperature) #check for outliers
#summary(data$temperature) #check for outliers


data$site<-"Heflin"

summary <- data %>%
  summarize(
    Total_Observations = n(),
    Observations_with_Temperature_Data = sum(!is.na(temperature)),
    Observations_with_NA = sum(is.na(temperature)),
    Minimum_Temperature = min(temperature, na.rm = TRUE),
    Maximum_Temperature = max(temperature, na.rm = TRUE)
  )

write.csv(data, "heflin.csv", row.names = FALSE)
