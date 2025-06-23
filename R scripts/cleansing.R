# import libraries
library(readr)
library(dplyr)

install.packages(c("spdep", "sf"))

library(spdep)
library(sf)


# load the data
data_mevo <- read_csv("Data/Data for analysis/data_mevo.csv")
View(data_mevo)
nrow(data_mevo)

# remove the column isVirtual
data_mevo <- data_mevo %>%
  select(-is_virtual_station)

# change the capacity value for GDA268 to 1000
data_mevo <- data_mevo %>%
  mutate(capacity = ifelse(name == "GDA268", 1000, capacity))

# add some columns 
data_mevo <- data_mevo %>%
  mutate(total_activity = rent_num + depo_num) %>%
  mutate(turnover_rate = (rent_num + depo_num) / capacity) %>%
  mutate(morning_ratio = morning_rent_num / rent_num) %>%
  mutate(evening_ratio = evening_rent_num / rent_num) %>%
  mutate(afternoon_ratio = afternoon_rent_num / rent_num)

# remove the -rent_num_right column
data_mevo <- data_mevo %>%
  select(-last_col())

# Function to calculate station density using Euclidean distance
calculate_density <- function(i, data_mevo, radius_km) {
  # Calculate distance to all other stations
  lat_diff <- data_mevo$lat - data_mevo$lat[i]
  lon_diff <- data_mevo$lon - data_mevo$lon[i]
  
  # Convert to approximate meters (rough but works for local areas)
  # 1 degree lat ≈ 111km, 1 degree lon ≈ 111km * cos(latitude)
  lat_dist_m <- lat_diff * 111000
  lon_dist_m <- lon_diff * 111000 * cos(data_mevo$lat[i] * pi/180)
  
  # Euclidean distance in meters
  distances <- sqrt(lat_dist_m^2 + lon_dist_m^2)
  
  # Count stations within radius (excluding self)
  sum(distances <= (radius_km * 1000) & distances > 0)
}

# Create both density variables
data_mevo$density_500m <- sapply(1:nrow(data_mevo), calculate_density, 
                                 data = data_mevo, radius_km = 0.5)

data_mevo$density_1000m <- sapply(1:nrow(data_mevo), calculate_density, 
                                  data = data_mevo, radius_km = 1.0)


# save the csv 
write_csv(data_mevo, "Data/Data for analysis/data_mevo_cleaned.csv")
