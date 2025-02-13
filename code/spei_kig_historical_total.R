#'#####################################################################################
#' Project: [Kigigak Island Spectacled Eider Historical Data Standardization and Compilation]
#' Contents:
#'  -
#'  Author: [Ali McCarron]
#'
#'######################################################################################

install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos = 'http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c("workflowr", "tidyverse", "foreign", "dplyr", "here", "tools", "stringr","lubridate", "sp", "sf")
sapply(package_vec, install.load.package)


################################

#Combining header data from 1994-2005 and 2006-2015

# Convert the PHOTO column to character in both data frames
header_combined_data1994.2005$PHOTO <- as.character(header_combined_data1994.2005$PHOTO)
header_combined_data2006.2015$PHOTO <- as.character(header_combined_data2006.2015$PHOTO)

# Now combine the data frames
combined_header_data_total <- bind_rows(header_combined_data1994.2005, header_combined_data2006.2015)

#Converting NA values to a placeholder value
combined_header_data_total$EASTING[is.na(combined_header_data_total$EASTING)] <- 0
combined_header_data_total$NORTHING[is.na(combined_header_data_total$NORTHING)] <- 0

# Convert to sf object with the UTM coordinate system (e.g., UTM zone 33N)
sf_combined <- st_as_sf(combined_header_data_total, coords = c("EASTING", "NORTHING"), crs = 32603)

# Transform the coordinates to WGS84 (Latitude/Longitude in decimal degrees)
sf_combined <- st_transform(sf_combined, crs = 4326)

# Extract Latitude and Longitude from transformed coordinates
combined_header_data_total$LAT <- st_coordinates(sf_combined)[,2]
combined_header_data_total$LON <- st_coordinates(sf_combined)[,1]

# Convert the placeholder 0 values back to NA
combined_header_data_total$Latitude[combined_header_data_total$EASTING == 0 & combined_header_data_total$NORTHING == 0] <- NA
combined_header_data_total$Longitude[combined_header_data_total$EASTING == 0 & combined_header_data_total$NORTHING == 0] <- NA



##################################

#Combining markdata data from 1994-2005 and 2006-2015

combined_markdata_total <- bind_rows(markdata_combined_data1994.2005, markdata_combined_data2006.2015)

#Changing the date from "1900" to "2000" for data collected in 2000

combined_markdata_total <- combined_markdata_total %>%
  mutate(DATE = case_when(
    year(DATE) == 1900 ~ update(DATE, year = 2000),
    year(DATE) == 1902 ~ update(DATE, year = 2002),
    TRUE ~ DATE
  ))

#Combining "PREFIXNUMB" and "BANDNUMBER" into the "BANDNU" column to create a column that includes the entire band number
combined_markdata_total$BANDNU <- paste0(combined_markdata_total$PREFIXNUMB, combined_markdata_total$BANDNUMBER)

# Combine BANDNU and DATE into a unique ID column, labeled "MARK_ID"
combined_markdata_total$MARK_ID <- paste(combined_markdata_total$BANDNU, combined_markdata_total$DATE,
                                         sep = "_")

#Moving "MARK_ID" to front of datafram for ease of viewing
combined_markdata_total <- combined_markdata_total %>% relocate(MARK_ID, .before = BAND)

#Ensure there are no repeated rows of data by "MARK_ID"
combined_markdata_total <- combined_markdata_total %>% distinct(MARK_ID, .keep_all = TRUE)

#Using the Northing Easting coordinates to create Lat Long columns using decimal degrees

# Replace NAs in EASTING and NORTHING with placeholder value (e.g., 0)
combined_markdata_total$EASTING[is.na(combined_markdata_total$EASTING)] <- 0
combined_markdata_total$NORTHING[is.na(combined_markdata_total$NORTHING)] <- 0

# Convert to sf object with the UTM coordinate system (e.g., UTM zone 33N)
sf_combined <- st_as_sf(combined_markdata_total, coords = c("EASTING", "NORTHING"), crs = 32603)

# Transform the coordinates to WGS84 (Latitude/Longitude in decimal degrees)
sf_combined <- st_transform(sf_combined, crs = 4326)

# Extract Latitude and Longitude from transformed coordinates
combined_markdata_total$LATITITUDE <- st_coordinates(sf_combined)[,2]
combined_markdata_total$LONGITUDE <- st_coordinates(sf_combined)[,1]

# Convert the placeholder 0 values back to NA
combined_markdata_total$Latitude[combined_markdata_total$EASTING == 0 & combined_markdata_total$NORTHING == 0] <- NA
combined_markdata_total$Longitude[combined_markdata_total$EASTING == 0 & combined_markdata_total$NORTHING == 0] <- NA


###################################

#combining resight data from 1994-2005 and 2006-2015

combined_resight_data_total <- bind_rows(resight_combined_data1994.2005, resight_combined_data2006.2015)

#Using the Northing Easting coordinates to create Lat Long columns using decimal degrees

# Replace NAs in EASTING and NORTHING with placeholder value (e.g., 0)
combined_resight_data_total$EASTING[is.na(combined_resight_data_total$EASTING)] <- 0
combined_resight_data_total$NORTHING[is.na(combined_resight_data_total$NORTHING)] <- 0

# Convert to sf object with the UTM coordinate system (e.g., UTM zone 33N)
sf_combined <- st_as_sf(combined_resight_data_total, coords = c("EASTING", "NORTHING"), crs = 32603)

# Transform the coordinates to WGS84 (Latitude/Longitude in decimal degrees)
sf_combined <- st_transform(sf_combined, crs = 4326)

# Extract Latitude and Longitude from transformed coordinates
combined_resight_data_total$LAT <- st_coordinates(sf_combined)[,2]
combined_resight_data_total$LON <- st_coordinates(sf_combined)[,1]

# Convert the placeholder 0 values back to NA
combined_resight_data_total$Latitude[combined_resight_data_total$EASTING == 0 & combined_resight_data_total$NORTHING == 0] <- NA
combined_resight_data_total$Longitude[combined_resight_data_total$EASTING == 0 & combined_resight_data_total$NORTHING == 0] <- NA


####################################

#Combining visit data from 1994-2005 and 2006-2015

# Convert the PHOTO column to character in both data frames
visit_combined_data1994.2005$PHOTO <- as.character(visit_combined_data1994.2005$PHOTO)
visit_combined_data2006.2015$PHOTO <- as.character(visit_combined_data2006.2015$PHOTO)

combined_visit_data_total <- bind_rows(visit_combined_data1994.2005, visit_combined_data2006.2015)

#Using the Northing Easting coordinates to create Lat Long columns using decimal degrees

# Replace NAs in EASTING and NORTHING with placeholder value (e.g., 0)
combined_visit_data_total$EASTING[is.na(combined_visit_data_total$EASTING)] <- 0
combined_visit_data_total$NORTHING[is.na(combined_visit_data_total$NORTHING)] <- 0

# Convert to sf object with the UTM coordinate system (e.g., UTM zone 33N)
sf_combined <- st_as_sf(combined_visit_data_total, coords = c("EASTING", "NORTHING"), crs = 32603)

# Transform the coordinates to WGS84 (Latitude/Longitude in decimal degrees)
sf_combined <- st_transform(sf_combined, crs = 4326)

# Extract Latitude and Longitude from transformed coordinates
combined_visit_data_total$LAT <- st_coordinates(sf_combined)[,2]
combined_visit_data_total$LON <- st_coordinates(sf_combined)[,1]

# Convert the placeholder 0 values back to NA
combined_visit_data_total$Latitude[combined_visit_data_total$EASTING == 0 & combined_visit_data_total$NORTHING == 0] <- NA
combined_visit_data_total$Longitude[combined_visit_data_total$EASTING == 0 & combined_visit_data_total$NORTHING == 0] <- NA


