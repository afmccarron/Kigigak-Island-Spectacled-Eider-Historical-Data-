#'###########################################################################################
#'###########################################################################################
#' Project: [Kigigak Island Spectacled Eider Historical Data Standardization and Compilation]
#' Contents: [Combining the data frames compiled from years 1994-2005 and 2006-2015]
#'  - Installing and loading packages required for the following code.
#'  - Combining the header data
#'    - Converting the form of the "PHOTO" column data to be standard to combine dataframes
#'    - Combining dataframes from 1994-2005 and 2006-2015
#'    - Converting the Easting Northing values to Latitude Longitude in decimal degrees
#'  -Combining the markdata dataframes
#'    - Combining markdata datadrames from 1994-2005 and 2006-2015
#'    - Adjusting some of the date data that was incorrectly input. (Year 1900=2000 and 1902=2002)
#'    - Combining the prefix number and remainder of band number columns to get a column with entire band number
#'    - Combining band number and date to create a unique identifying column and ensuring there is no repeating data
#'    - Converting Northing and Easting values to Latitute Longitude in decimal degrees
#'  -Combining resight data
#'    - Combining resight dateframes from 1994-2005 and 2006-2015
#'    - Converting Northing and Easting values to Latitute Longitude in decimal degrees
#'  - Combining visit data
#'    - Converting data in "PHOTO" column to character to be standard for compilation
#'    - Combining visit dataframes from 1994-2005 and 2006-2015
#'    - Converting Northing and Easting values to Latitude Longitude in decimal degrees
#'
#'  Author: [Ali McCarron]
#'
#'######################################################################################

install.load.package <- function(x) {

  if (!require(x, character.only = TRUE)) {

    install.packages(x, repos = 'http://cran.us.r-project.org')
  }

  require(x, character.only = TRUE)
}

package_vec <- c("workflowr", "tidyverse", "foreign", "dplyr", "here", "tools", "stringr","readxl", "purrr", "lubridate", "sp", "sf", "leaflet")
sapply(package_vec, install.load.package)

################################

#Combining header data from 1994-2005 and 2006-2015

# Convert the PHOTO column to character in both data frames
header_combined_data1994.2005$PHOTO <- as.character(header_combined_data1994.2005$PHOTO)
header_combined_data2006.2015$PHOTO <- as.character(header_combined_data2006.2015$PHOTO)

#Convert EASTING columns to character in both data frames
header_combined_data1994.2005$EASTING <- as.integer(header_combined_data1994.2005$EASTING)
header_combined_data2006.2015$EASTING <- as.integer(header_combined_data2006.2015$EASTING)

#Convert NORTHING columns to character in both data frames
header_combined_data1994.2005$NORTHING <- as.integer(header_combined_data1994.2005$NORTHING)
header_combined_data2006.2015$NORTHING <- as.integer(header_combined_data2006.2015$NORTHING)

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
combined_header_data_total$LAT[combined_header_data_total$EASTING == 0 & combined_header_data_total$NORTHING == 0] <- NA
combined_header_data_total$LON[combined_header_data_total$EASTING == 0 & combined_header_data_total$NORTHING == 0] <- NA

#Plot the converted LAT LON values
leaflet(combined_header_data_total)%>%
  addProviderTiles('Esri.WorldImagery')%>%
  addCircleMarkers(
    lng = ~LON,
    lat = ~LAT,
    radius = 5,
    color = "blue",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste("HEADER_ID: ", combined_header_data_total$HEADER_ID)
  )

#identify data points that have lat long values outside the Kigigak area

  #Define Kig area boundaries
  min_lat <- 60.814000
  max_lat <- 60.879000
  min_lon <- -165.029600
  max_lon <- -164.883000

  #Filter for rows outside Kig area
  header_rows_outside_kig <- combined_header_data_total %>% filter((LAT < min_lat) |
                                                                     (LAT > max_lat) |
                                                                     (LON < min_lon) |
                                                                     (LON > max_lon)
                                                                   )

#Remove columns that have only NAs
combined_header_data_total <- combined_header_data_total %>% select_if(~!all(is.na(.) | . == 0))

#merging columns H2O_DIST and H20_DIST
combined_header_data_total <- combined_header_data_total %>%
  mutate(
    H2O_DIST = coalesce(H2O_DIST, H20_DIST)
  ) %>%
  select(-H20_DIST)

#standardizing entries in column "SITE"
combined_header_data_total <- combined_header_data_total %>%
  mutate(
    SITE = case_when(

      # Sloughbank variants
      tolower(SITE) %in% c("slbnk", "slbnki", "slbnkt", "slbnko", "sloughbank",
                                  "sloughbank/poolshore", "bnk") ~ "sloughbank",

      # Pondshore variants
      tolower(SITE) %in% c("polshr", "polsho", "poolsh", "poolshore", "pondshore",
                                  "pondshore/peninsula", "laksho", "lakshr", "laksh",
                                  "lshore", "lakeshore") ~ "pondshore",

      # Island variants
      tolower(SITE) %in% c("island", "islan") ~ "island",

      # Displaced island
      tolower(SITE) %in% c("dispis", "displaced island") ~ "displaced_island",

      # Grassflat variants
      tolower(SITE) %in% c("gflat", "gras", "grassflat", "grassflat/sloughbank", "meadow") ~ "grassflat",

      # Peninsula variants
      tolower(SITE) %in% c("penins", "penin", "ppenin", "peninsula") ~ "peninsula",

      # Pingo
      tolower(SITE) %in% c("pingo") ~ "pingo"

    )
  )

#Export combined data frame to .csv
write.csv(combined_header_data_total, "output/combined_header_data_1994-2015.csv")

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

#Combining "PREFIXNUMB" and "BANDNUMBER" into the "BANDNU_COMPLETE" column to create a column that includes the entire band number
combined_markdata_total$BANDNU_COMPLETE <- paste0(combined_markdata_total$PREFIXNUMB, combined_markdata_total$BANDNUMBER)

# Combine BANDNU_COMPLETE and DATE into a unique ID column, labeled "MARK_ID"
combined_markdata_total$MARK_ID <- paste(combined_markdata_total$DATE,
                                         combined_markdata_total$BANDNU_COMPLETE,
                                         combined_markdata_total$NEST_NO,
                                         sep = "_")

#Moving "MARK_ID" to front of data frame for ease of viewing
combined_markdata_total <- combined_markdata_total %>% relocate(MARK_ID, .before = BAND)


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
combined_markdata_total$LATITITUDE[combined_markdata_total$EASTING == 0 & combined_markdata_total$NORTHING == 0] <- NA
combined_markdata_total$LONGITUDE[combined_markdata_total$EASTING == 0 & combined_markdata_total$NORTHING == 0] <- NA

#Plot the converted LAT LON values
leaflet(combined_markdata_total)%>%
  addProviderTiles('Esri.WorldImagery')%>%
  addCircleMarkers(
    lng = ~LONGITUDE,
    lat = ~LATITITUDE,
    radius = 4,
    color = "yellow",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste("MARK_ID: ", combined_markdata_total$MARK_ID)
  )

#identify data points that have lat long values outside the Kigigak area

#Define Kig area boundaries
min_lat <- 60.814000
max_lat <- 60.879000
min_lon <- -165.029600
max_lon <- -164.883000

#Filter for rows outside Kig area
markdata_rows_outside_kig <- combined_markdata_total %>% filter((LATITITUDE < min_lat) |
                                                                   (LATITITUDE > max_lat) |
                                                                   (LONGITUDE < min_lon) |
                                                                   (LONGITUDE > max_lon)
)

#Remove columns that have only NAs
combined_markdata_total <- combined_markdata_total %>% select_if(~!all(is.na(.)))
  #manually removing columns that contain only NAs and 0s (and the 0s in this case are equivilent to NAs)
  combined_markdata_total <-  combined_markdata_total[, !(colnames(combined_markdata_total) %in% c("BAND", "BANDNU"))]

#Merging columns TARSALCODE and TARSAL
  combined_markdata_total <- combined_markdata_total %>%
    mutate(
      TARSALCODE = coalesce(TARSALCODE, TARSAL)
    ) %>%
    select(-TARSAL)

 #Merging columns NASALCODE and NASAL
   combined_markdata_total <- combined_markdata_total %>%
    mutate(
      NASALCODE = coalesce(NASALCODE, NASAL)
    ) %>%
    select(-NASAL)


#Standardizing entries in column "AGE"
  combined_markdata_total <- combined_markdata_total %>%
    mutate(
      AGE = case_when(

        # Local variants
        tolower(AGE) %in% c("loc" ) ~ "L",

        #After hatch year
        tolower(AGE) %in% c("ahy") ~ "AHY",

        #After third year
        tolower(AGE) %in% c("aty") ~ "ATY",

        #After second year
        tolower(AGE) %in% c("asy") ~ "ASY",

        #Third year
        tolower(AGE) %in% c("ty") ~ "TY",

        #Unknown
        tolower(AGE) %in% c("unk") ~ "UNK",

        #Second year
        tolower(AGE) %in% c("sy") ~ "SY"

      )
    )


#Exporting combined data frame to .csv
write.csv(combined_markdata_total, "output/combined_markdata_1994-2015.csv")

###################################

#combining resight data from 1994-2005 and 2006-2015

#standardizing the NORTHING column to be able to combine
resight_combined_data1994.2005$NORTHING <- as.character(resight_combined_data1994.2005$NORTHING)
resight_combined_data2006.2015$NORTHING <- as.character(resight_combined_data2006.2015$NORTHING)

#combining data frames
combined_resight_data_total <- bind_rows(resight_combined_data1994.2005, resight_combined_data2006.2015)

#Changing incorrect years "1900" and "1902" to correct "2000" and "2002"
combined_resight_data_total <- combined_resight_data_total %>%
  mutate(DATE = case_when(
    year(DATE) == 1900 ~ update(DATE, year = 2000),
    year(DATE) == 1902 ~ update(DATE, year = 2002),
    TRUE ~ DATE
  ))

#Combining columns 'TARSUS', 'TARSAL', and 'TARSALCODE', as they collect the same data.
combined_resight_data_total$TARSALCODE <-
  coalesce(combined_resight_data_total$TARSUS, combined_resight_data_total$TARSAL)
  #Dropping original 'TARSUS' and 'TARSAL' columns, as they are redundant now
combined_resight_data_total <- combined_resight_data_total %>%
  select(-TARSUS, -TARSAL)


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
combined_resight_data_total$LAT[combined_resight_data_total$EASTING == 0 & combined_resight_data_total$NORTHING == 0] <- NA
combined_resight_data_total$LON[combined_resight_data_total$EASTING == 0 & combined_resight_data_total$NORTHING == 0] <- NA

#Plot the converted LAT LON values
leaflet(combined_resight_data_total)%>%
  addProviderTiles('Esri.WorldImagery')%>%
  addCircleMarkers(
    lng = ~LON,
    lat = ~LAT,
    radius = 4,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste("TARSALCODE", combined_resight_data_total$TARSALCODE)
  )

#identify data points that have lat long values outside the Kigigak area

#Define Kig area boundaries
min_lat <- 60.814000
max_lat <- 60.879000
min_lon <- -165.029600
max_lon <- -164.883000

#Filter for rows outside Kig area
resight_rows_outside_kig <- combined_resight_data_total %>% filter((LAT < min_lat) |
                                                                  (LAT > max_lat) |
                                                                  (LON < min_lon) |
                                                                  (LON > max_lon)
)


#Remove columns that have only NAs
combined_resight_data_total <- combined_resight_data_total %>% select_if(~!all(is.na(.) | . == 0))

#Merging columns COMMENTS and Comments
combined_resight_data_total <- combined_resight_data_total %>%
  mutate(
    COMMENTS = coalesce(COMMENTS, Comments)
  ) %>%
  select(-Comments)

#Standardizing entries in column "FIRST_MARK"
combined_resight_data_total <- combined_resight_data_total %>%
  mutate(
    FIRST_MARK = case_when(

      # NONE variants
      tolower(FIRST_MARK) %in% c("none", "non", "none", "nonw" ) ~ "NONE",

      #NASAL variants
      tolower(FIRST_MARK) %in% c("nasa", "nasl", "nasal") ~ "NASAL",

      #TARSAL variants
      tolower(FIRST_MARK) %in% c("tars", "tar", "tarsal") ~ "TARSAL",

      #Unknown variants
      tolower(FIRST_MARK) %in% c("unk") ~ "UNK",

      # META ?
      tolower(FIRST_MARK) %in% c("meta") ~ "META"
    )
  )

#Standardizing entries in column "RESIGHT_METHOD"
combined_resight_data_total <- combined_resight_data_total %>%
  mutate(
    RESIGHT_METHOD = case_when(

      #Camera resights
      tolower(RESIGHT_METHOD) %in% c("camera", "tarsal via camera, nasal disk visually" ) ~ "CAMERA",

      #VISUAL variants
      tolower(RESIGHT_METHOD) %in% c("visual", "scope") ~ "VISUAL",

      #RECAP variants
      tolower(RESIGHT_METHOD) %in% c("recap", "recap", "trapped") ~ "RECAP",

      #V/C?
      tolower(RESIGHT_METHOD) %in% c("v/c") ~ "V/C"

    )
  )
    #adding original, long text value from one entry in RESIGHT_METHOD to comments, after changing value to proper data value above
    combined_resight_data_total$COMMENTS[6745] <- "nasal disk resighted visually, tarsal band resighted via camera"


#Export the combined data frame to .csv
write.csv(combined_resight_data_total, "output/combined_resight_data_1994-2015.csv")


####################################

#Combining visit data from 1994-2005 and 2006-2015

# Convert the PHOTO column to character in both data frames
visit_combined_data1994.2005$PHOTO <- as.character(visit_combined_data1994.2005$PHOTO)
visit_combined_data2006.2015$PHOTO <- as.character(visit_combined_data2006.2015$PHOTO)

#Convert TIME column to integer in both data frames
visit_combined_data1994.2005$TIME <- as.integer(visit_combined_data1994.2005$TIME)
visit_combined_data2006.2015$TIME <- as.integer(visit_combined_data2006.2015$TIME)

#Convert CANDLE2 column to integer in both data frames
visit_combined_data1994.2005$CANDLE2 <- as.double(visit_combined_data1994.2005$CANDLE2)
visit_combined_data2006.2015$CANDLE2 <- as.double(visit_combined_data2006.2015$CANDLE2)

#Combining the two data frames into one
combined_visit_data_total <- bind_rows(visit_combined_data1994.2005, visit_combined_data2006.2015)

#Changing incorrect years "1900" and "1902" to correct "2000" and "2002"
combined_visit_data_total <- combined_visit_data_total %>%
  mutate(DATE = case_when(
    year(DATE) == 1900 ~ update(DATE, year = 2000),
    year(DATE) == 1902 ~ update(DATE, year = 2002),
    TRUE ~ DATE
  ))

#Creating a unique ID column by combining NEST_NO and DATE
combined_visit_data_total$VISIT_ID <- paste(combined_visit_data_total$DATE, combined_visit_data_total$NEST_NO,
                                         sep = "_")
#Ensure there are no repeated rows of data by "VISIT_ID"
combined_visit_data_total <- combined_visit_data_total %>% distinct(VISIT_ID, .keep_all = TRUE)
#Move the "VISIT_ID" column to the left for ease of viewing
combined_visit_data_total <- combined_visit_data_total[, c("VISIT_ID", setdiff(names(combined_visit_data_total), "VISIT_ID"))]

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
combined_visit_data_total$LAT[combined_visit_data_total$EASTING == 0 & combined_visit_data_total$NORTHING == 0] <- NA
combined_visit_data_total$LON[combined_visit_data_total$EASTING == 0 & combined_visit_data_total$NORTHING == 0] <- NA

#Plot the converted LAT LON values
leaflet(combined_visit_data_total)%>%
  addProviderTiles('Esri.WorldImagery')%>%
  addCircleMarkers(
    lng = ~LON,
    lat = ~LAT,
    radius = 4,
    color = "pink",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste("NEST_NO", combined_visit_data_total$NEST_NO)
  )

#identify data points that have lat long values outside the Kigigak area

#Define Kig area boundaries
min_lat <- 60.814000
max_lat <- 60.879000
min_lon <- -165.029600
max_lon <- -164.883000

#Filter for rows outside Kig area
visit_rows_outside_kig <- combined_visit_data_total %>% filter((LAT < min_lat) |
                                                                  (LAT > max_lat) |
                                                                  (LON < min_lon) |
                                                                  (LON > max_lon)
)


#Remove columns that have only NAs
combined_visit_data_total <- combined_visit_data_total %>% select_if(~!all(is.na(.)))

#Merging columns HEN_STAT and HEN_STATUS
combined_visit_data_total <- combined_visit_data_total %>%
  mutate(
    HEN_STAT = coalesce(HEN_STAT, HEN_STATUS)
  ) %>%
  select(-HEN_STATUS)

#Merging columns EGGS_INVI and EGGS_INVIABLE
combined_visit_data_total <- combined_visit_data_total %>%
  mutate(
    EGGS_INVI = coalesce(EGGS_INVI, EGGS_INVIABLE)
  ) %>%
  select(-EGGS_INVIABLE)


#Standardize STATUS column entries
combined_visit_data_total <- combined_visit_data_total %>%
  mutate(
    STATUS = case_when(

      #Incubate variants
      tolower(STATUS) %in% c("incubating", "incuba", "incub", "incubat", "incubatng", "i", "i?" ) ~ "INCUBATE",

      #MISSING/DESTROYED variants
      tolower(STATUS) %in% c("failed", "1 egg broken", "a or p", "a/p?") ~ "MISSING/DESTROYED",

      #HATCHING variants
      tolower(STATUS) %in% c("pipped", "pip") ~ "HATCHING",

      #HATCHED variants
      tolower(STATUS) %in% c("hatched", "ducklings", "hatch", "hatche", "1 hatched", "2 hatched", "ducklin", "h", "brood", "hatched?", "hatched/trapped") ~ "HATCHED",

      #COLD variants
      tolower(STATUS) %in% c("abandoned", "abandon", "abando", "abandned", "a", "a?") ~ "COLD",

      #LAYING variants
      tolower(STATUS) %in% c("laying", " l", "l/i") ~ "LAYING",

      #PREDATED variants
      tolower(STATUS) %in% c("predated", "depred", "depredated", "predat", "predate", "p", "d") ~ "PREDATED",

      #UNKNOWN variants
      tolower(STATUS) %in% c("unknown", "u", "h/p?") ~ "UNKNOWN",

      #TRAP variants
      tolower(STATUS) %in% c("t", "trapped", "trappeding", "trappe") ~ "TRAP",

      #OTHER
      tolower(STATUS) %in% c("other", "resighted", "ok") ~ "OTHER"
    )
  )

#Export combined data frame as a .csv
write.csv(combined_visit_data_total, "output/combined_visit_data_1994-2015.csv")

