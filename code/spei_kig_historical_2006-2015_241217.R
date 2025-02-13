#'#####################################################################################
#' Project: [Kigigak Island Spectacled Eider Historical Data Standardization and Compilation]
#' Contents:
#'  - Identifying all file types in folders from Eider2006-Eider2015
#'  - Identifying file names and column names within each file for the .xls and .xlsx files from folders Eider2006-Eider2015
#'  -
#'  Author: [Ali McCarron]
#'
#'######################################################################################

install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos = 'http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c("workflowr", "tidyverse", "foreign", "dplyr", "here", "tools", "stringr", "readxl", "purrr")
sapply(package_vec, install.load.package)
#######################################################

#Identifying file types, file names, and columns

#Identifying all file types within folders Eider1994-Eider2005

# List of folder paths to process
folders_pt2 <- c("data/Eider2006",
             "data/Eider2007",
             "data/Eider2008",
             "data/Eider2009",
             "data/Eider2010",
             "data/Eider2011",
             "data/Eider2012",
             "data/Eider2013",
             "data/Eider2014",
             "data/Eider2015")

# Define a function to process each folder and extract file types
get_file_types <- function(folder_path) {
  # List all files in the folder
  files <- list.files(path = folder_path, full.names = TRUE)
  # Extract file extensions
  file_extensions <- file_ext(files)
  # Get unique file types (extensions)
  unique_file_types <- unique(file_extensions)
  return(unique_file_types)}

# Apply the function to each folder and store the results
file_types_per_folder_pt2 <- lapply(folders_pt2, get_file_types)

# Extract just the folder names (not the full paths)
folder_names <- rep(basename(folders_pt2), times = sapply(file_types_per_folder_pt2, length))

# Flatten the list of unique file types
file_types <- unlist(file_types_per_folder_pt2)

# Create a data frame with folder names and their respective file types
file_types_df_pt2 <- data.frame(
  Folder = folder_names,
  File_Type = file_types)

#identify all unique file types across folders 2005-2015
file_types_all_pt2 <- unique(file_types_df_pt2$File_Type)

#########################################################################################

#Identifying all excel files and column names within each file from folders Eider2006-Eider2015

# Define a function to process a single folder
process_folder <- function(folder_path) {
  # List all Excel files in the folder
  files <- list.files(path = folder_path, pattern = "\\.xls[x]?$", full.names = TRUE, ignore.case = TRUE)

  # Function to extract column names from each DBF file
  get_file_columns <- function(file_path) {
    tryCatch({
      # Read the DBF file
      df <- read_excel(file_path)
      # Return the file name and column names
      data.frame(
        folder = basename(folder_path),  # Include the folder name
        file_name = basename(file_path),  # Extract just the file name
        column_names = paste(colnames(df), collapse = ", ")  # Concatenate column names
      )}, error = function(e) {
        data.frame(
          folder = basename(folder_path),
          file_name = basename(file_path),
          column_names = NA  # If there's an error, return NA for column names
        )})}

  # Apply the function to all DBF files and combine the results into a dataframe
  file_info_df <- do.call(rbind, lapply(files, get_file_columns))

  return(file_info_df)}

# Define a vector of folder paths to Eider data from 1994-2005
eider_data_folders_2006.2015 <- c("data/Eider2006",
                                  "data/Eider2007",
                                  "data/Eider2008",
                                  "data/Eider2009",
                                  "data/Eider2010",
                                  "data/Eider2011",
                                  "data/Eider2012",
                                  "data/Eider2013",
                                  "data/Eider2014",
                                  "data/Eider2015")

# Apply the process_folder function to each folder and combine the results
combined_eider_data_names_2006.2015 <- do.call(rbind, lapply(eider_data_folders_2006.2015, process_folder))

# View the resulting combined dataframe
print(combined_eider_data_names_2006.2015)
#Dataframe includes folder names, file names of .dbf files within each folder, and column names within each file.

####################################################################################

#Create a reference data frame with columns specified as needed in the project protocols
#This data frame will have no data in it, it is just used as a guide to compare the collected data sets to

#############

#Reference data frame for "Header" data

# Define the expected column names (reference)
expected_columns_Header <- c("NEST_NO", "SPECIES", "STUDYAREA", "SITE", "EASTING", "NORTHING")

# Create an empty data frame with these columns
Header_reference_df <- data.frame(matrix(ncol = length(expected_columns_Header), nrow = 0))
colnames(Header_reference_df) <- expected_columns_Header

##############

#Reference data frame for "markdata" data

# Define the expected column names (reference)
expected_columns_markdata <- c("PREFIXNUMB", "BANDNUMBER", "RECAP", "SPECIESCOD", "AGE", "SEX", "WT", "CULMEN", "TARSUS", "DATE", "WING", "NASALCODE", "TARSALCODE", "NEST_NO", "EASTING", "NORTHING")

# Create an empty data frame with these columns
markdata_reference_df <- data.frame(matrix(ncol = length(expected_columns_markdata), nrow = 0))
colnames(markdata_reference_df) <- expected_columns_markdata

################

#Reference data frame for "resight" data

# Define the expected column names (reference)
expected_columns_resight <- c("CAPTURE", "FIRST_MARK", "TARSALCODE", "NASAL", "SEX", "NEST_NO", "DATE", "TIME", "OBS", "EASTING", "NORTHING", "ASSOC", "COMMENTS")

# Create an empty data frame with these columns
resight_reference_df <- data.frame(matrix(ncol = length(expected_columns_resight), nrow = 0))
colnames(resight_reference_df) <- expected_columns_resight

#################

#Reference data frame for "visit" data

# Define the expected column names (reference)
expected_columns_visit <- c("NEST_NO", "SPECIES", "DATE", "OBS", "NO_EGGS", "EGGS_NEW", "EGGS_MISS", "WARM", "DOWN", "HEN_STAT", "DRAKE", "FLOAT1", "FLOAT2", "CANDLE1", "CANDLE2", "STATUS", "COMMENTS")

# Create an empty data frame with these columns
visit_reference_df <- data.frame(matrix(ncol = length(expected_columns_visit), nrow = 0))
colnames(visit_reference_df) <- expected_columns_visit

#################

#Reference data frame for "egg" data

# Define the expected column names (reference)
expected_columns_egg <- c("NEST_NO", "SPECIES", "OBS", "EGGNO", "LENGTH", "WIDTH", "LINK", "WEBTAG", "TAGDATE")

# Create an empty data frame with these columns
egg_reference_df <- data.frame(matrix(ncol = length(expected_columns_egg), nrow = 0))
colnames(egg_reference_df) <- expected_columns_egg

########################################################################################

#Compare the excel files stored in the "data" folder to the reference data frames to identify differences
#Differences will be defined as "missing" or "extra"
#############
#comparing Header excel files to reference files

# List all excel files from those folders, including only files that have 'header' in the name
Header_excel_files_eider2006.2015 <- unlist(lapply(eider_data_folders_2006.2015, function(folder) {
  # List excel files and filter for files with "header" in the name
  excel_files_all <- list.files(path = folder, pattern = "\\.xls[x]?$", full.names = TRUE, ignore.case = TRUE)
  excel_files_filtered <- excel_files_all[grepl("header", basename(excel_files_all), ignore.case = TRUE)]
  return(excel_files_filtered)
}))
# View the filtered list of excel files with 'header' in the name
print(Header_excel_files_eider2006.2015)

#Comparing columns of the Header files to the Header reference dataframe

# Function to compare column names of a excel file with the reference data frame
compare_columns <- function(excel_file, Header_reference_df) {
  # Read the excel file
  excel_data <- read_excel(excel_file)

  # Get column names
  excel_cols <- colnames(excel_data)
  Header_reference_cols <- colnames(Header_reference_df)

  # Convert excel file column names to uppercase to ignore case differences
  excel_cols_upper <- toupper(excel_cols)

  # Identify differences in column names
  missing_in_reference <- setdiff(excel_cols_upper, Header_reference_cols)
  missing_in_excel <- setdiff(Header_reference_cols, excel_cols_upper)

  # Return the result
  list(
    excel_file = excel_file,
    missing_in_reference = missing_in_reference,
    missing_in_excel = missing_in_excel
  )
}

# Apply the comparison function to all filtered .dbf files with 'header' in the name
Header_comparison_results2006.2015 <- lapply(Header_excel_files_eider2006.2015, compare_columns, Header_reference_df = Header_reference_df)
#years 2006, 2009-2015 are missing the field: STUDYAREA which will need to be added to those years of data. All data was collected on KIGI
#years 2009-2015 have columns "NEST #" and "NEST SITE" which will need to be changed slightly to "NEST_NO" and "SITE" to match reference df
#There are several columns in each excel file that do not occur in the reference df, these will be combined using NAs if it is for data that was not collected in every year.

#####################
#comparing markdata excel files to reference files

# List all excel files from those folders, including only files that have 'markdata' in the name
markdata_excel_files_eider2006.2015 <- unlist(lapply(eider_data_folders_2006.2015, function(folder) {
  # List excel files and filter for files with "markdata" or "mark" in the name (in 2006, markdata was named MARK)
  excel_files_all <- list.files(path = folder, pattern = "\\.xls[x]?$", full.names = TRUE, ignore.case = TRUE)
  excel_files_filtered <- excel_files_all[grepl("mark|markdata", basename(excel_files_all), ignore.case = TRUE)]
  return(excel_files_filtered)
}))
# View the filtered list of excel files with 'markdata' in the name
print(markdata_excel_files_eider2006.2015)

#Comparing columns of the Header files to the markdata reference dataframe

# Function to compare column names of a excel file with the reference data frame
compare_columns <- function(excel_file, markdata_reference_df) {
  # Read the excel file
  excel_data <- read_excel(excel_file)

  # Get column names
  excel_cols <- colnames(excel_data)
  markdata_reference_cols <- colnames(markdata_reference_df)

  # Convert excel file column names to uppercase to ignore case differences
  excel_cols_upper <- toupper(excel_cols)

  # Identify differences in column names
  missing_in_reference <- setdiff(excel_cols_upper, markdata_reference_cols)
  missing_in_excel <- setdiff(markdata_reference_cols, excel_cols_upper)

  # Return the result
  list(
    excel_file = excel_file,
    missing_in_reference = missing_in_reference,
    missing_in_excel = missing_in_excel
  )
}

# Apply the comparison function to all filtered .dbf files with 'markdata' in the name
markdata_comparison_results2006.2015 <- lapply(markdata_excel_files_eider2006.2015, compare_columns, markdata_reference_df = markdata_reference_df)
#There is no markdata collected for years 2007 and 2015
#years 2006 and 2008 columns 'NASAL' and 'TARSAL' need to be changed to 'NASALCODE' and 'TARSALCODE'
#years 2009-2011 columns 'NASAL' 'TARSAL' 'NEST #' 'BAND NUMBER' 'SPECIES' need changed to 'NASALCODE' 'TARSALCODE' 'NEST_NO' 'BANDNUMBER' 'SPECIESCOD'
#years 2012-2014 columns 'NASAL' 'TARSAL' 'NEST #' 'BAND NUMBER' 'SPECIES' 'WT (G)' CULMEN (MM)' 'TARSUS (MM)' need changed to 'NASALCODE' 'TARSALCODE' 'NEST_NO' 'BANDNUMBER' 'SPECIESCOD' 'WT' 'CULMEN' 'TARSUS'


############################

#comparing resight excel files to reference files

# List all excel files from those folders, including only files that have 'resight' in the name
resight_excel_files_eider2006.2015 <- unlist(lapply(eider_data_folders_2006.2015, function(folder) {
  # List excel files and filter for files with 'resight' in the name
  excel_files_all <- list.files(path = folder, pattern = "\\.xls[x]?$", full.names = TRUE, ignore.case = TRUE)
  excel_files_filtered <- excel_files_all[grepl("resight", basename(excel_files_all), ignore.case = TRUE)]
  return(excel_files_filtered)
}))
# View the filtered list of excel files with 'markdata' in the name
print(resight_excel_files_eider2006.2015)

#Comparing columns of the Header files to the markdata reference dataframe

# Function to compare column names of a excel file with the reference data frame
compare_columns <- function(excel_file, resight_reference_df) {
  # Read the excel file
  excel_data <- read_excel(excel_file)

  # Get column names
  excel_cols <- colnames(excel_data)
  resight_reference_cols <- colnames(resight_reference_df)

  # Convert excel file column names to uppercase to ignore case differences
  excel_cols_upper <- toupper(excel_cols)

  # Identify differences in column names
  missing_in_reference <- setdiff(excel_cols_upper, resight_reference_cols)
  missing_in_excel <- setdiff(resight_reference_cols, excel_cols_upper)

  # Return the result
  list(
    excel_file = excel_file,
    missing_in_reference = missing_in_reference,
    missing_in_excel = missing_in_excel
  )
}

# Apply the comparison function to all filtered excel files with 'resight' in the name
resight_comparison_results2006.2015 <- lapply(resight_excel_files_eider2006.2015, compare_columns, resight_reference_df = resight_reference_df)


################################

#comparing Visit excel files to reference files

# List all excel files from those folders, including only files that have 'visit' in the name
visit_excel_files_eider2006.2015 <- unlist(lapply(eider_data_folders_2006.2015, function(folder) {
  # List excel files and filter for files with 'visit' in the name
  excel_files_all <- list.files(path = folder, pattern = "\\.xls[x]?$", full.names = TRUE, ignore.case = TRUE)
  excel_files_filtered <- excel_files_all[grepl("visit", basename(excel_files_all), ignore.case = TRUE)]
  return(excel_files_filtered)
}))
# View the filtered list of excel files with 'markdata' in the name
print(visit_excel_files_eider2006.2015)

#Comparing columns of the Header files to the markdata reference dataframe

# Function to compare column names of a excel file with the reference data frame
compare_columns <- function(excel_file, visit_reference_df) {
  # Read the excel file
  excel_data <- read_excel(excel_file)

  # Get column names
  excel_cols <- colnames(excel_data)
  visit_reference_cols <- colnames(visit_reference_df)

  # Convert excel file column names to uppercase to ignore case differences
  excel_cols_upper <- toupper(excel_cols)

  # Identify differences in column names
  missing_in_reference <- setdiff(excel_cols_upper, visit_reference_cols)
  missing_in_excel <- setdiff(visit_reference_cols, excel_cols_upper)

  # Return the result
  list(
    excel_file = excel_file,
    missing_in_reference = missing_in_reference,
    missing_in_excel = missing_in_excel
  )
}

# Apply the comparison function to all filtered .dbf files with 'header' in the name
visit_comparison_results2006.2015 <- lapply(visit_excel_files_eider2006.2015, compare_columns, visit_reference_df = visit_reference_df)

########################################################################################
#Combining the excel files from each year into one data frame for respective category (e.g. 'Header' data)

########################
#Combining all "header" data from 2006-2015

# Initialize an empty list to store data frames
header_data_list2006.2015 <- list()

# Initialize an empty vector to store all possible column names
all_columns <- NULL

# Define a mapping of column names that should be unified across all files
# This is a list where the key is the original column name and the value is the new standardized column name
column_rename_map <- list(
  "Northing" = "NORTHING",   # Rename'Northing' to 'NORTHING'
  "Easting" = "EASTING",     # Rename 'Easting' to 'EASTING'
  "Nest #" = "NEST_NO",      # Rename 'Nest #' to 'NEST_NO'
  "Nest Site" = "SITE"       # Rename 'Nest Site' to 'SITE'
)



# First loop: Collect all unique column names across all files
for (file in Header_excel_files_eider2006.2015) {
  # Read the .dbf files into a data frame
  df <- read_excel(file)

  # Extract the year (from folder name) from the file path (e.g., "1994" from "Eider1994")
  folder_name <- sub(".*(\\d{4}).*", "\\1", basename(dirname(file)))

  # Add the folder name (year) as a new column
  df$Year <- folder_name

  # Apply column renaming based on the predefined mapping
  colnames(df) <- sapply(colnames(df), function(x) {
    # Use the map if the column exists in the rename map, otherwise leave as is
    if (x %in% names(column_rename_map)) {
      return(column_rename_map[[x]])
    } else {
      return(x)
    }
  })

  # Store all column names to determine the full set of columns
  all_columns <- union(all_columns, colnames(df))

  # Handle duplicate or unnamed columns (e.g., `...9`, `...19`)
  colnames(df) <- make.names(colnames(df), unique = TRUE)

  # Add missing columns (if any) and fill them with NA
  missing_columns <- setdiff(all_columns, colnames(df))
  for (col in missing_columns) {
    df[[col]] <- NA
  }

  # Check and ensure 'Northing' exists, otherwise create it with NA
  if (!"Northing" %in% colnames(df)) {
    df$Northing <- NA
  } else {
    df$Northing <- as.double(df$Northing)
  }

  # Check and ensure 'Easting' exists, otherwise create it with NA
  if (!"Easting" %in% colnames(df)) {
    df$Easting <- NA
  } else {
    df$Easting <- as.character(df$Easting)  # Convert 'Easting' to character for consistency
  }

  # Store the modified data frame in the list
  header_data_list2006.2015[[file]] <- df
}

# Second loop: Adjust data frames to match the full set of columns
for (i in 1:length(header_data_list2006.2015)) {
  df <- header_data_list2006.2015[[i]]

  # Apply column renaming again in case any file had other inconsistent names
  colnames(df) <- sapply(colnames(df), function(x) {
    if (x %in% names(column_rename_map)) {
      return(column_rename_map[[x]])
    } else {
      return(x)
    }
  })

  # Ensure all columns have the same names as in all_columns
  missing_columns <- setdiff(all_columns, colnames(df))
  for (col in missing_columns) {
    df[[col]] <- NA
  }

  # Convert 'Easting' to character (for consistency across all data frames)
  if ("EASTING" %in% colnames(df)) {
    df$EASTING <- as.character(df$EASTING)  # Ensure 'Easting' is consistently character
  }

  # Convert 'Northing' to character (for consistency across all data frames)
  if ("NORTHING" %in% colnames(df)) {
    df$NORTHING <- as.character(df$NORTHING)  # Ensure 'Easting' is consistently character
  }

  # Add the updated data frame back to the list
  header_data_list2006.2015[[i]] <- df
}

# Combine all the data frames into one
header_combined_data2006.2015 <- bind_rows(header_data_list2006.2015)

#############################
#Combining all "Markdata" data from 2006-2015

# Initialize an empty list to store data frames
markdata_data_list2006.2015 <- list()

# Initialize an empty vector to store all possible column names
all_columns <- NULL

# Define a mapping of column names that should be unified across all files
# This is a list where the key is the original column name and the value is the new standardized column name
column_rename_map_markdata <- list(
  "NASAL" = "NASALCODE",
  "TARSAL" = "TARSALCODE",
  "Nest #" = "NEST_NO",
  "Band Number" = "BANDNUMBER",
  "SPECIES" = "SPECIESCOD",
  "WT(g)" = "WT",
  "CULMEN (mm)" = "CULMEN",
  "TARSUS (mm)" = "TARSUS",
  "PLOT #" = "PLOT"
)


# First loop: Collect all unique column names across all files
for (file in markdata_excel_files_eider2006.2015) {
  # Read the excel files into a data frame
  df <- read_excel(file)

  # Extract the year (from folder name) from the file path (e.g., "1994" from "Eider1994")
  folder_name <- sub(".*(\\d{4}).*", "\\1", basename(dirname(file)))

  # Add the folder name (year) as a new column
  df$YEAR <- folder_name

  # Apply column renaming based on the predefined mapping
  colnames(df) <- sapply(colnames(df), function(x) {
    if (x %in% names(column_rename_map_markdata)) {
      return(column_rename_map_markdata[[x]])
    } else {
      return(x)
    }
  })

  # Standardize 'RECAP' column to logical type if it exists
  if ("RECAP" %in% colnames(df)) {
    df$RECAP <- as.logical(df$RECAP)  # Convert 'RECAP' to logical (TRUE/FALSE)
  } else {
    df$RECAP <- NA  # If 'RECAP' doesn't exist, assign NA
  }

  # Standardize 'EASTING' column to numeric type if it exists
  if ("EASTING" %in% colnames(df)) {
    df$EASTING <- as.numeric(df$EASTING)  # Convert 'EASTING' to numeric
  } else {
    df$EASTING <- NA  # If 'EASTING' doesn't exist, assign NA
  }

  # Standardize 'PLOT' column to character type if it exists
  if ("PLOT" %in% colnames(df)) {
    df$PLOT <- as.character(df$PLOT)  # Convert 'PLOT' to character
  } else {
    df$PLOT <- NA  # If 'PLOT' doesn't exist, assign NA
  }

  # Store all column names to determine the full set of columns
  all_columns <- union(all_columns, colnames(df))

  # Handle duplicate or unnamed columns (e.g., `...9`, `...19`)
  colnames(df) <- make.names(colnames(df), unique = TRUE)

  # Add missing columns (if any) and fill them with NA
  missing_columns <- setdiff(all_columns, colnames(df))
  for (col in missing_columns) {
    df[[col]] <- NA
  }

  # Store the modified data frame in the list
  markdata_data_list2006.2015[[file]] <- df
}

# Second loop: Adjust data frames to match the full set of columns and ensure column types are consistent
for (i in 1:length(markdata_data_list2006.2015)) {
  df <- markdata_data_list2006.2015[[i]]

  # Apply column renaming again in case any file had other inconsistent names
  colnames(df) <- sapply(colnames(df), function(x) {
    if (x %in% names(column_rename_map_markdata)) {
      return(column_rename_map_markdata[[x]])
    } else {
      return(x)
    }
  })

  # Ensure all columns have the same names as in all_columns
  missing_columns <- setdiff(all_columns, colnames(df))
  for (col in missing_columns) {
    df[[col]] <- NA
  }

  # Standardize specific columns to appropriate types:

  # Ensure 'RECAP' is standardized to logical type
  if ("RECAP" %in% colnames(df)) {
    df$RECAP <- as.logical(df$RECAP)
  }

  # Ensure 'EASTING' is standardized to numeric type
  if ("EASTING" %in% colnames(df)) {
    df$EASTING <- as.numeric(df$EASTING)
  }

  # Ensure 'NORTHING' is standardized to numeric type
  if ("NORTHING" %in% colnames(df)) {
    df$NORTHING <- as.numeric(df$NORTHING)
  }

  # Clean and standardize 'PLOT' column to numeric type, replacing invalid entries with NA
  if ("PLOT" %in% colnames(df)) {
    # Convert 'PLOT' to numeric, replacing non-numeric entries with NA
    df$PLOT <- as.character(df$PLOT)  # Ensure it's character type first
    df$PLOT <- trimws(df$PLOT)  # Remove any leading or trailing spaces

  }

  # Add the updated data frame back to the list
  markdata_data_list2006.2015[[i]] <- df
}

# Now, combine all the data frames into one
markdata_combined_data2006.2015 <- bind_rows(markdata_data_list2006.2015)



##################################
#Combining all "resight" data from 2006-2015

# Initialize an empty list to store data frames
resight_data_list2006.2015 <- list()

# Initialize an empty vector to store all possible column names
all_columns <- NULL

# Define a mapping of column names that should be unified across all files
# This is a list where the key is the original column name and the value is the new standardized column name
column_rename_map_resight <- list(
  "TARSAL" = "TARSALCODE",
  "Nest #" = "NEST_NO",
  "PLOT #" = "PLOT"
)


# First loop: Collect all unique column names across all files
for (file in resight_excel_files_eider2006.2015) {
  # Read the .dbf files into a data frame
  df <- read_excel(file)

  # Extract the year (from folder name) from the file path (e.g., "1994" from "Eider1994")
  folder_name <- sub(".*(\\d{4}).*", "\\1", basename(dirname(file)))

  # Add the folder name (year) as a new column
  df$Year <- folder_name

  # Apply column renaming based on the predefined mapping
  colnames(df) <- sapply(colnames(df), function(x) {
    # Use the map if the column exists in the rename map, otherwise leave as is
    if (x %in% names(column_rename_map_resight)) {
      return(column_rename_map_resight[[x]])
    } else {
      return(x)
    }
  })

  # Store all column names to determine the full set of columns
  all_columns <- union(all_columns, colnames(df))

  # Handle duplicate or unnamed columns (e.g., `...9`, `...19`)
  colnames(df) <- make.names(colnames(df), unique = TRUE)

  # Add missing columns (if any) and fill them with NA
  missing_columns <- setdiff(all_columns, colnames(df))
  for (col in missing_columns) {
    df[[col]] <- NA
  }

  # Store the modified data frame in the list
  resight_data_list2006.2015[[file]] <- df
}

# Second loop: Adjust data frames to match the full set of columns
for (i in 1:length(resight_data_list2006.2015)) {
  df <- resight_data_list2006.2015[[i]]

  # Apply column renaming again in case any file had other inconsistent names
  colnames(df) <- sapply(colnames(df), function(x) {
    if (x %in% names(column_rename_map_resight)) {
      return(column_rename_map_resight[[x]])
    } else {
      return(x)
    }
  })

  # Ensure all columns have the same names as in all_columns
  missing_columns <- setdiff(all_columns, colnames(df))
  for (col in missing_columns) {
    df[[col]] <- NA
  }

  # Standardize 'DATE' column to Date type
  if ("DATE" %in% colnames(df)) {
    df$DATE <- as.Date(df$DATE, format="%Y-%m-%d")  # Adjust format if necessary
  }

  # Ensure 'EASTING' is standardized to numeric type
  if ("EASTING" %in% colnames(df)) {
    df$EASTING <- as.numeric(df$EASTING)
  }

  # Ensure 'NORTHING' is standardized to numeric type
  if ("NORTHING" %in% colnames(df)) {
    df$NORTHING <- as.numeric(df$NORTHING)
  }

  # Standardize 'PLOT' column to character type if it exists
  if ("PLOT" %in% colnames(df)) {
    df$PLOT <- as.character(df$PLOT)  # Convert 'PLOT' to character
  } else {
    df$PLOT <- NA  # If 'PLOT' doesn't exist, assign NA
  }

  # Add the updated data frame back to the list
  resight_data_list2006.2015[[i]] <- df
}

# Combine all the data frames into one
resight_combined_data2006.2015 <- bind_rows(resight_data_list2006.2015)


#####################################
#Combining all "Visit" data from 2006-2015

# Initialize an empty list to store data frames
visit_data_list2006.2015 <- list()

# Initialize an empty vector to store all possible column names
all_columns <- NULL

# Define a mapping of column names that should be unified across all files
# This is a list where the key is the original column name and the value is the new standardized column name
column_rename_map_visit <- list(
  "Nest #" = "NEST_NO",
  "HEN STATUS" = "HEN_STATUS",
  "Total..Eggs" = "NO_EGGS",
  "New Eggs" = "EGGS_NEW",
  "NEST STATUS" = "STATUS",
  "VISIT #" = "VISIT",
  "Total...Eggs" = "NO_EGGS"
)


# First loop: Collect all unique column names across all files
for (file in visit_excel_files_eider2006.2015) {
  # Read the .dbf files into a data frame
  df <- read_excel(file)

  # Extract the year (from folder name) from the file path (e.g., "1994" from "Eider1994")
  folder_name <- sub(".*(\\d{4}).*", "\\1", basename(dirname(file)))

  # Add the folder name (year) as a new column
  df$Year <- folder_name

  # Apply column renaming based on the predefined mapping
  colnames(df) <- sapply(colnames(df), function(x) {
    # Use the map if the column exists in the rename map, otherwise leave as is
    if (x %in% names(column_rename_map_visit)) {
      return(column_rename_map_visit[[x]])
    } else {
      return(x)
    }
  })

  # Store all column names to determine the full set of columns
  all_columns <- union(all_columns, colnames(df))

  # Handle duplicate or unnamed columns (e.g., `...9`, `...19`)
  colnames(df) <- make.names(colnames(df), unique = TRUE)

  # Add missing columns (if any) and fill them with NA
  missing_columns <- setdiff(all_columns, colnames(df))
  for (col in missing_columns) {
    df[[col]] <- NA
  }

  # Store the modified data frame in the list
  visit_data_list2006.2015[[file]] <- df
}

# Second loop: Adjust data frames to match the full set of columns
for (i in 1:length(visit_data_list2006.2015)) {
  df <- visit_data_list2006.2015[[i]]

  # Apply column renaming again in case any file had other inconsistent names
  colnames(df) <- sapply(colnames(df), function(x) {
    if (x %in% names(column_rename_map_visit)) {
      return(column_rename_map_visit[[x]])
    } else {
      return(x)
    }
  })

  # Ensure all columns have the same names as in all_columns
  missing_columns <- setdiff(all_columns, colnames(df))
  for (col in missing_columns) {
    df[[col]] <- NA
  }

  # Standardize column types where necessary
  # Example: Standardize 'Total..Eggs' column to numeric (double)
  if ("NO_EGGS" %in% colnames(df)) {
    df$NO_EGGS <- as.double(df$NO_EGGS)
  }

  # Example: Standardize 'Missing..Eggs' column to numeric (double)
  if ("Missing..Eggs" %in% colnames(df)) {
    df$`Missing..Eggs` <- as.double(df$`Missing..Eggs`)
  }

  # Standardize 'Depredated.Eggs'
  if ("Depredated.Eggs" %in% colnames(df)) {
    df$`Depredated.Eggs` <- as.double(df$`Depredated.Eggs`)
  }

  # Standardize 'Inviable.Eggs'
  if ("Inviable.Eggs" %in% colnames(df)) {
    df$`Inviable.Eggs` <- as.double(df$`Inviable.Eggs`)
  }

  # Standardize 'Unknown.Eggs'
  if ("Unknown.Eggs" %in% colnames(df)) {
    df$Unknown.Eggs <- as.double(df$Unknown.Eggs)
  }

  # Standardize 'Unknown.Eggs'
  if ("Aband.Eggs" %in% colnames(df)) {
    df$`Aband.Eggs` <- as.double(df$`Aband.Eggs`)
  }

  # Standardize 'CANDLE2'
  if ("CANDLE2" %in% colnames(df)) {
    df$CANDLE2 <- as.double(df$CANDLE2)
  }

  # Standardize 'VISIT'
  if ("VISIT" %in% colnames(df)) {
    df$VISIT <- as.double(df$VISIT)
  }

  # Standardize 'DATE' column to Date type
  if ("DATE" %in% colnames(df)) {
    df$DATE <- as.Date(df$DATE, format="%Y-%m-%d")  # Adjust format if necessary
  }

  # Ensure 'EASTING' is standardized to numeric type
  if ("EASTING" %in% colnames(df)) {
    df$EASTING <- as.numeric(df$EASTING)
  }

  # Ensure 'NORTHING' is standardized to numeric type
  if ("NORTHING" %in% colnames(df)) {
    df$NORTHING <- as.numeric(df$NORTHING)
  }

  # Add the updated data frame back to the list
  visit_data_list2006.2015[[i]] <- df
}

# Combine all the data frames into one
visit_combined_data2006.2015 <- bind_rows(visit_data_list2006.2015)

