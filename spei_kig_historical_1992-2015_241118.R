#'#####################################################################################
#' Project: [Kigigak Island Spectacled Eider Historical Data Standardization and Compilation]
#' Contents:
#'  - Identifying all file types in folders from Eider1994-Eider2005
#'  - Identifying all .dbf files and column names within each file from folders Eider1994-Eider2005
#'  - Creating reference data frame for each of the tables to identify each of the required column names
#'  - Comparing the data collected in each year to the reference data frames, identifying the differences in column names
#'  -
#'  Author: [Ali McCarron]
#'
#'######################################################################################

library(workflowr)

library(tidyverse)
library(foreign)
library(dplyr)
library(here)
library(tools)

wflow_git_commit(all = TRUE)
?wflow_git_commit
#Identifying file types, file names, and columns

#Identifying all file types within folders Eider1994-Eider2005

# List of folder paths to process
folders <- c("data/Eider1994",
             "data/Eider1995",
             "data/Eider1996",
             "data/Eider1997",
             "data/Eider1998",
             "data/Eider1999",
             "data/Eider2000",
             "data/Eider2001",
             "data/Eider2002",
             "data/Eider2003",
             "data/Eider2004",
             "data/Eider2005")

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
file_types_per_folder <- lapply(folders, get_file_types)

# Extract just the folder names (not the full paths)
folder_names <- rep(basename(folders), times = sapply(file_types_per_folder, length))

# Flatten the list of unique file types
file_types <- unlist(file_types_per_folder)

# Create a data frame with folder names and their respective file types
file_types_df <- data.frame(
  Folder = folder_names,
  File_Type = file_types)

#identify all unique file types across folders 1994-2005
file_types_all <- unique(file_types_df$File_Type)

##########################################################################

#Identifying all .dbf files and column names within each file from folders Eider1994-Eider2005

# Define a function to process a single folder
process_folder <- function(folder_path) {
  # List all DBF files in the folder
  files <- list.files(path = folder_path, pattern = "\\.dbf$", full.names = TRUE, ignore.case = TRUE)

  # Function to extract column names from each DBF file
  get_file_columns <- function(file_path) {
    tryCatch({
      # Read the DBF file
      df <- read.dbf(file_path)
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
eider_data_folders_1994.2005 <- c("data/Eider1994",
                                  "data/Eider1995",
                                  "data/Eider1996",
                                  "data/Eider1997",
                                  "data/Eider1998",
                                  "data/Eider1999",
                                  "data/Eider2000",
                                  "data/Eider2001",
                                  "data/Eider2002",
                                  "data/Eider2003",
                                  "data/Eider2004",
                                  "data/Eider2005")

# Apply the process_folder function to each folder and combine the results
combined_eider_data_names_1994.2005 <- do.call(rbind, lapply(eider_data_folders_1994.2005, process_folder))

# View the resulting combined dataframe
print(combined_eider_data_names_1994.2005)
#Dataframe includes folder names, file names of .dbf files within each folder, and column names within each file.

#########################################################################################################

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
expected_columns_resight <- c("CAPTURE", "FIRST_MARK", "TARSUS", "NASAL", "SEX", "NEST_NO", "DATE", "TIME", "OBS", "EASTING", "NORTHING", "ASSOC", "COMMENTS")

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

##########################################################################################################

#Compare the .dbf files stored in the "data" folder to the reference data frames to identify differences
#Differences will be defined as "missing" or "extra"
#############
#comparing Header .dbf files to reference files

# List all .dbf files from those folders, including only files that have 'header' in the name
Header_dbf_files_eider1994.2005 <- unlist(lapply(eider_data_folders_1994.2005, function(folder) {
  # List .dbf files and filter for files with "header" in the name
  dbf_files_all <- list.files(path = folder, pattern = "\\.dbf$", full.names = TRUE, ignore.case = TRUE)
  dbf_files_filtered <- dbf_files_all[grepl("header", basename(dbf_files_all), ignore.case = TRUE)]
  return(dbf_files_filtered)
}))
# View the filtered list of .dbf files with 'header' in the name
print(Header_dbf_files_eider1994.2005)

#Comparing columns of the Header files to the Header reference dataframe

# Function to compare column names of a .dbf file with the reference data frame
compare_columns <- function(dbf_file, Header_reference_df) {
  # Read the .dbf file
  dbf_data <- read.dbf(dbf_file)

  # Get column names
  dbf_cols <- colnames(dbf_data)
  Header_reference_cols <- colnames(Header_reference_df)

  # Identify differences in column names
  missing_in_reference <- setdiff(dbf_cols, Header_reference_cols)
  missing_in_dbf <- setdiff(Header_reference_cols, dbf_cols)

  # Return the result
  list(
    dbf_file = dbf_file,
    missing_in_reference = missing_in_reference,
    missing_in_dbf = missing_in_dbf
  )
}

# Apply the comparison function to all filtered .dbf files with 'header' in the name
Header_comparison_results <- lapply(Header_dbf_files_eider1994.2005, compare_columns, Header_reference_df = Header_reference_df)
###For Header data, it appears that from the years 1994-2005, all of the .dbf files include at least the data columns required by the protocol
###This ensures there is no essential missing data from any of the Header.dbf data files from the years 1994-2005

#####################
#comparing Markdata .dbf files to reference files

# List all .dbf files from those folders, including only files that have 'markdata' in the name
markdata_dbf_files_eider1994.2005 <- unlist(lapply(eider_data_folders_1994.2005, function(folder) {
  # List .dbf files and filter for files with "markdata" in the name
  dbf_files_all <- list.files(path = folder, pattern = "\\.dbf$", full.names = TRUE, ignore.case = TRUE)
  dbf_files_filtered <- dbf_files_all[grepl("markdata", basename(dbf_files_all), ignore.case = TRUE)]
  return(dbf_files_filtered)
}))
# View the filtered list of .dbf files with 'markdata' in the name
print(markdata_dbf_files_eider1994.2005)

#Comparing columns of the markdata files to the markdata reference dataframe

# Function to compare column names of a .dbf file with the reference data frame
compare_columns <- function(dbf_file, markdata_reference_df) {
  # Read the .dbf file
  dbf_data <- read.dbf(dbf_file)

  # Get column names
  dbf_cols <- colnames(dbf_data)
  markdata_reference_cols <- colnames(markdata_reference_df)

  # Identify differences in column names
  missing_in_reference <- setdiff(dbf_cols, markdata_reference_cols)
  missing_in_dbf <- setdiff(markdata_reference_cols, dbf_cols)

  # Return the result
  list(
    dbf_file = dbf_file,
    missing_in_reference = missing_in_reference,
    missing_in_dbf = missing_in_dbf
  )
}

# Apply the comparison function to all filtered .dbf files with 'markdata' in the name
markdata_comparison_results <- lapply(markdata_dbf_files_eider1994.2005, compare_columns, markdata_reference_df = markdata_reference_df)
#years 1994-2000 (excluding the kigmarkdata.dbf in year 2000) include at least all of the fields specified in the reference dataframe
#years 2001-2005 are missing two fields that are included in the reference dataframe: 'NASALCODE' and 'TARSALCODE'.
##This means either this data was not collected during those years and NA will have to be included. Or that data was collected under a different name.
#Year 2000 indluded two files with 'markdata' in the name, "kigmarkdata.dbf" is missing one field from the reference table: 'SPECIESCOD'

############################

#comparing Resight .dbf files to reference files

# List all .dbf files from those folders, including only files that have 'resight' in the name
resight_dbf_files_eider1994.2005 <- unlist(lapply(eider_data_folders_1994.2005, function(folder) {
  # List .dbf files and filter for files with "resight" in the name
  dbf_files_all <- list.files(path = folder, pattern = "\\.dbf$", full.names = TRUE, ignore.case = TRUE)
  dbf_files_filtered <- dbf_files_all[grepl("resight", basename(dbf_files_all), ignore.case = TRUE)]
  return(dbf_files_filtered)
}))
# View the filtered list of .dbf files with 'resight' in the name
print(resight_dbf_files_eider1994.2005)

#Comparing columns of the resight data files to the resight reference dataframe

# Function to compare column names of a .dbf file with the reference data frame
compare_columns <- function(dbf_file, resight_reference_df) {
  # Read the .dbf file
  dbf_data <- read.dbf(dbf_file)

  # Get column names
  dbf_cols <- colnames(dbf_data)
  resight_reference_cols <- colnames(resight_reference_df)

  # Identify differences in column names
  missing_in_reference <- setdiff(dbf_cols, resight_reference_cols)
  missing_in_dbf <- setdiff(resight_reference_cols, dbf_cols)

  # Return the result
  list(
    dbf_file = dbf_file,
    missing_in_reference = missing_in_reference,
    missing_in_dbf = missing_in_dbf
  )
}

# Apply the comparison function to all filtered .dbf files with 'resight' in the name
resight_comparison_results <- lapply(resight_dbf_files_eider1994.2005, compare_columns, resight_reference_df = resight_reference_df)
#years 1994-2000 include at least all the fields specified in the reference data frame
#years 2001-2005 are missing one field that is included in the reference data frame: "TARSUS"

################################

#comparing Visit .dbf files to reference files

# List all .dbf files from those folders, including only files that have 'visit' in the name
visit_dbf_files_eider1994.2005 <- unlist(lapply(eider_data_folders_1994.2005, function(folder) {
  # List .dbf files and filter for files with "visit" in the name
  dbf_files_all <- list.files(path = folder, pattern = "\\.dbf$", full.names = TRUE, ignore.case = TRUE)
  dbf_files_filtered <- dbf_files_all[grepl("visit", basename(dbf_files_all), ignore.case = TRUE)]
  return(dbf_files_filtered)
}))
# View the filtered list of .dbf files with 'visit' in the name
print(visit_dbf_files_eider1994.2005)

#Comparing columns of the visit files to the visit reference dataframe

# Function to compare column names of a .dbf file with the reference data frame
compare_columns <- function(dbf_file, visit_reference_df) {
  # Read the .dbf file
  dbf_data <- read.dbf(dbf_file)

  # Get column names
  dbf_cols <- colnames(dbf_data)
  visit_reference_cols <- colnames(visit_reference_df)

  # Identify differences in column names
  missing_in_reference <- setdiff(dbf_cols, visit_reference_cols)
  missing_in_dbf <- setdiff(visit_reference_cols, dbf_cols)

  # Return the result
  list(
    dbf_file = dbf_file,
    missing_in_reference = missing_in_reference,
    missing_in_dbf = missing_in_dbf
  )
}

# Apply the comparison function to all filtered .dbf files with 'visit' in the name
visit_comparison_results <- lapply(visit_dbf_files_eider1994.2005, compare_columns, visit_reference_df = visit_reference_df)
#years 1994-2005 include at least all of the fields specified in the reference data frame

###########################

#comparing Egg .dbf files to reference files

# List all .dbf files from those folders, including only files that have 'egg' in the name
egg_dbf_files_eider1994.2005 <- unlist(lapply(eider_data_folders_1994.2005, function(folder) {
  # List .dbf files and filter for files with "visit" in the name
  dbf_files_all <- list.files(path = folder, pattern = "\\.dbf$", full.names = TRUE, ignore.case = TRUE)
  dbf_files_filtered <- dbf_files_all[grepl("egg", basename(dbf_files_all), ignore.case = TRUE)]
  return(dbf_files_filtered)
}))
# View the filtered list of .dbf files with 'egg' in the name
print(egg_dbf_files_eider1994.2005)

#Comparing columns of the egg files to the egg reference dataframe

# Function to compare column names of a .dbf file with the reference data frame
compare_columns <- function(dbf_file, egg_reference_df) {
  # Read the .dbf file
  dbf_data <- read.dbf(dbf_file)

  # Get column names
  dbf_cols <- colnames(dbf_data)
  egg_reference_cols <- colnames(egg_reference_df)

  # Identify differences in column names
  missing_in_reference <- setdiff(dbf_cols, egg_reference_cols)
  missing_in_dbf <- setdiff(egg_reference_cols, dbf_cols)

  # Return the result
  list(
    dbf_file = dbf_file,
    missing_in_reference = missing_in_reference,
    missing_in_dbf = missing_in_dbf
  )
}

# Apply the comparison function to all filtered .dbf files with 'egg' in the name
egg_comparison_results <- lapply(egg_dbf_files_eider1994.2005, compare_columns, egg_reference_df = egg_reference_df)

