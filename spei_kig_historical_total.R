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
package_vec <- c("workflowr", "tidyverse", "foreign", "dplyr", "here", "tools", "stringr")
sapply(package_vec, install.load.package)


################################

#Combining header data from 1994-2005 and 2006-2015

# Convert the PHOTO column to character in both data frames
header_combined_data1994.2005$PHOTO <- as.character(header_combined_data1994.2005$PHOTO)
header_combined_data2006.2015$PHOTO <- as.character(header_combined_data2006.2015$PHOTO)

# Now combine the data frames
combined_header_data_total <- bind_rows(header_combined_data1994.2005, header_combined_data2006.2015)


##################################

#Combining markdata data from 1994-2005 and 2006-2015

combined_markdata_total <- bind_rows(markdata_combined_data1994.2005, markdata_combined_data2006.2015)


###################################

#combining resight data from 1994-2005 and 2006-2015

combined_resight_data_total <- bind_rows(resight_combined_data1994.2005, resight_combined_data2006.2015)

####################################

#Combining visit data from 1994-2005 and 2006-2015

# Convert the PHOTO column to character in both data frames
visit_combined_data1994.2005$PHOTO <- as.character(visit_combined_data1994.2005$PHOTO)
visit_combined_data2006.2015$PHOTO <- as.character(visit_combined_data2006.2015$PHOTO)

combined_visit_data_total <- bind_rows(visit_combined_data1994.2005, visit_combined_data2006.2015)
