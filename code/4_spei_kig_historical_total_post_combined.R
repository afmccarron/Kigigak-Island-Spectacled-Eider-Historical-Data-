#'###########################################################################################
#'###########################################################################################
#' Project: [Kigigak Island Spectacled Eider Historical Data Standardization and Compilation]
#' Contents: [Reducing unusable data within combined data tables]
#'  -
#'
#'  Author: [Ali McCarron]
#'
#'######################################################################################
#'


install.load.package <- function(x) {

  if (!require(x, character.only = TRUE)) {

    install.packages(x, repos = 'http://cran.us.r-project.org')
  }

  require(x, character.only = TRUE)
}

package_vec <- c("workflowr", "tidyverse", "foreign", "dplyr", "here", "tools", "stringr","readxl", "purrr", "lubridate", "sp", "sf", "leaflet")
sapply(package_vec, install.load.package)

#####################################################

#Creating a new data frame from combined_header_data_total removing less relevant columns and maintaining their relationship to header_ID

#selecting columns to add to the new dataframe
associated_spp_header_combined <- combined_header_data_total %>%
  select(HEADER_ID, NEST_NO, VEG_HT, VEG_DENS1, VEG_DENS2, DOM_SPEC1,
         SPEC_COV1, DOM_SPEC2, SPEC_COV2, DOM_SPEC3, SPEC_COV3,
         DOM_SPEC4, SPEC_COV4, GOOSE_10M, GOOSE1_SP, GOOSE1_E,
         GOOSE1_N, GOOSE2_E, GOOSE2_N, GOOSE2_SP, GOOSE3_E, GOOSE3_N,
         GOOSE3_SP, GOOSE4_E, GOOSE4_N, GOOSE4_SP, GOOSE5_E, GOOSE5_N,
         GOOSE5_SP, GOOSE6_E, GOOSE6_N, GOOSE6_SP, GOOSE7_E, GOOSE7_N,
         GOOSE7_SP, GOOSE8_E, GOOSE8_N, GOOSE8_SP, GOOSE_COMM, GULL_10M,
         GULL1_E, GULL1_N, GULL2_E, GULL2_N, GULL3_E, GULL3_N, GULL4_E,
         GULL4_N, GULL5_E, GULL5_N, GULL6_E, GULL6_N, GULL7_E, GULL7_N,
         GULL8_E, GULL8_N, GULL_COMM    )
#removing columns from combined_header_data_total
##creating a vector specifying columns to remove
columns_to_remove <- c("VEG_HT", "VEG_DENS1", "VEG_DENS2", "DOM_SPEC1",
                       "SPEC_COV1", "DOM_SPEC2", "SPEC_COV2", "DOM_SPEC3", "SPEC_COV3",
                       "DOM_SPEC4", "SPEC_COV4", "GOOSE_10M", "GOOSE1_SP", "GOOSE1_E",
                       "GOOSE1_N", "GOOSE2_E", "GOOSE2_N", "GOOSE2_SP", "GOOSE3_E", "GOOSE3_N",
                       "GOOSE3_SP", "GOOSE4_E", "GOOSE4_N", "GOOSE4_SP", "GOOSE5_E", "GOOSE5_N",
                       "GOOSE5_SP", "GOOSE6_E", "GOOSE6_N", "GOOSE6_SP", "GOOSE7_E", "GOOSE7_N",
                       "GOOSE7_SP", "GOOSE8_E", "GOOSE8_N", "GOOSE8_SP", "GOOSE_COMM", "GULL_10M",
                       "GULL1_E", "GULL1_N", "GULL2_E", "GULL2_N", "GULL3_E", "GULL3_N", "GULL4_E",
                       "GULL4_N", "GULL5_E", "GULL5_N", "GULL6_E", "GULL6_N", "GULL7_E", "GULL7_N",
                       "GULL8_E", "GULL8_N","GULL_COMM")

combined_header_data_reduced <- combined_header_data_total %>%
  select(-all_of(columns_to_remove))


