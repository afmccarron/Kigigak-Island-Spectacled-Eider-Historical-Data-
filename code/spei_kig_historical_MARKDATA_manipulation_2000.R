install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos = 'http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c("workflowr", "tidyverse", "foreign", "dplyr", "here", "tools", "stringr", "lubridate")
sapply(package_vec, install.load.package)

##################################################################################
#Manipulating the duplicate markdata files in the Eider2000 folder.

#Reading the .dbf files in Eider2000 that have "markdown" in the name
markdata2000 <- read.dbf("data/Eider2000/Markdata.dbf")
kigmarkdata2000 <- read.dbf("data/Eider2000/kigmarkdata.dbf")

#Changing the date from "1900" to "2000"
kigmarkdata2000_updated <- kigmarkdata2000 %>%
  mutate(DATE = update(DATE, year = 2000))
#repeating the process for markdata2000
markdata2000_updated <- markdata2000 %>%
  mutate(DATE = update(DATE, year = 2000))

#Creating a unique identifying number for each row by combining band number and date
#This is because some individual birds were captured twice in one season and there will therefore be repeating band numbers

#Combining "PREFIXNUMB" and "BANDNUMBER" into the "BANDNU" column to create a column that includes the entire band number
kigmarkdata2000_updated$BANDNU <- paste0(kigmarkdata2000$PREFIXNUMB, kigmarkdata2000$BANDNUMBER)
#repeat process for markdata2000
markdata2000_updated$BANDNU <- paste0(markdata2000$PREFIXNUMB, markdata2000$BANDNUMBER)

# Combine BANDNU and DATE into a unique ID column, labeled "MARK_ID"
kigmarkdata2000_updated$MARK_ID <- paste(kigmarkdata2000_updated$BANDNU, kigmarkdata2000_updated$DATE, sep = "_")
#repeat process for markdata2000
markdata2000_updated$MARK_ID <- paste(markdata2000_updated$BANDNU, markdata2000_updated$DATE, sep = "_")

#Changing the name of column "SPECIESCOD" to "SPECIES" to be consistent
markdata2000_updated <- markdata2000_updated %>% rename(SPECIES = SPECIESCOD)

#comparing column names between the two dataframes

setdiff(colnames(kigmarkdata2000_updated), colnames(markdata2000_updated))
setdiff(colnames(markdata2000_updated), colnames(kigmarkdata2000_updated))
intersect(colnames(kigmarkdata2000_updated), colnames(markdata2000_updated))

merged_markdata2000 <- bind_rows(kigmarkdata2000_updated, markdata2000_updated)



