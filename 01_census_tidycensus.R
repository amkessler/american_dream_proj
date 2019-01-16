library(tidyverse)
library(tigris)
library(tidycensus)
library(reshape2)
library(janitor)
library(datapasta)
options(tigris_class = "sf")

# census_api_key("XXXXXXXXX", install=TRUE)


#### SET CENSUS VARIABLES TO BE PULLED ####

myvars <- c(totalpop = "B01003_001",
            medincome = "B19013_001",
            medage = "B01002_001",
            education.total = "B06009_001",
            education.bachelors = "B06009_005",  
            education.gradprofess = "B06009_006",
            mortgage.total = "B25091_001",
            mortgage.40to49pct = "B25091_010",
            mortgage.50pctplus = "B25091_011",
            healthins.male.18to24.total = "B27001_009",
            healthins.male.18to24.coverage = "B27001_010",
            healthins.male.18to24.nocoverage = "B27001_011",
            healthins.female.18to24.total = "B27001_037",
            healthins.female.18to24.coverage = "B27001_038",
            healthins.female.18to24.nocoverage = "B27001_039",
            healthins.male.25to34.total = "B27001_012",
            healthins.male.25to34.coverage = "B27001_013",
            healthins.male.25to34.nocoverage = "B27001_014",
            healthins.female.25to34.total = "B27001_040",
            healthins.female.25to34.coverage = "B27001_041",
            healthins.female.25to34.nocoverage = "B27001_042"
            )


#health insurances tables:
# https://www.socialexplorer.com/data/ACS2012_5yr/metadata/?ds=ACS12_5yr&table=B27001


# # #temp labels to pull individual census variables
# myvars <- c(varname = "B25091")
# 


#############################################################

# SET VARIABLE VECTOR AS CURRENT CHOICE TO USE BELOW
currentchoice <- myvars


#single state ####
vt <- get_acs(geography = "county",
              variables = c(currentchoice),
              state = "VT")


#Getting data for all states in the US ####

head(fips_codes) #built into tidycensus
us <- unique(fips_codes$state)[1:51]

#wide format to allow for percentage calculations
alldistricts_wide <- map_df(us, function(x) {
  get_acs(geography = "county", variables = c(currentchoice), 
          state = x, output = "wide")
})

saveRDS(alldistricts_wide, "countydatatable.rds")
