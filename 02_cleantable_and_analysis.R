library(tidyverse)
library(tigris)
library(tidycensus)
library(reshape2)
library(janitor)
library(datapasta)
options(tigris_class = "sf")

#load data file from step 01
alldistricts_wide <- readRDS("countydatatable.rds")


#remove MOE columns - they all end with "M"
alldistricts_wide <- alldistricts_wide %>% 
  select(-ends_with("M"))

names(alldistricts_wide)

#cleaning up and splitting NAME
split <- str_split(alldistricts_wide$NAME, ",", simplify = TRUE) #true returns matrix

dist <- split[ ,1]
dist <- str_trim(dist)

st <- split[ ,2]
st <- str_trim(st)

alldistricts_wide$county.name <- dist
alldistricts_wide$state.name <- st

#bring new columns forward
alldistricts_wide <- alldistricts_wide %>% 
  select(GEOID, state.name, county.name, everything(), -NAME)

names(alldistricts_wide)


#clean up names to remove trailing E
colnames(alldistricts_wide) <- sub("E$", "", colnames(alldistricts_wide)) # $ means end of string only

names(alldistricts_wide)



#percentage calculations ####

# tricky, since demo groups differ in columns
# so doing individually for each demographic grouping

#in the B census table bachelors and grad students must be summed for total of bachelor's or higher
alldistricts_wide$pct.ed.collegegrad <- round_half_up((alldistricts_wide$education.bachelors+alldistricts_wide$education.gradprofess)/alldistricts_wide$education.total*100, 3)

alldistricts_wide$pct.mortgage40plus <- round_half_up((alldistricts_wide$mortgage.40to49pct+alldistricts_wide$mortgage.50pctplus)/alldistricts_wide$mortgage.total*100, 3)
alldistricts_wide$pct.mortgage50plus <- round_half_up(alldistricts_wide$mortgage.50pctplus/alldistricts_wide$mortgage.total*100, 3)


### ANALYZING THE VARIABLES ####

# mortgages .................

mortgages <- alldistricts_wide %>% 
  select(GEOID, state.name, county.name, mortgage.total, mortgage.40to49pct, mortgage.50pctplus,
         pct.mortgage40plus, pct.mortgage50plus) 

mortgages %>% 
  filter(mortgage.total > 1000) %>% 
  arrange(desc(pct.mortgage50plus)) %>% 
  head(100) %>% 
  View()
  




#remove unneeded columns
# alldistricts_wide <- alldistricts_wide %>% 
#   select(-natborn.total
# )




### NATIONAL-LEVEL FIGURES FOR COMPARISON ####

#whole country figures ####
national <- get_acs(geography = "us",
                    variables = c(currentchoice),
                    output = "wide")


#remove MOE columns - they all end with "M"
national <- national %>% 
  select(-ends_with("M"))

names(national)

#clean up names to remove trailing E
colnames(national) <- sub("E$", "", colnames(national)) # $ means end of string only


# national level percentage calculations ####

names(national)

national$pct.born.foreign <- round_half_up(national$natborn.foreign/national$natborn.total*100, 3)

national$pct.mil.veteran <- round_half_up(national$military.veteran/national$military.total*100, 3)

national$pct.race.white <- round_half_up(national$originrace.whitealone/national$originrace.total.all*100, 3)
national$pct.race.nonwhite <- 100-national$pct.race.white 
national$pct.race.white <- NULL

national$pct.ed.college.all <- round_half_up((national$education.bachelors+national$education.gradprofess)/national$education.total*100, 3)

national$pct.ed.college.white <- round_half_up((national$white.ed.male.bachelors+national$white.ed.female.bachelors)/national$white.ed.totalall*100, 3)
national$pct.ed.college.white.male <- round_half_up(national$white.ed.male.bachelors/national$white.ed.male.total*100, 3)
national$pct.ed.college.white.female <- round_half_up(national$white.ed.female.bachelors/national$white.ed.female.total*100, 3)


#remove unneeded columns
national <- national %>% 
  select(-natborn.total
  )

colnames(national)

colnames(national) <- c("GEOID",
                      "natl.name",
                      "natl.totalpop",
                      "natl.medincome",
                      "natl.medage",
                      "natl.pct.born.foreign",
                      "natl.pct.mil.veteran",
                      "natl.pct.race.nonwhite",
                      "natl.pct.ed.college.all",         
                      "natl.pct.ed.college.white",
                      "natl.pct.ed.college.white.male",
                      "natl.pct.ed.college.white.female"
                      )


#### ADD NATIONAL NUMBERS AS COLUMNS TO DISTRICT DATA #####

# single vector value will repeat itself in all rows
alldistricts_wide$natl.medincome <- national$natl.medincome
alldistricts_wide$natl.medage <- national$natl.medage
alldistricts_wide$natl.pct.born.foreign <- national$natl.pct.born.foreign
alldistricts_wide$natl.pct.mil.veteran <- national$natl.pct.mil.veteran
alldistricts_wide$natl.pct.race.nonwhite <- national$natl.pct.race.nonwhite
alldistricts_wide$natl.pct.ed.college.all <- national$natl.pct.ed.college.all
alldistricts_wide$natl.pct.ed.college.white <- national$natl.pct.ed.college.white
alldistricts_wide$natl.pct.ed.college.white.male <- national$natl.pct.ed.college.white.male
alldistricts_wide$natl.pct.ed.college.white.female <- national$natl.pct.ed.college.white.female


# calculate whether district figure is above of below national figure ####
alldistricts_wide$medincome.abovebelow.natl <- if_else((alldistricts_wide$medincome < alldistricts_wide$natl.medincome), 'BELOW', 'ABOVE')
alldistricts_wide$medage.abovebelow.natl <- if_else((alldistricts_wide$medage < alldistricts_wide$natl.medage), 'BELOW', 'ABOVE')
alldistricts_wide$pct.born.foreign.abovebelow.natl <- if_else((alldistricts_wide$pct.born.foreign < alldistricts_wide$natl.pct.born.foreign), 'BELOW', 'ABOVE')
alldistricts_wide$pct.mil.veteran.abovebelow.natl <- if_else((alldistricts_wide$pct.mil.veteran < alldistricts_wide$natl.pct.mil.veteran), 'BELOW', 'ABOVE')
alldistricts_wide$pct.race.nonwhite.abovebelow.natl <- if_else((alldistricts_wide$pct.race.nonwhite < alldistricts_wide$natl.pct.race.nonwhite), 'BELOW', 'ABOVE')
alldistricts_wide$pct.ed.college.all.abovebelow.natl <- if_else((alldistricts_wide$pct.ed.college.all < alldistricts_wide$natl.pct.ed.college.all), 'BELOW', 'ABOVE')
alldistricts_wide$pct.ed.college.white.abovebelow.natl <- if_else((alldistricts_wide$pct.ed.college.white < alldistricts_wide$natl.pct.ed.college.white), 'BELOW', 'ABOVE')
alldistricts_wide$pct.ed.college.white.male.abovebelow.natl <- if_else((alldistricts_wide$pct.ed.college.white.male < alldistricts_wide$natl.pct.ed.college.white.male), 'BELOW', 'ABOVE')
alldistricts_wide$pct.ed.college.white.female.abovebelow.natl <- if_else((alldistricts_wide$pct.ed.college.white.female < alldistricts_wide$natl.pct.ed.college.white.female), 'BELOW', 'ABOVE')


# save result ####
saveRDS(alldistricts_wide, "alldistricts_wide.rds")
write.csv(alldistricts_wide, "alldistricts_wide.csv")


