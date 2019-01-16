library(tidyverse)
library(sf)
library(janitor)
library(datapasta)
library(skimr)
library(summarytools)
library(Hmisc)



#load saved data file from Step 02
alldistricts_FINAL <- readRDS("alldistricts_FINAL.rds")

#PA?
alldistricts_FINAL %>% 
  filter(state.name == "Pennsylvania") %>% 
  select(pct.mil.veteran, medincome)

data <- alldistricts_FINAL
names(data)

data <- data %>% 
  mutate(keyrace_rating_wcurparty = paste0(keyrace_rating,"__",current_party))

data <- data %>% 
  mutate(hillary_romney = case_when((p16winningparty=="D" & p12winningparty=="R") ~ "YES"),
         trump_obama = case_when((p16winningparty=="R" & p12winningparty=="D") ~ "YES")
         )


#### create table with just key races #####
keyraceonly <- data %>%
  filter(!is.na(keyrace_rating))

names(keyraceonly)

for_ron <- keyraceonly %>% 
  select(
    GEOID, 
    state.name, 
    house_dist, 
    keyrace_rating, 
    current_party, 
    medincome.abovebelow.natl, 
    medage.abovebelow.natl, 
    pct.mil.veteran.abovebelow.natl,
    pct.race.nonwhite.abovebelow.natl, 
    pct.ed.college.all.abovebelow.natl, 
    pct.ed.college.white.abovebelow.natl,
    pct.ed.college.white.male.abovebelow.natl, 
    pct.ed.college.white.female.abovebelow.natl, 
    pct.rural.above20,
    p16winningparty, 
    p12winningparty, 
    hillary_romney, 
    trump_obama,
    pct.born.foreign.abovebelow.natl,
    gdp_abovebelow_natlavg)

write.csv(for_ron, "for_ron2.csv", row.names = FALSE)


for_ron_allcds <- data %>% 
  select(GEOID,
  state.name,
  house_dist,
  keyrace_rating,
  current_party,
  totalpop,
  medincome,
  medage,
  pct.race.nonwhite,
  pct.ed.college.all,
  natl.medincome,
  natl.medage,
  natl.pct.race.nonwhite,
  natl.pct.ed.college.all,
  medincome.abovebelow.natl,
  medage.abovebelow.natl,
  pct.race.nonwhite.abovebelow.natl,
  pct.ed.college.all.abovebelow.natl,
  pct.ed.college.white.abovebelow.natl,
  pct.ed.college.white.male.abovebelow.natl, 
  pct.ed.college.white.female.abovebelow.natl,
  p16winningparty,
  p12winningparty,
  pct.rural,
  pct.rural.above20,
  keyrace_rating_wcurparty,
  hillary_romney,
  trump_obama,
  pct.born.foreign.abovebelow.natl,
  gdp_abovebelow_natlavg)


write.csv(for_ron_allcds, "for_ron_allcds2.csv", row.names = FALSE)

#summaries
summary(data)

skim(data) 

descr(data)
# 
# view(dfSummary(data))
# #save output
# view(dfSummary(data), file = "data_summarystats.html")

freq(data$keyrace_rating, style = "rmarkdown")



t <- with(keyraceonly, print(ctable(keyrace_rating, medincome.abovebelow.natl), method = 'render'))
htmltools::save_html(t, "t.html")

t2 <- with(keyraceonly, print(ctable(keyrace_rating_wcurparty, medincome.abovebelow.natl), method = 'render'))
htmltools::save_html(t2, "t2.html")


t <- with(keyraceonly, print(ctable(keyrace_rating_wcurparty, pct.ed.college.all.abovebelow.natl), method = 'render'))
htmltools::save_html(t, "summary_college.html")

t <- with(keyraceonly, print(ctable(keyrace_rating_wcurparty, trump_obama), method = 'render'))
htmltools::save_html(t, "summary_temp.html")


names(data)





names(data)


# Frequency counts using janitor's tabyl
# https://cran.r-project.org/web/packages/janitor/readme/README.html

keyraceonly %>% 
  tabyl(keyrace_rating, medincome.abovebelow.natl)

mylist <- c("medincome.abovebelow.natl", "medage.abovebelow.natl")


keyraceonly %>% 
  tabyl(keyrace_rating, medincome.abovebelow.natl) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title("combined")



keyraceonly %>% 
  tabyl(keyrace_rating, medincome.abovebelow.natl, current_party) %>%
  adorn_totals("row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title("combined") 



## with hmisc package
# https://dabblingwithdata.wordpress.com/2018/01/02/my-favourite-r-package-for-summarising-data/

describe(keyraceonly)



# t <- with(data, print(ctable(paste0(current_party,keyrace_rating), medincome.abovebelow.natl), method = 'render'))




#### key race ratings ####

#top level count
data %>% 
  count(keyrace_rating)


data %>% 
  filter(current_party == "R",
         keyrace_rating %in% c("likely democratic", "lean democratic")) %>% 
  select(house_dist, trump_percent, clinton_percent, p16winningparty) %>% 
  mutate(margin = abs(trump_percent - clinton_percent)) %>% 
  arrange(p16winningparty, trump_percent)



data %>% 
  filter(current_party == "R",
         keyrace_rating == "lean republican") %>% 
  count(medincome.abovebelow.natl)


rank_college_nums <- rank(-alldistricts_FINAL$pct.ed.college.all)
alldistricts_FINAL$rank_college <- rank_college_nums

aaa <- alldistricts_FINAL %>% 
  filter(rank_college >75,
         rank_college <360,
         !is.na(keyrace_rating)) 

aaa %>% 
  count(pct.ed.college.all.abovebelow.natl)


a <- alldistricts_FINAL %>% 
  filter(rank_college <=75,
         !is.na(keyrace_rating)) 

aa <- alldistricts_FINAL %>% 
  filter(rank_college >360,
         !is.na(keyrace_rating)) 


write.csv(alldistricts_FINAL, "alldistricts_FINAL.csv", row.names = FALSE)
