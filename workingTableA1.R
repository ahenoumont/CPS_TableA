# Recreate "Table A-1. Employment status of the civilian population by sex and age"
# Source: "https://www.bls.gov/news.release/empsit.t01.htm"
#
# This project will recreat the Bureau of Labor Statistics monthly report of the Employment data Using the Current Population Survey.
# Data was obtained from the Data Ferret Application and pulled on 31 Aug 2018
# 
# I plan on using APIs to retrieve the data tables
# The guide can be found at "https://api.census.gov/data.html"
# My key is e598c3c5061e24f346054a29807b641a355f013b
# 
# Explanation of variables:
#   HRHHID: Household Identifier. Each observation is an individual and this variable is a decribes all individuals in a given household.
#   HRHHID2: Household Identifier
#   OCCURNUM: Unique person identifier.
#   YYYYMM: Year and month observation was taken
#   PESEX: Gender
#     1 = Male
#     2 = Female
#   PRTAGE:
#     Age
#     80 = 80-84
#     85 = 85+
#   PEMLR:Employment type
#     1	EMPLOYED-AT WORK
#     2	EMPLOYED-ABSENT
#     3	UNEMPLOYED-ON LAYOFF
#     4	UNEMPLOYED-LOOKING
#     5	NOT IN LABOR FORCE-RETIRED
#     6	NOT IN LABOR FORCE-DISABLED
#     7	NOT IN LABOR FORCE-OTHER
#   PRPERTYP: Person Type
#     1	CHILD HOUSEHOLD MEMBER
#     2	ADULT CIVILIAN HOUSEHOLD MEMBER
#     3	ADULT ARMED FORCES HOUSEHOLD MEMBER
#   PRWNTJOB
#     1 YES
#     2 NO


#load libraries
library(tidyverse)

### Cature

# name paths
# pathHome <- "/Users/MacUser/Documents/Data Science/CPS_TableA"
# pathRawData <- "/Users/MacUser/Documents/Data Science/CPS_TableA/Rawdata"
# setwd(pathRawData) 
temp <-  list.files(pattern="*.csv")
cpsData = as_data_frame(lapply(temp, read_csv) %>% bind_rows())
# setwd(pathHome)
# rm(temp)

###Table A-1. Employment status of the civilian population by sex and age

#TOTAL
tableA_total <- cpsData %>% 
  group_by(YYYYMM) %>% 
  summarize(`Civilian population` = n(),
            `Civilian labor force` = sum(PRPERTYP == 2),
            Employed = sum(PEMLR == 1 | PEMLR == 2),
            Unemployed = sum(PEMLR == 3 | PEMLR == 4),
            `Not in labor force` = sum(PEMLR == 5 | PEMLR == 6),
            `Person wants job` = sum(PRWNTJOB == 1)) %>% 
  mutate(`Participation rate` = `Civilian labor force` / `Civilian population` * 100,
         `Employment-population ratio` = Employed / `Civilian population` * 100,
         `Unemployment rate` = Unemployed / `Civilian population` * 100) %>% 
  ungroup()

#MEN, 16 AND OVER
tableA_maleOver16 <- cpsData %>%
  filter(PESEX == 1, PRTAGE >= 16) %>% 
  group_by(YYYYMM) %>% 
  summarize(`Civilian population` = n(),
            `Civilian labor force` = sum(PRPERTYP == 2),
            Employed = sum(PEMLR == 1 | PEMLR == 2),
            Unemployed = sum(PEMLR == 3 | PEMLR == 4),
            `Not in labor force` = sum(PEMLR == 5 | PEMLR == 6),
            `Person wants job` = sum(PRWNTJOB == 1)) %>% 
  mutate(`Participation rate` = `Civilian labor force` / `Civilian population` * 100,
         `Employment-population ratio` = Employed / `Civilian population` * 100,
         `Unemployment rate` = Unemployed / `Civilian population` * 100) %>% 
  ungroup()

#MEN, 20 AND OVER
tableA_maleOver20 <- cpsData %>%
  filter(PESEX == 1, PRTAGE >= 20) %>% 
  group_by(YYYYMM) %>% 
  summarize(`Civilian population` = n(),
            `Civilian labor force` = sum(PRPERTYP == 2),
            Employed = sum(PEMLR == 1 | PEMLR == 2),
            Unemployed = sum(PEMLR == 3 | PEMLR == 4),
            `Not in labor force` = sum(PEMLR == 5 | PEMLR == 6),
            `Person wants job` = sum(PRWNTJOB == 1)) %>% 
  mutate(`Participation rate` = `Civilian labor force` / `Civilian population` * 100,
         `Employment-population ratio` = Employed / `Civilian population` * 100,
         `Unemployment rate` = Unemployed / `Civilian population` * 100) %>% 
  ungroup()

#WOMEN, 16 AND OVER
tableA_femaleOver16 <- cpsData %>%
  filter(PESEX == 2, PRTAGE >= 16) %>% 
  group_by(YYYYMM) %>% 
  summarize(`Civilian population` = n(),
            `Civilian labor force` = sum(PRPERTYP == 2),
            Employed = sum(PEMLR == 1 | PEMLR == 2),
            Unemployed = sum(PEMLR == 3 | PEMLR == 4),
            `Not in labor force` = sum(PEMLR == 5 | PEMLR == 6),
            `Person wants job` = sum(PRWNTJOB == 1)) %>% 
  mutate(`Participation rate` = `Civilian labor force` / `Civilian population` * 100,
         `Employment-population ratio` = Employed / `Civilian population` * 100,
         `Unemployment rate` = Unemployed / `Civilian population` * 100) %>% 
  ungroup()

#WOMEN, 20 AND OVER
tableA_femaleOver20 <- cpsData %>%
  filter(PESEX == 2, PRTAGE >= 20) %>% 
  group_by(YYYYMM) %>% 
  summarize(`Civilian population` = n(),
            `Civilian labor force` = sum(PRPERTYP == 2),
            Employed = sum(PEMLR == 1 | PEMLR == 2),
            Unemployed = sum(PEMLR == 3 | PEMLR == 4),
            `Not in labor force` = sum(PEMLR == 5 | PEMLR == 6),
            `Person wants job` = sum(PRWNTJOB == 1)) %>% 
  mutate(`Participation rate` = `Civilian labor force` / `Civilian population` * 100,
         `Employment-population ratio` = Employed / `Civilian population` * 100,
         `Unemployment rate` = Unemployed / `Civilian population` * 100) %>% 
  ungroup()

#BOTH SEXES BETWEEN 16 AND 19
tableA_femaleOver20 <- cpsData %>%
  filter(PRTAGE >= 16, PRTAGE <= 19) %>% 
  group_by(YYYYMM) %>% 
  summarize(`Civilian population` = n(),
            `Civilian labor force` = sum(PRPERTYP == 2),
            Employed = sum(PEMLR == 1 | PEMLR == 2),
            Unemployed = sum(PEMLR == 3 | PEMLR == 4),
            `Not in labor force` = sum(PEMLR == 5 | PEMLR == 6),
            `Person wants job` = sum(PRWNTJOB == 1)) %>% 
  mutate(`Participation rate` = `Civilian labor force` / `Civilian population` * 100,
         `Employment-population ratio` = Employed / `Civilian population` * 100,
         `Unemployment rate` = Unemployed / `Civilian population` * 100)# %>%
  # ungroup()
#Analyse
#Report

sum(ifelse(cpsData$PEMLR==2,cpsData$PWCMPWGT,0))
