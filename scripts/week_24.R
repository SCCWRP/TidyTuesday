# # Tidy Tuesday: Data Cleaning and Wrangling Continued
# Name: Annie Holt
# Date: 5/11/21

# in the past we have covered ways to tidy and wrangle  single datasets
# tidy data: each variable is in a column, each observation is in a row, each type of observational unit is in a table
# using functions to create and filter variables, and using pipes to connect our manipulation steps 

# now introduce some ways we can deal with multiple datasets, and also introduce some more advanced cleaning methods
# first going to introduce ways we can join/merge datasets with common variables
# then go over ways we can deal with messy strings and dates


#### load packages ####

# packages from tidyverse
library(tidyverse) #dplyr, stringr, tidyr
# recommended package for dealing with dates/times
library(lubridate)

#### joins using dplyr ####

# create examples datasets 
# note that they all relate by ID
data1 <- data.frame(ID = 1:2, variable1 = c("a1", "a2"), stringsAsFactors = FALSE)

data2 <- data.frame(ID = 2:3, variable2 = c("b1", "b2"),stringsAsFactors = FALSE)

# relates to data2 by ID and variable2
data3 <- data.frame(ID = c(2,4), variable2 = c("c1", "c2"), variable3 = c("d1", "d2"),stringsAsFactors = FALSE)


# various ways we can merge data1 and data1
# general syntax is join(firstdata, seconddata, by = columntojoinon)

# include all rows in data1, and all columns from data1 and data2
# NAs for rows in data1 with no match to data2
# if multiple matches, all combinations returned
join <- data1 %>% 
  left_join(data2, by = "ID")


# opposite: include all rows in data2, and all columns from data1 and data2
join <- data1 %>% 
  right_join(data2, by = "ID")


# merge the columns of both data frames, but retain only rows with a shared ID (i.e. ID No. 2)
join <- data1 %>% 
  inner_join(data2, by = "ID")

# retain all rows and variables from both datasets
# retains most data of all join functions
# NAs for non matches
# full outer join
join <- data1 %>% 
  full_join(data2, by = "ID")

# filtering join:
# retain rows in data1 that don't have a match in data2, keep only columns from data1
join <- data1 %>% 
  anti_join(data2, by = "ID")



# you can also join multiple datasets
join <- data1 %>% 
  full_join(data2, by = "ID") %>% 
  full_join(data3, by = "ID")

# or can join by multiple shared columns
join <- data2 %>% 
  full_join(data3, by = c("ID", "variable2"))

# these joins are useful for combining datasets, but they can also be useful for data inventorying
# for example, if you want to figure out which station you have chemistry data for, but not biological data for, etc.

# CEDEN data example
# you have chemistry dataset, and separate station lookup dataset that houses station information like lat/long
# say you want to map you chemistry data, so need to get lat long associated with chemistry data
ceden_chem <- data.frame(stationcode = c("X", "Y"), analytename = "total nitrogen", result = c(2,3))
ceden_station <- data.frame(stationid = c("X", "Y"), stationname = c("stream a", "stream b"), latitude = c(34.2, 34.1), longitude = c(-118.7, -188.6))

chem_station <- ceden_chem %>% 
  left_join(ceden_station, by = c("stationcode" = "stationid"))

# or say you want to know which stations have chemistry data, but no habitat data
ceden_hab <- data.frame(stationcode = "X", metric = "riparian cover", result = 0.2)

chem_nohab <- ceden_chem %>% 
  anti_join(ceden_hab, by = "stationcode")




#### string manipulation using stringr ####

# create messy string
fruit <- c(" banana ", "fuji apple", "granny smith apple", "tangerine15")

# str_to_upper: make strings all uppercase
# str_to_lower
str_to_upper(fruit)

# str_detect: detect a particular string within a value
str_detect(fruit, "apple")

# str_trim: remove all white spaces at beginning and end of a string
str_trim(fruit)

# str_replace_all: replace characters in a string
str_replace_all(fruit,"[0-9]","")

# str_sub: extract part of a string by position 
str_sub(fruit, end = 1)

# can use these functions in dataframe manipulation, in conjunction with other tidy functions
fruit_df <- data.frame(fruit = c("apple", "apple", "orange"), type = c("fuji", "granny smith", "blood"))

fruit_filter <- fruit_df %>% 
  mutate(id = str_sub(fruit, end = 1)) %>% 
  filter(str_detect(fruit, "apple"))
  

# another example of data cleaning and wrangling
# goal: clean chemistry dataset, join with station data

# messy chemistry dataset, with common issues
# why messy? not ideal date format, sampleid column houses two variables, including desired replicate information,
# analyte names contain spaces and ar enot conformed
df_chem <- data.frame(stationcode = c("A", "A", "B", "B"), sampledate = "5/12/21", 
                      sampleid = c("A_1", "A_1", "B_2", "B_2"),
                      analyte = c(" Total Nitrogen", "Total Phosphorus ", "Nitrogen, Total", "Phosphorus as P, Total"),
                   result = 1:4)

# associated station information 
df_station <- data.frame(stationid = c("B", "C"), landuse = c("Ag", "Urban"), sitestatus = "not reference" )



chem_clean <- df_chem %>% 
  # equivalent: mutate(sampledate_good = as.Date(sampledate, "%m/%d/%y")
  mutate(sampledate_good = mdy(sampledate), #using lubridate to reformat dates
         #extract part of a string by position 
         replicate = str_sub(sampleid, start = -1),
         # removes white spaces at beginning and end of a string
         analyte_good = str_trim(analyte)) %>% 
  mutate(analyte_better = str_to_lower(analyte_good), #all lowercase
         # conform analytenames using case_when and str_detect
         analyte_best = case_when(str_detect(analyte_better, "nitrogen") ~ "tn",
                                  str_detect(analyte_better, "phosphorus") ~ "tp")) %>% 
  # retain cleaned columns
  select(stationcode, sampledate_good, replicate, analyte_best, result)
# note: I made intermediate variables for example purposes, can instead overwrite origin column names like: mutate(sampledate = myd(sampledate))


# other common lubridate functions:
# year(), month(),day()
# myd(inputdate)
# ymd(inputdate)



# want to merge chem and station information
# can use dplyr joins

# retain chem rows, chem and station columns
join <- df_chem %>% 
  left_join(df_station,
            by = c("stationcode" = "stationid"))
# retain station rows, chem and station columns
join <- df_chem %>% 
  right_join(df_station, by = c("stationcode" = "stationid"))

# retain stations that have both nutrient data and station information
join <- df_chem %>% 
  inner_join(df_station, by = c("stationcode" = "stationid"))


