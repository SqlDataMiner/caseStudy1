setwd(r"{C:\Users\spwot\OneDrive\MscStats\src\EDA\PayGapAnalysis}")
library(dplyr)
library(tidyverse)


# define function for reading table and adding the start and end data to the data
read_data_augment_with_years <- function(file_to_read){
  # find the years from file name
  years_result <- gregexpr("[0-9]{4}", file_to_read)
  start_year_index <- years_result[[1]][[1]]
  end_year_index <- years_result[[1]][[2]]
  start_year <- substr(file_to_read, start_year_index, start_year_index + 3)
  end_year <- substr(file_to_read, end_year_index, end_year_index + 3)

  # read table and add the years to data
  table <- read.csv(file_to_read)
  table["start_year"] <- start_year
  table["end_year"] <- end_year

  table
}

#read all the data and merge together
data_files <- list.files(path="data", full.names=TRUE, pattern="^UK Gender Pay Gap Data -(.*).csv$" )
data_tables <- lapply(data_files, read_data_augment_with_years)
merged_tables <- Reduce(function(x,y) {merge(x,y, all=TRUE)}, data_tables)

# join the university list with the pay gap data
# Note that we can't join on name as they are not consistent
# we are told so must change to join on ID
uni <- read.csv("data/universities.csv")
uni_data_find_employers <- inner_join(uni, merged_tables, by="EmployerName")
uni_employers_by_id <- uni_data_find_employers %>%
  select(EmployerId, EmployerName) %>%
  distinct()

uni_data_emp <- inner_join(uni, uni_employers_by_id, by="EmployerName")
uni_data <- inner_join(uni_data_emp, merged_tables, by="EmployerId")
# have validated that the counts are correct i.e. 96 in both lists.

#lets find how many universities have changed name.
name_changes <- uni_data %>%
                #filter(EmployerId == 1547 | EmployerId == 296) %>%
                filter(EmployerName.y != EmployerName.x) %>%
                select(EmployerId)  %>%
                distinct()

data_excluding_2021 <- filter(uni_data, start_year !=2021)

counts_by_uni <- aggregate(data_excluding_2021$EmployerId, by=list(data_excluding_2021$EmployerId, data_excluding_2021$institution), FUN=length) %>%
                  select(EmployerId=Group.1,institution=Group.2, Count=x  )

uni_with_full_result_sets <- filter(counts_by_uni, Count == 4)

unis_who_changed_names_with_full_results <- inner_join(name_changes, uni_with_full_result_sets, by="EmployerId") %>%
                                            select(EmployerId) %>%
                                            distinct()

counts_by_institution <- aggregate(uni_with_full_result_sets$institution,
                                   by=list(uni_with_full_result_sets$institution, uni_with_full_result_sets$Count),
                                   FUN=length)
