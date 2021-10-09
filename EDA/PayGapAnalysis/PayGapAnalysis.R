setwd(r"{C:\Users\spwot\OneDrive\MscStats\src\EDA\PayGapAnalysis}")
library(dplyr)

# define function for reading table and adding the start and end data to the data
read_data_augment_with_years <- function(file_to_read){
  # find the years from file name
  years_result <- gregexpr("[0-9]{4}", file_to_read)
  start_year_index = years_result[[1]][[1]]
  end_year_index = years_result[[1]][[2]]
  start_year <- substr(file_to_read, start_year_index, start_year_index + 3)
  end_year <- substr(file_to_read, end_year_index, end_year_index + 3)

  # read table and add the years to data
  table = read.csv(file_to_read)
  table["start_year"] <- start_year
  table["end_year"] <- end_year

  table
}

#read all the data and merge together
data_files =list.files(path="data", full.names=TRUE, pattern="^UK Gender Pay Gap Data -(.*).csv$" )
data_tables = lapply(data_files, read_data_augment_with_years)
merged_tables = Reduce(function(x,y) {merge(x,y, all=TRUE)}, data_tables)

# join the university list with the pay gap data
uni <- read.csv("data/universities.csv")
uni_data = inner_join(uni, merged_tables, by="EmployerName")


counts_by_uni <- aggregate(uni_data$EmployerName, by=list(uni_data$EmployerName, uni_data$institution), FUN=length)

counts_by_institution <- aggregate(counts_by_uni$Group.2, by=list(counts_by_uni$Group.2, counts_by_uni$x), FUN=length)
counts_by_institution <- counts_by_institution[order(counts_by_institution$Group.1, counts_by_institution$Group.2),]