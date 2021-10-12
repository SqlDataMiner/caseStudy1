setwd(r"{C:\Users\spwot\OneDrive\MscStats\src\EDA\PayGapAnalysis}")
library(dplyr)

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
data_files <-list.files(path="data", full.names=TRUE, pattern="^UK Gender Pay Gap Data -(.*).csv$" )
data_tables <- lapply(data_files, read_data_augment_with_years)
merged_tables <- Reduce(function(x,y) {merge(x,y, all=TRUE)}, data_tables)

# join the university list with the pay gap data
uni <- read.csv("data/universities.csv")
uni_data <- inner_join(uni, merged_tables, by="EmployerName")

##STARTING TO LOOK AT THE DATA HERE considering female pay...
pay_ucl_2017 <- uni_data[which(uni_data$EmployerName == "UCL" & uni_data$start_year == 2017), ]

q1 <- pay_ucl_2017$FemaleLowerQuartile
q2 <- pay_ucl_2017$FemaleLowerMiddleQuartile
q3 <- pay_ucl_2017$FemaleUpperMiddleQuartile
q4 <- pay_ucl_2017$FemaleTopQuartile
# note their numbering of quarters is different to what I expect here
# we might need to verify that the order of data is uniform in all records
# i.e. in this case q1 >= q2 >= q3 >=q4 is that true for all of our records?
iqr <- q2 - q3

#' So in the data we have: Q1, Q2, Q3, and Q4.
#' Where the iterquartile range (IQR) = Q3 - Q2
#' What we are missing is the min and the mean to create
#' the box plot.  Let us suppose that the min was:
#' min = Q2 - 1.5IQR
#' This is the issue we have to guess the min
#' and we don't have the mean for the box plot either
min <- q2 - 1.5*iqr

#make some random min as iqr is zero in this case!
# comment this statement out to see what happens to box plot!
if (min > q4) min <- q4 - 5

#' create vector for boxplot including min but this is a
#' complete guess we have no basis for this.
#' However I wasn't sure if you were asking how to programatically
#' do this from raw data.  If so hope it helps.  If you already know
#' this then please take this in the spirit of me just trying to help.
box_plot_vector <- c(min, q1, q2, q3, q4)
#' turns out this data is rather odd:
#' q1 = 64, q2 = 53, q3=53, q4=37, therefore iqr=0 and min=53
#' a cursory inspection tells us no, so we will have to clean the data.
#' I can do that if no one else fancies the task


#' It took me a few minutes to wonder what had happened to the
#' box plot to be missing the "box and whiskers" part of the plot
#' It's because the IQR is zero hence we get a line with no whiskers!! and
#' our estimated min and q1 (largest value) hence I added the  statement:
#' if (min > q4) min <- q4 - 5
#' to get a proper box plot which if you comment it out you'll see what I mean
boxplot(box_plot_vector,
        main="Female pay at UCL in 2017"
)


