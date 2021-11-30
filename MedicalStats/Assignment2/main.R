library(dplyr)
library(tidyverse)

# Read file in as CSV, note converted RData to CSV so can use IntelliJ - an IDE I prefer.
csvFile <- "data/cirrhosis.csv"
table <- read.csv(csvFile)

group1 <- table %>%
          filter(Group == 1)

group2 <- table %>%
  filter(Group == 2)

setClass("summary", slots=list(n="numeric", mean="numeric", sd="numeric", group="numeric"))

tTestSummary <- function(test, summation) {
  writeLines(str_glue("A T-test performed on group 1 and 2 for: {summation}"))
  writeLines(str_glue("The test statistic was: {format(round(test$statistic, 3), nsmall=3)}"))
  writeLines(str_glue("p={format(round(test$p.value, 3), nsmall=3)}"))
  if(test$p.value <=0.05){
    writeLines("The result was significant at the 95% level.")
  }else {
    writeLines("The result was not significant at the 95% level.")
  }
  writeLines(str_glue("The confidence interval at 95% was: ({format(round(test$conf.int[1], 3), nsmall=3)}, {format(round(test$conf.int[2], 3), nsmall=3)})."))
  writeLines("\n\n")
}


## Note that the excercise data is presented differs to that of the file.
## PeriodDiffs in the file correspond to Diff1-(-2) in the exercise and
## Conversely TreatDiffs correspond to Diff1-2.
# Validated this in R studio to rule out data conversion issue.
# Am assuming the file has incorrect headers and proceeding on that basis.
carryOverTTest <- t.test(group1$Sum1.2, group2$Sum1.2)
tTestSummary(carryOverTTest, "Sum 1 + 2, to validate carryover effects")

treatmentTTest <- t.test(group1$PeriodDiffs, group2$PeriodDiffs)
tTestSummary(treatmentTTest, "Diff1-2, to validate treatment effects")

periodTTest <- t.test(group1$TreatDiffs, group2$TreatDiffs)
tTestSummary(periodTTest, "Diff1-(-2), to validate period effects")
