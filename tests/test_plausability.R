context("Plausability")

library(testthat)
library(ChurnProb)

#Set working directory
setwd("C:\\Users\\Remo_\\OneDrive\\Dokumente\\ChurnProb\\tests")

library(ChurnProb)

#Read in the Data
d_customer <- fread("data_customer.csv")
d_personal <- fread("data_personal.csv")
#Merge the Data
final_data <- merge(d_customer, d_personal, by="CustomerId",all=TRUE)

#Set columns as factors
final_data[, c("Exited", "Gender")] <- lapply(final_data[, c("Exited", "Gender")], as.factor)

test_that("Highest vs. lowest check",{
  expect_equal(ChurnProb(final_data, 15653251)$Churn_Prob, ChurnProb(final_data, 15653251)$Churn_Prob)
  expect_equal(ChurnProb(final_data, 15662641)$Churn_Prob, ChurnProb(final_data, 15662641)$Churn_Prob)
})
