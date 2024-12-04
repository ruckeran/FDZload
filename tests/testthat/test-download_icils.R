test_that("2013: stud_nat_dat was successfully downloaded", {
  fdz_icils <- download_icils(year = "2013", data_type = "stud_nat_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_icils, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_icils$dat), 0)  # expecting more than 0 columns
})

test_that("2013: stud_int_dat was successfully downloaded", {
  fdz_icils <- download_icils(year = "2013", data_type = "stud_int_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_icils, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_icils$dat), 0)  # expecting more than 0 columns
})

test_that("2013: teach_nat_dat was successfully downloaded", {
  fdz_icils <- download_icils(year = "2013", data_type = "teach_nat_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_icils, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_icils$dat), 0)  # expecting more than 0 columns
})

test_that("2013: teach_int_dat was successfully downloaded", {
  fdz_icils <- download_icils(year = "2013", data_type = "teach_int_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_icils, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_icils$dat), 0)  # expecting more than 0 columns
})

test_that("2013: school_nat_dat was successfully downloaded", {
  fdz_icils <- download_icils(year = "2013", data_type = "school_nat_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_icils, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_icils$dat), 0)  # expecting more than 0 columns
})

test_that("2013: school_int_dat was successfully downloaded", {
  fdz_icils <- download_icils(year = "2013", data_type = "school_int_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_icils, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_icils$dat), 0)  # expecting more than 0 columns
})

test_that("2018: stud_dat was successfully downloaded", {
  fdz_icils <- download_icils(year = "2018", data_type = "stud_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_icils, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_icils$dat), 0)  # expecting more than 0 columns
})

test_that("2018: teach_dat was successfully downloaded", {
  fdz_icils <- download_icils(year = "2018", data_type = "teach_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_icils, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_icils$dat), 0)  # expecting more than 0 columns
})

test_that("2018: school_dat was successfully downloaded", {
  fdz_icils <- download_icils(year = "2018", data_type = "school_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_icils, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_icils$dat), 0)  # expecting more than 0 columns
})

test_that("2018: it_dat was successfully downloaded", {
  fdz_icils <- download_icils(year = "2018", data_type = "it_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_icils, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_icils$dat), 0)  # expecting more than 0 columns
})
