test_that("2007: stud_par_dat was successfully downloaded", {
  fdz_timss <- download_timss(year = "2007", data_type = "stud_par_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_timss, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_timss$dat), 0)  # expecting more than 0 columns
})

test_that("2007: teach_dat was successfully downloaded", {
  fdz_timss <- download_timss(year = "2007", data_type = "teach_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_timss, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_timss$dat), 0)  # expecting more than 0 columns
})

test_that("2007: school_dat was successfully downloaded", {
  fdz_timss <- download_timss(year = "2007", data_type = "school_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_timss, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_timss$dat), 0)  # expecting more than 0 columns
})

test_that("2011: stud_par_dat was successfully downloaded", {
  fdz_timss <- download_timss(year = "2011", data_type = "stud_par_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_timss, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_timss$dat), 0)  # expecting more than 0 columns
})

test_that("2011: teach_dat was successfully downloaded", {
  fdz_timss <- download_timss(year = "2011", data_type = "teach_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_timss, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_timss$dat), 0)  # expecting more than 0 columns
})

test_that("2011: school_dat was successfully downloaded", {
  fdz_timss <- download_timss(year = "2011", data_type = "school_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_timss, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_timss$dat), 0)  # expecting more than 0 columns
})

test_that("2015: stud_par_dat was successfully downloaded", {
  fdz_timss <- download_timss(year = "2015", data_type = "stud_par_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_timss, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_timss$dat), 0)  # expecting more than 0 columns
})

test_that("2015: teach_dat was successfully downloaded", {
  fdz_timss <- download_timss(year = "2015", data_type = "teach_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_timss, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_timss$dat), 0)  # expecting more than 0 columns
})

test_that("2015: school_dat was successfully downloaded", {
  fdz_timss <- download_timss(year = "2015", data_type = "school_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_timss, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_timss$dat), 0)  # expecting more than 0 columns
})

test_that("2019: stud_par_dat was successfully downloaded", {
  fdz_timss <- download_timss(year = "2019", data_type = "stud_par_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_timss, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_timss$dat), 0)  # expecting more than 0 columns
})

test_that("2019: teach_dat was successfully downloaded", {
  fdz_timss <- download_timss(year = "2019", data_type = "teach_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_timss, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_timss$dat), 0)  # expecting more than 0 columns
})

test_that("2019: teach_stud_dat was successfully downloaded", {
  fdz_timss <- download_timss(year = "2019", data_type = "teach_stud_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_timss, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_timss$dat), 0)  # expecting more than 0 columns
})

test_that("2019: school_dat was successfully downloaded", {
  fdz_timss <- download_timss(year = "2019", data_type = "school_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_timss, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_timss$dat), 0)  # expecting more than 0 columns
})

test_that("2019: tracking was successfully downloaded", {
  fdz_timss <- download_timss(year = "2019", data_type = "tracking")
  # testing if result is a data frame
  expect_s3_class(fdz_timss, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_timss$dat), 0)  # expecting more than 0 columns
})
