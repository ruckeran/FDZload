test_that("2001: stud_par_dat was successfully downloaded", {
  fdz_iglu <- download_iglu(year = "2001", data_type = "stud_par_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_iglu, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_iglu$dat), 0)  # expecting more than 0 columns
})

test_that("2001: teach_ger_dat was successfully downloaded", {
  fdz_iglu <- download_iglu(year = "2001", data_type = "teach_ger_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_iglu, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_iglu$dat), 0)  # expecting more than 0 columns
})

test_that("2001: teach_math_dat was successfully downloaded", {
  fdz_iglu <- download_iglu(year = "2001", data_type = "teach_math_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_iglu, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_iglu$dat), 0)  # expecting more than 0 columns
})

test_that("2001: teach_gen_dat was successfully downloaded", {
  fdz_iglu <- download_iglu(year = "2001", data_type = "teach_gen_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_iglu, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_iglu$dat), 0)  # expecting more than 0 columns
})

test_that("2001: school_dat was successfully downloaded", {
  fdz_iglu <- download_iglu(year = "2001", data_type = "school_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_iglu, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_iglu$dat), 0)  # expecting more than 0 columns
})

test_that("2006: stud_par_dat was successfully downloaded", {
  fdz_iglu <- download_iglu(year = "2006", data_type = "stud_par_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_iglu, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_iglu$dat), 0)  # expecting more than 0 columns
})

test_that("2006: teach_dat was successfully downloaded", {
  fdz_iglu <- download_iglu(year = "2006", data_type = "teach_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_iglu, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_iglu$dat), 0)  # expecting more than 0 columns
})

test_that("2006: school_dat was successfully downloaded", {
  fdz_iglu <- download_iglu(year = "2006", data_type = "school_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_iglu, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_iglu$dat), 0)  # expecting more than 0 columns
})

test_that("2006: testscores was successfully downloaded", {
  fdz_iglu <- download_iglu(year = "2006", data_type = "testscores")
  # testing if result is a data frame
  expect_s3_class(fdz_iglu, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_iglu$dat), 0)  # expecting more than 0 columns
})

test_that("2011: stud_par_dat was successfully downloaded", {
  fdz_iglu <- download_iglu(year = "2011", data_type = "stud_par_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_iglu, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_iglu$dat), 0)  # expecting more than 0 columns
})

test_that("2011: teach_dat was successfully downloaded", {
  fdz_iglu <- download_iglu(year = "2011", data_type = "teach_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_iglu, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_iglu$dat), 0)  # expecting more than 0 columns
})

test_that("2011: school_dat was successfully downloaded", {
  fdz_iglu <- download_iglu(year = "2011", data_type = "school_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_iglu, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_iglu$dat), 0)  # expecting more than 0 columns
})

test_that("2016: stud_par_dat was successfully downloaded", {
  fdz_iglu <- download_iglu(year = "2016", data_type = "stud_par_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_iglu, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_iglu$dat), 0)  # expecting more than 0 columns
})

test_that("2016: teach_dat was successfully downloaded", {
  fdz_iglu <- download_iglu(year = "2016", data_type = "teach_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_iglu, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_iglu$dat), 0)  # expecting more than 0 columns
})

test_that("2016: teach_stud_dat was successfully downloaded", {
  fdz_iglu <- download_iglu(year = "2016", data_type = "teach_stud_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_iglu, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_iglu$dat), 0)  # expecting more than 0 columns
})

test_that("2016: school_dat was successfully downloaded", {
  fdz_iglu <- download_iglu(year = "2016", data_type = "school_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_iglu, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_iglu$dat), 0)  # expecting more than 0 columns
})

test_that("2016: tracking was successfully downloaded", {
  fdz_iglu <- download_iglu(year = "2016", data_type = "tracking")
  # testing if result is a data frame
  expect_s3_class(fdz_iglu, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_iglu$dat), 0)  # expecting more than 0 columns
})
