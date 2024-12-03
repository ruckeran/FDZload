test_that("2000: stud_dat_9kl was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2000", data_type = "stud_dat_9kl")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2000: stud_dat_15j was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2000", data_type = "stud_dat_15j")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2000: school_dat was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2000", data_type = "school_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2003: stud_dat_9kl was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2003", data_type = "stud_dat_9kl")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2003: stud_dat_15j was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2003", data_type = "stud_dat_15j")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2003: teach_dat_9kl was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2003", data_type = "teach_dat_9kl")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2003: teach_dat_15j was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2003", data_type = "teach_dat_15j")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2003: school_dat_9kl was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2003", data_type = "school_dat_9kl")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2003: school_dat_15j was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2003", data_type = "school_dat_15j")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2006: stud_dat_9kl was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2006", data_type = "stud_dat_9kl")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2006: stud_dat_15j was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2006", data_type = "stud_dat_15j")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2006: teach_dat was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2006", data_type = "teach_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2006: school_dat was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2006", data_type = "school_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2006: par_dat_9kl was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2006", data_type = "par_dat_9kl")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2006: par_dat_15j was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2006", data_type = "par_dat_15j")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2009: stud_par_dat was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2009", data_type = "stud_par_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2009: teach_dat was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2009", data_type = "teach_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2009: school_dat was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2009", data_type = "school_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2012: stud_dat_9kl was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2012", data_type = "stud_dat_9kl")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2012: stud_dat_15j was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2012", data_type = "stud_dat_15j")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2012: teach_dat was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2012", data_type = "teach_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2012: school_dat was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2012", data_type = "school_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2012: par_dat was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2012", data_type = "par_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2012: matching was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2012", data_type = "matching")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2015: stud_par_dat_9kl was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2015", data_type = "stud_par_dat_9kl")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2015: stud_par_dat_15j was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2015", data_type = "stud_par_dat_15j")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2015: teach_dat was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2015", data_type = "teach_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2015: school_dat was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2015", data_type = "school_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2015: timing was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2015", data_type = "timing")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2018: stud_par_dat_9kl was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2015", data_type = "stud_par_dat_9kl")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2018: stud_par_dat_15j was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2015", data_type = "stud_par_dat_15j")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2018: teach_dat was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2015", data_type = "teach_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2018: school_dat was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2015", data_type = "school_dat")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})

test_that("2018: timing was successfully downloaded", {
  fdz_pisa <- download_pisa(year = "2015", data_type = "timing")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})
