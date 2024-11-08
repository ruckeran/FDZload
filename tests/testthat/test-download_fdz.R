test_that("data set was successfully downloaded", {
  fdz_pisa <- download_fdz(study = "PISA", year = "2015", data_type = "stud_quest")
  # testing if result is a data frame
  expect_s3_class(fdz_pisa, "GADSdat")
  # testing whether the data set is not empty
  expect_gt(ncol(fdz_pisa$dat), 0)  # expecting more than 0 columns
})
