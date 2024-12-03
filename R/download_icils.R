#' Download and import an empty FDZ file.
#'
#' Download and import an empty ICILS data set from the \href{https://www.iqb.hu-berlin.de/fdz/studies/}{FDZ homepage}.
#'
#' The function downloads and imports an empty ICILS data set (\code{Leerdatensatz}) from the FDZ homepage.
#' These data sets contain zero rows.
#' The data is imported via \code{\link[eatGADS]{import_spss}} as a \code{GADSdat} object.
#'
#'@param year Year of the assessment.
#'@param data_type Type of the data.
#'
#'@examples
#' fdz_icils <- download_icils(year = "2018",
#'                         data_type = "stud_dat")
#'@export
download_icils <- function(year = c("2018", "2013"),
                          data_type = c("stud_dat", "stud_nat_dat", "stud_int_dat",
                                        "teach_dat", "teach_nat_dat", "teach_int_dat",
                                        "school_dat", "school_nat_dat", "school_int_dat",
                                        "it_dat")) {
  ## input validation
  year <- match.arg(year)
  data_type <- match.arg(data_type)

  # URL table for each study, year and data type
  download_paths <- list(
    "2013" = list(
      stud_nat_dat = "https://www.iqb.hu-berlin.de/fdz/studies/ICILS_2013/ICILS2013_Schuel_1.sav",
      stud_int_dat = "https://www.iqb.hu-berlin.de/fdz/studies/ICILS_2013/ICILS2013_Schuel.sav",
      teach_nat_dat = "https://www.iqb.hu-berlin.de/fdz/studies/ICILS_2013/ICILS2013_Lehrer_1.sav",
      teach_int_dat = "https://www.iqb.hu-berlin.de/fdz/studies/ICILS_2013/ICILS2013_Lehrer.sav",
      school_nat_dat = "https://www.iqb.hu-berlin.de/fdz/studies/ICILS_2013/ICILS2013_Schull_1.sav",
      school_int_dat = "https://www.iqb.hu-berlin.de/fdz/studies/ICILS_2013/ICILS2013_Schull.sav"
    ),
    "2018" = list(
      stud_dat = "https://www.iqb.hu-berlin.de/fdz/studies/ICILS_2018/ICILS_2018_Schue.sav",
      teach_dat = "https://www.iqb.hu-berlin.de/fdz/studies/ICILS_2018/ICILS_2018_Lehrk.sav",
      school_dat = "https://www.iqb.hu-berlin.de/fdz/studies/ICILS_2018/ICILS_2018_Schul.sav",
      it_dat = "https://www.iqb.hu-berlin.de/fdz/studies/ICILS_2018/ICILS_2018_ITFra.sav"
    )
  )

  # call up URL for specific combination
  if (year %in% names(download_paths) &&
      data_type %in% names(download_paths[[year]])) {

    download_path <- download_paths[[year]][[data_type]]
  } else {
    stop("The corresponding download has not been implemented yet.")
  }

  ### read data
  eatGADS::import_spss(download_path, checkVarNames = FALSE)
  #haven::read_sav(download_path, n_max = 1, user_na = TRUE)
}

