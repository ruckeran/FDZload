#' Download and import an empty FDZ file.
#'
#' Download and import an empty TVD data set from the \href{https://www.iqb.hu-berlin.de/fdz/studies/}{FDZ homepage}.
#'
#' The function downloads and imports an empty TVD data set (\code{Leerdatensatz}) from the FDZ homepage.
#' These data sets contain zero rows.
#' The data is imported via \code{\link[eatGADS]{import_spss}} as a \code{GADSdat} object.
#'
#'@param data_type Type of the data.
#'
#'@examples
#' fdz_tvd <- download_tvd(data_type = "stud_dat")
#'@export
download_tvd <- function(data_type = c("stud_dat", "teach_dat", "teach_log_dat",
                                       "video_timss_dat", "video_dat", "video_subj_dat",
                                       "video_teach_dat", "video_third_dat", "artefact")) {

  ## input validation
  data_type <- match.arg(data_type)

  # URL table for each study, year and data type
  download_paths <- list(
    stud_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TVD/TVD_student_data.sav",
    teach_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TVD/TVD_Teacher_v1_L.sav",
    teach_log_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TVD/TVD_Teacher_Log_.sav",
    video_timss_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TVD/TVD_TIMSS_Video_.sav",
    video_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TVD/TVD_Video_v1_Lee.sav",
    video_subj_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TVD/TVD_Video_Aggreg.sav",
    video_teach_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TVD/TVD_Video_Aggreg_1.sav",
    video_third_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TVD/TVD_Video_Third_.sav",
    artefact = "https://www.iqb.hu-berlin.de/fdz/studies/TVD/TVD_Artefact_v1_.sav"
  )


  # call up URL for specific combination
  if (data_type %in% names(download_paths)) {

    download_path <- download_paths[[data_type]]
  } else {
    stop("The corresponding download has not been implemented yet.")
  }

  ### read data
  eatGADS::import_spss(download_path, checkVarNames = FALSE)
  #haven::read_sav(download_path, n_max = 1, user_na = TRUE)
}

