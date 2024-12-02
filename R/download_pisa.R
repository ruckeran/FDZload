#' Download and import an empty FDZ file.
#'
#' Download and import an empty data set from the \href{https://www.iqb.hu-berlin.de/fdz/studies/}{FDZ homepage}.
#'
#' The function downloads and imports an empty data set (\code{Leerdatensatz}) from the FDZ homepage.
#' These data sets contain zero rows.
#' The data is imported via \code{\link[eatGADS]{import_spss}} as a \code{GADSdat} object.
#'
#'@param year Year of the assessment.
#'@param data_type Type of the data.
#'
#'@examples
#' fdz_pisa <- download_pisa(year = "2018",
#'                         data_type = "stud_par_dat_9kl")
#'@export
download_pisa <- function(year = c("2018", "2015", "2012", "2009", "2006", "2003", "2000"),
                         data_type = c("stud_dat_9kl", "stud_dat_15j", "stud_par_dat",
                                       "stud_par_dat_9kl", "stud_par_dat_15j",
                                       "par_dat", "par_dat_9kl", "par_dat_15j",
                                       "teach_dat", "teach_dat_9kl", "teach_dat_15j",
                                       "school_dat", "school_dat_9kl", "school_dat_15j",
                                       "matching", "timing")) {
  ## input validation
  year <- match.arg(year)
  data_type <- match.arg(data_type)

  # URL table for each study, year and data type
  download_paths <- list(
      "2000" = list(
        stud_dat_9kl = "https://www.iqb.hu-berlin.de/fdz/studies/PISA-2000/PISA2000_9kl_SC.sav",
        stud_dat_15j = "https://www.iqb.hu-berlin.de/fdz/studies/PISA-2000/PISA2000_15J_SC.sav",
        school_dat = "https://www.iqb.hu-berlin.de/fdz/studies/PISA-2000/PISA2000_Schule.sav"
      ),
      "2003" = list(
        stud_dat_9kl = "https://www.iqb.hu-berlin.de/fdz/studies/PISA-2003/PISA2003I_SC_9K.sav",
        stud_dat_15j = "https://www.iqb.hu-berlin.de/fdz/studies/PISA-2003/PISA2003I_SC_15.sav",
        teach_dat_9kl = "https://www.iqb.hu-berlin.de/fdz/studies/PISA-2003/PISA2003I_LE_9K.sav",
        teach_dat_15j = "https://www.iqb.hu-berlin.de/fdz/studies/PISA-2003/PISA2003I_LE_15.sav",
        school_dat_9kl = "https://www.iqb.hu-berlin.de/fdz/studies/PISA-2003/PISA2003I_SN_9K.sav",
        school_dat_15j = "https://www.iqb.hu-berlin.de/fdz/studies/PISA-2003/PISA2003I_SN_15.sav"
      ),
      "2006" = list(
        stud_dat_9kl = "https://www.iqb.hu-berlin.de/fdz/studies/PISA-2006/PISA2006I_SC_9K.sav",
        stud_dat_15j = "https://www.iqb.hu-berlin.de/fdz/studies/PISA-2006/PISA2006I_SC_15.sav",
        teach_dat = "https://www.iqb.hu-berlin.de/fdz/studies/PISA-2006/PISA2006I_LE_Le.sav",
        school_dat = "https://www.iqb.hu-berlin.de/fdz/studies/PISA-2006/PISA2006I_Schull.sav",
        par_dat_9kl = "https://www.iqb.hu-berlin.de/fdz/studies/PISA-2006/PISA2006I_EL_9k.sav",
        par_dat_15j = "https://www.iqb.hu-berlin.de/fdz/studies/PISA-2006/PISA2006I_EL_15.sav"
      ),
      "2009" = list(
        stud_par_dat = "https://www.iqb.hu-berlin.de/fdz/studies/PISA-2009/PISA09_Leer_SEFB.sav",
        teach_dat = "https://www.iqb.hu-berlin.de/fdz/studies/PISA-2009/PISA09_Leer_LFB.sav",
        school_dat = "https://www.iqb.hu-berlin.de/fdz/studies/PISA-2009/PISA09_Leer_SLFB.sav"
      ),
      "2012" = list(
        stud_dat_9kl = "https://www.iqb.hu-berlin.de/fdz/studies/PISA_2012/PISA2012_Schuele.sav",
        stud_dat_15j = "https://www.iqb.hu-berlin.de/fdz/studies/PISA_2012/PISA2012_Schuele_1.sav",
        teach_dat = "https://www.iqb.hu-berlin.de/fdz/studies/PISA_2012/PISA2012_LehrerI.sav",
        school_dat = "https://www.iqb.hu-berlin.de/fdz/studies/PISA_2012/PISA2012_Schulle.sav",
        par_dat = "https://www.iqb.hu-berlin.de/fdz/studies/PISA_2012/PISA2012_Eltern_.sav",
        matching = "https://www.iqb.hu-berlin.de/fdz/studies/PISA_2012/PISA2012_Matchin.sav"
      ),
      "2015" = list(
        stud_par_dat_9kl = "https://www.iqb.hu-berlin.de/fdz/studies/PISA_2015/PISA2015_Schuele.sav",
        stud_par_dat_15j = "https://www.iqb.hu-berlin.de/fdz/studies/PISA_2015/PISA2015_Schuele_1.sav",
        teach_dat = "https://www.iqb.hu-berlin.de/fdz/studies/PISA_2015/PISA2015_LehrerI.sav",
        school_dat = "https://www.iqb.hu-berlin.de/fdz/studies/PISA_2015/PISA2015_Schulle.sav",
        timing = "https://www.iqb.hu-berlin.de/fdz/studies/PISA_2015/PISA2015_Schuele_2.sav"
      ),
      "2018" = list(
        stud_par_dat_9kl = "https://www.iqb.hu-berlin.de/fdz/studies/PISA_2018/PISA2018_Datensa_1.sav",
        stud_par_dat_15j = "https://www.iqb.hu-berlin.de/fdz/studies/PISA_2018/PISA2018_Datensa_2.sav",
        teach_dat = "https://www.iqb.hu-berlin.de/fdz/studies/PISA_2018/PISA2018_Datensa.sav",
        school_dat = "https://www.iqb.hu-berlin.de/fdz/studies/PISA_2018/PISA2018_Datensa_4.sav",
        timing = "https://www.iqb.hu-berlin.de/fdz/studies/PISA_2018/PISA2018_Datensa_3.sav"
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

