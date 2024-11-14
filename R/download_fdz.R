#' Download and import an empty FDZ file.
#'
#' Download and import an empty data set from the \href{https://www.iqb.hu-berlin.de/fdz/studies/}{FDZ homepage}.
#'
#' The function downloads and imports an empty data set (\code{Leerdatensatz}) from the FDZ homepage.
#' These data sets contain zero rows.
#' The data is imported via \code{\link[eatGADS]{import_spss}} as a \code{GADSdat} object.
#'
#'@param study Name of the study
#'@param year Year of the assessment (only needed for longitudinal assessments).
#'@param data_type Type of the data.
#'
#'@examples
#' fdz_pisa <- download_fdz(study = "TIMSS", year = "2019",
#'                         data_type = "stud_par_dat")
#'@export
download_fdz <- function(study = c("PISA", "TIMSS", "IGLU", "ICILS", "TVD"),
                         year = c("2019", "2018", "2016", "2015", "2013", "2012", "2011", "2009", "2007", "2006",
                                  "2003", "2001", "2000", NULL),
                         data_type = c("stud_dat", "stud_dat_9kl", "stud_dat_15j", "stud_par_dat",
                                       "stud_par_dat_9kl", "stud_par_dat_15j", "stud_nat_dat", "stud_int_dat",
                                       "teach_dat", "teach_stud_dat", "teach_dat_9kl", "teach_dat_15j",
                                       "teach_ger_dat", "teach_math_dat", "teach_gen_dat", "teach_nat_dat",
                                       "teach_int_dat", "teach_log_dat",
                                       "school_dat", "school_dat_9kl", "school_dat_15j", "school_nat_dat",
                                       "school_int_dat",
                                       "par_dat", "par_dat_9kl", "par_dat_15j",
                                       "it_dat", "matching", "testscores", "timing", "tracking",
                                       "video_dat", "video_timss_dat", "video_subj_dat", "video_teach_dat",
                                       "video_third_dat", "artefact")) {
  ## input validation
  study <- match.arg(study)
  year <- match.arg(year)
  data_type <- match.arg(data_type)

  # URL table for each study, year and data type
  download_paths <- list(
    "PISA" = list(
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
    ),
    "TIMSS" = list(
      "2007" = list(
        stud_par_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TIMSS2007/TIMSS2007_Leer_S.sav",
        teach_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TIMSS2007/TIMSS2007_Leer_L.sav",
        school_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TIMSS2007/TIMSS2007_Leer_S_1.sav"
      ),
      "2011" = list(
        stud_par_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TIMSS_2011/TIMSS2011_SEFB_l.sav",
        teach_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TIMSS_2011/TIMSS2011_LSFB_l.sav",
        school_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TIMSS_2011/TIMSS2011_SLFB_l.sav"
      ),
      "2015" = list(
        stud_par_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TIMSS_2015_v2/TIMSS2015_SEFB_v.sav",
        teach_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TIMSS_2015_v2/TIMSS2015_LSFB_L.sav",
        school_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TIMSS_2015_v2/TIMSS2015_SLFB_L.sav"
      ),
      "2019" = list(
        stud_par_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TIMSS_2019/TIMSS2019_HS_SEF.sav",
        teach_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TIMSS_2019/TIMSS2019_HS_LFB.sav",
        teach_stud_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TIMSS_2019/TIMSS2019_HS_LSF.sav",
        school_dat = "https://www.iqb.hu-berlin.de/fdz/studies/TIMSS_2019/TIMSS2019_HS_SLF.sav",
        tracking = "https://www.iqb.hu-berlin.de/fdz/studies/TIMSS_2019/TIMSS2019_HS_Tra.sav"
      )
    ),
    "IGLU" = list(
      "2001" = list(
        stud_par_dat = "https://www.iqb.hu-berlin.de/fdz/studies/IGLU_2001/IGLU2001_SC_Lee.sav",
        teach_ger_dat = "https://www.iqb.hu-berlin.de/fdz/studies/IGLU_2001/IGLU2001_DE_Lee.sav",
        teach_math_dat = "https://www.iqb.hu-berlin.de/fdz/studies/IGLU_2001/IGLU2001_MA_Lee.sav",
        teach_gen_dat = "https://www.iqb.hu-berlin.de/fdz/studies/IGLU_2001/IGLU2001_SU_Lee.sav",
        school_dat = "https://www.iqb.hu-berlin.de/fdz/studies/IGLU_2001/IGLU2001_SL_Lee.sav"
      ),
      "2006" = list(
        stud_par_dat = "https://www.iqb.hu-berlin.de/fdz/studies/IGLU-2006/IGLU2006_SC_EL_.sav",
        teach_dat = "https://www.iqb.hu-berlin.de/fdz/studies/IGLU-2006/IGLU2006_DE_Lee.sav",
        school_dat = "https://www.iqb.hu-berlin.de/fdz/studies/IGLU-2006/IGLU2006_SL_Lee.sav",
        testscores = "https://www.iqb.hu-berlin.de/fdz/studies/IGLU-2006/IGLU2006_TS_Lee.sav"
      ),
      "2011" = list(
        stud_par_dat = "https://www.iqb.hu-berlin.de/fdz/studies/IGLU_2011/IGLU2011_SEFB_le.sav",
        teach_dat = "https://www.iqb.hu-berlin.de/fdz/studies/IGLU_2011/IGLU2011_LSFB_le.sav",
        school_dat = "https://www.iqb.hu-berlin.de/fdz/studies/IGLU_2011/IGLU2011_SLFB_le.sav"
      ),
      "2016" = list(
        stud_par_dat = "https://www.iqb.hu-berlin.de/fdz/studies/IGLU_2016/IGLU2016_SEFB_v1.sav",
        teach_dat = "https://www.iqb.hu-berlin.de/fdz/studies/IGLU_2016/IGLU2016_LFB_v1_.sav",
        teach_stud_dat = "https://www.iqb.hu-berlin.de/fdz/studies/IGLU_2016/IGLU2016_LSFB_v1.sav",
        school_dat = "https://www.iqb.hu-berlin.de/fdz/studies/IGLU_2016/IGLU2016_SLFB_v1.sav",
        tracking = "https://www.iqb.hu-berlin.de/fdz/studies/IGLU_2016/IGLU2016_TR_v1_L.sav"
      )
    ),
    "ICILS" = list(
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
    ),
    "TVD" = list(
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
    )

  # call up URL for specific combination
  if (study %in% names(download_paths) &&
      year %in% names(download_paths[[study]]) &&
      data_type %in% names(download_paths[[study]][[year]])) {

    download_path <- download_paths[[study]][[year]][[data_type]]
  } else {
    stop("The corresponding download has not been implemented yet.")
  }

  ### read data
  eatGADS::import_spss(download_path, checkVarNames = FALSE)
  #haven::read_sav(download_path, n_max = 1, user_na = TRUE)
}
