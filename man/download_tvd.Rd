% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_tvd.R
\name{download_tvd}
\alias{download_tvd}
\title{Download and import an empty FDZ file.}
\usage{
download_tvd(
  data_type = c("stud_dat", "teach_dat", "teach_log_dat", "video_timss_dat", "video_dat",
    "video_subj_dat", "video_teach_dat", "video_third_dat", "artefact")
)
}
\arguments{
\item{data_type}{Type of the data.}
}
\description{
Download and import an empty TVD data set from the \href{https://www.iqb.hu-berlin.de/fdz/studies/}{FDZ homepage}.
}
\details{
The function downloads and imports an empty TVD data set (\code{Leerdatensatz}) from the FDZ homepage.
These data sets contain zero rows.
The data is imported via \code{\link[eatGADS]{import_spss}} as a \code{GADSdat} object.
}
\examples{
fdz_tvd <- download_tvd(data_type = "stud_dat")
}
