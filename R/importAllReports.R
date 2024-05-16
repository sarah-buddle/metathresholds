#' Import all reports
#'
#'
#'
#' @param samplesheet samplesheet
#' @return A report for all the samples in the samplesheet
#' @export

importAllReports <- function(samplesheet) {

  reports <- data.frame()

  for (i in 1:nrow(samplesheet)) {

    sample_id <- samplesheet$sample_id[i]
    tool <- samplesheet$tool[i]
    filepath <- samplesheet$filepath[i]

    report <- importReport(sample_id, tool, filepath)

    reports <- rbind(reports, report)

  }

  return(reports)

}
