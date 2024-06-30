#' Link sample data to corresponding controls
#'
#'
#'
#' @param report_full reports linked with relevant control IDs
#' @return A report with control read counts linke dto their relevant sample
#' @export

extractThreshold <- function(thresholds, threshold_name) {

  row <- thresholds %>%
    dplyr::filter(threshold == threshold_name)

  value_raw <- row$value

  if (value_raw == "TRUE") {

    value <- TRUE

  } else if (value_raw == "FALSE") {

    value <- FALSE

  } else {

    value <- as.numeric(value_raw)

  }

  return(value)

}
